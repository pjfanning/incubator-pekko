/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements. See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.apache.pekko.stream.io

import java.io.File
import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets
import java.nio.file.Files
import java.security.Security
import java.util.concurrent.TimeUnit
import javax.net.ssl.{ SSLEngine, SSLEngineResult }
import javax.net.ssl.SSLEngineResult.HandshakeStatus.{ FINISHED, NEED_TASK, NEED_WRAP, NOT_HANDSHAKING }

import scala.concurrent.Await
import scala.concurrent.duration._
import scala.util.Success

import org.apache.pekko
import pekko.actor.ActorSystem
import pekko.stream.{ ClosedShape, IgnoreComplete }
import pekko.stream.TLSProtocol._
import pekko.stream.impl.io.TlsGraphStage
import pekko.stream.scaladsl.{ BidiFlow, GraphDSL, RunnableGraph }
import pekko.stream.scaladsl.GraphDSL.Implicits._
import pekko.stream.testkit.{ TestPublisher, TestSubscriber }
import pekko.stream.testkit.scaladsl.{ TestSink, TestSource }
import pekko.util.ByteString

import com.typesafe.config.ConfigFactory
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * Drives a real JDK TLS 1.3 KeyUpdate through the graph-stage engine. The KeyUpdate is forced with a
 * low `jdk.tls.keyLimits`, a JVM-wide security property, so the scenario runs in a separate JVM.
 */
class TlsGraphStageKeyUpdateSpec extends AnyWordSpec with Matchers {

  "TlsGraphStage with a JDK SSLEngine" should {
    "keep draining accepted plaintext after a TLS 1.3 KeyUpdate received before output cancellation" in {
      val javaBin = new File(new File(System.getProperty("java.home"), "bin"), "java").getPath
      // A file rather than a pipe, so a stuck child cannot block the test before the timeout.
      val log = File.createTempFile("TlsGraphStageKeyUpdate", ".log")
      try {
        val process = new ProcessBuilder(javaBin, TlsGraphStageKeyUpdateApp.getClass.getName.stripSuffix("$"))
          .redirectErrorStream(true)
          .redirectOutput(log)
        // stream-tests run in a forked JVM, so java.class.path is the test classpath
        process.environment().put("CLASSPATH", System.getProperty("java.class.path"))
        val started = process.start()
        val exited = started.waitFor(60, TimeUnit.SECONDS)
        if (!exited) started.destroyForcibly().waitFor()
        val output = new String(Files.readAllBytes(log.toPath), StandardCharsets.UTF_8)
        withClue(output) {
          exited shouldBe true
          started.exitValue() shouldBe 0
          output should include("peerKeyUpdate=true")
          output should include(s"peerReceived=[${TlsGraphStageKeyUpdateApp.Payload}]")
        }
      } finally log.delete()
    }
  }
}

/** Run in a separate JVM by [[TlsGraphStageKeyUpdateSpec]]. */
object TlsGraphStageKeyUpdateApp {
  final val Payload = "accepted-before-cancel"

  def main(args: Array[String]): Unit = {
    // Must be set before any TLS cipher is initialized in this JVM.
    Security.setProperty("jdk.tls.keyLimits", "AES/GCM/NoPadding KeyUpdate 2^12, ChaCha20-Poly1305 KeyUpdate 2^12")
    val system = ActorSystem("TlsGraphStageKeyUpdate", ConfigFactory.parseString("pekko.loglevel = WARNING"))
    try {
      run()(system)
      System.exit(0)
    } catch {
      case t: Throwable =>
        t.printStackTrace()
        System.exit(1)
    } finally {
      system.terminate()
      Await.ready(system.whenTerminated, 10.seconds)
    }
  }

  private def run()(implicit system: ActorSystem): Unit = {
    val context = TlsSpec.initSslContext("TLSv1.3")
    val serverEngine = context.createSSLEngine()
    serverEngine.setUseClientMode(false)
    serverEngine.setEnabledProtocols(Array("TLSv1.3"))
    val peer = new PeerEngine(context.createSSLEngine())

    val (plainIn, cipherIn, cipherOut, plainOut) =
      RunnableGraph
        .fromGraph(
          GraphDSL.createGraph(
            TestSource[SslTlsOutbound](),
            TestSource[ByteString](),
            TestSink[ByteString](),
            TestSink[SslTlsInbound]())(Tuple4.apply) { implicit b => (plainIn, cipherIn, cipherOut, plainOut) =>
            val tls = b.add(
              BidiFlow
                .fromGraph(new TlsGraphStage(() => serverEngine, _ => Success(()), IgnoreComplete))
                .withAttributes(TlsGraphStage.StreamTlsAttributes))
            plainIn  ~> tls.in1
            tls.out1 ~> cipherOut
            cipherIn ~> tls.in2
            tls.out2 ~> plainOut
            ClosedShape
          })
        .run()
    val transport = new Transport(cipherIn, cipherOut)

    // Unwrapping needs plaintext demand, also during the handshake.
    plainOut.request(1000)
    peer.engine.beginHandshake()
    var rounds = 0
    var quiet = false
    while (!(quiet && peer.established) && rounds < 100) {
      if (peer.engine.getHandshakeStatus == NEED_WRAP) transport.send(peer.wrap(ByteBuffer.allocate(0)))
      quiet = transport.pull(300.millis) match {
        case Some(bytes) => peer.unwrap(bytes); false
        case None        => true
      }
      rounds += 1
    }
    require(peer.established, "handshake did not complete")

    // Use up the outstanding ciphertext demand so the stage cannot wrap until the test requests more.
    plainIn.sendNext(SendBytes(ByteString("ping")))
    while (peer.received.utf8String != "ping")
      peer.unwrap(transport.pull(3.seconds).getOrElse(throw new IllegalStateException("ping not received")))
    peer.received = ByteString.empty
    // A NewSessionTicket read after the handshake is not the KeyUpdate this test looks for.
    peer.sawPostHandshakeMessage = false

    // Accepted while the stage has no ciphertext demand, so it stays queued.
    plainIn.sendNext(SendBytes(ByteString(Payload)))
    // Past the key limit the stage needs to wrap a KeyUpdate, but has no demand to do so. It holds the
    // record's plaintext until then, so give it time to read; if cancellation wins, the KeyUpdate
    // arrives during the drain instead, which needs the same handling.
    transport.send(peer.wrap(ByteBuffer.wrap(new Array[Byte](16 * 1024))))
    plainOut.receiveWithin(1.second)

    plainOut.cancel()

    val deadline = 10.seconds.fromNow
    while (!peer.engine.isInboundDone && deadline.hasTimeLeft())
      transport.pull(deadline.timeLeft).foreach(peer.unwrap)
    println(s"peerKeyUpdate=${peer.sawPostHandshakeMessage}")
    println(s"peerReceived=[${peer.received.utf8String}]")
  }

  private final class Transport(in: TestPublisher.Probe[ByteString], out: TestSubscriber.Probe[ByteString]) {
    private var outstanding = 0

    def send(bytes: ByteString): Unit = if (bytes.nonEmpty) in.sendNext(bytes)

    // Keep at most one element of demand so the stage only wraps when the test pulls.
    def pull(within: FiniteDuration): Option[ByteString] = {
      if (outstanding == 0) {
        out.request(1)
        outstanding = 1
      }
      val next = out.receiveWithin(within, 1).headOption
      if (next.isDefined) outstanding = 0
      next
    }
  }

  /** The client side, driven directly so the test controls when its records reach the stage. */
  private final class PeerEngine(val engine: SSLEngine) {
    engine.setUseClientMode(true)
    engine.setEnabledProtocols(Array("TLSv1.3"))

    private var inbound = ByteBuffer.allocate(0)
    private var handshakeDone = false
    var received = ByteString.empty
    var sawPostHandshakeMessage = false

    def established: Boolean = handshakeDone && engine.getHandshakeStatus == NOT_HANDSHAKING

    def wrap(src: ByteBuffer): ByteString = {
      var out = ByteString.empty
      val dst = ByteBuffer.allocate(engine.getSession.getPacketBufferSize)
      var continue = true
      while (continue) {
        dst.clear()
        val result = engine.wrap(src, dst)
        track(result)
        dst.flip()
        out ++= ByteString(dst)
        continue = result.getStatus == SSLEngineResult.Status.OK &&
          (src.hasRemaining || engine.getHandshakeStatus == NEED_WRAP)
      }
      out
    }

    def unwrap(bytes: ByteString): Unit = {
      val merged = ByteBuffer.allocate(inbound.remaining() + bytes.size)
      merged.put(inbound).put(bytes.toByteBuffer).flip()
      inbound = merged
      val dst = ByteBuffer.allocate(engine.getSession.getApplicationBufferSize)
      var continue = true
      while (continue) {
        dst.clear()
        val result = engine.unwrap(inbound, dst)
        track(result)
        if (handshakeDone && result.getHandshakeStatus != NOT_HANDSHAKING) sawPostHandshakeMessage = true
        dst.flip()
        received ++= ByteString(dst)
        // A KeyUpdate request needs a response before the peer reads further; the stage is closing, so drop it.
        if (engine.getHandshakeStatus == NEED_WRAP && handshakeDone) wrap(ByteBuffer.allocate(0))
        continue = result.getStatus == SSLEngineResult.Status.OK && result.bytesConsumed() > 0
      }
    }

    private def track(result: SSLEngineResult): Unit = {
      if (result.getHandshakeStatus == NEED_TASK) {
        var task = engine.getDelegatedTask
        while (task ne null) {
          task.run()
          task = engine.getDelegatedTask
        }
      }
      if (result.getHandshakeStatus == FINISHED) handshakeDone = true
    }
  }
}
