/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, which was derived from Akka.
 */

package org.apache.pekko.util

import java.nio.ByteOrder

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

/**
 * Tests that the type aliases in org.apache.pekko.util work correctly for
 * backward source compatibility. ByteString and related types were moved to
 * org.apache.pekko.bytestring but are accessible via type aliases in
 * org.apache.pekko.util.
 */
class ByteStringTypeAliasSpec extends AnyWordSpec with Matchers {

  "org.apache.pekko.util.ByteString (type alias)" should {
    "be usable without explicit import within the util package" in {
      val bs: ByteString = ByteString("hello")
      bs.utf8String shouldEqual "hello"
    }

    "support basic operations" in {
      val bs = ByteString("hello world")
      bs.length shouldEqual 11
      bs.take(5).utf8String shouldEqual "hello"
      (ByteString("hello") ++ ByteString(" world")).utf8String shouldEqual "hello world"
    }

    "support companion object methods" in {
      ByteString.empty.isEmpty shouldBe true
      ByteString(1.toByte, 2.toByte, 3.toByte).length shouldEqual 3
    }
  }

  "org.apache.pekko.util.CompactByteString (type alias)" should {
    "be accessible" in {
      val cbs: CompactByteString = CompactByteString("compact")
      cbs.utf8String shouldEqual "compact"
      cbs.isCompact shouldBe true
    }
  }

  "org.apache.pekko.util.ByteStringBuilder (type alias)" should {
    "be usable" in {
      val builder: ByteStringBuilder = ByteString.newBuilder
      builder.putByte(65.toByte)
      builder.putByte(66.toByte)
      builder.result().utf8String shouldEqual "AB"
    }
  }

  "org.apache.pekko.util.ByteIterator (type alias)" should {
    "be usable" in {
      val it: ByteIterator = ByteString("ABC").iterator
      it.next() shouldEqual 'A'.toByte
      it.next() shouldEqual 'B'.toByte
      it.next() shouldEqual 'C'.toByte
      it.hasNext shouldBe false
    }

    "support companion object ByteArrayIterator" in {
      val it = ByteIterator.ByteArrayIterator(Array[Byte](1, 2, 3))
      it.toList shouldEqual List[Byte](1, 2, 3)
    }
  }
}
