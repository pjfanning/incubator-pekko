/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, which was derived from Akka.
 */

/*
 * Copyright (C) 2014-2022 Lightbend Inc. <https://www.lightbend.com>
 */

package org.apache.pekko.util

import java.nio.charset.StandardCharsets
import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations._

@State(Scope.Benchmark)
@Measurement(timeUnit = TimeUnit.MILLISECONDS)
class ByteString_startEnd_Benchmark {
  val start = ByteString("abcdefg") ++ ByteString("hijklmno") ++ ByteString("pqrstuv")
  val bss = start ++ start ++ start ++ start ++ start ++ ByteString("xyz")

  val bs = bss.compact // compacted
  val start = "abcdefghijk"
  val startBytes = start.getBytes(StandardCharsets.UTF_8)
  val end = "pqrstuvxyz"
  val endBytes = end.getBytes(StandardCharsets.UTF_8)

  @Benchmark
  def bss_startsWith: Boolean = bss.startsWith(start)

  @Benchmark
  def bss_endsWith: Boolean = bss.endsWith(end)

  @Benchmark
  def bs_startsWith: Boolean = bs.startsWith(start)

  @Benchmark
  def bs_endsWith: Boolean = bs.endsWith(end)

  @Benchmark
  def bs_startsWithBytes: Boolean = bs.startsWith(startBytes)

  @Benchmark
  def bs_endsWithBytes: Boolean = bs.endsWith(endBytes)
}
