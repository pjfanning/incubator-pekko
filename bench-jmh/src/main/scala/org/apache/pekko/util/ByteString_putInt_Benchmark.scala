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

import java.util.concurrent.TimeUnit

import org.openjdk.jmh.annotations.{ Benchmark, Measurement, Scope, State }

@State(Scope.Benchmark)
@Measurement(timeUnit = TimeUnit.MILLISECONDS)
class ByteString_putInt_Benchmark {

  @Benchmark
  def putLong(): ByteStringBuilder = {
    val builder = ByteString.newBuilder
    for (_ <- 1 to 100)
      builder.putLong(0x0102030405060708L)(java.nio.ByteOrder.BIG_ENDIAN)
    builder
  }

}
