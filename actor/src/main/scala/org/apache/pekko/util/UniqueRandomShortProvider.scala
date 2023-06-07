/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, derived from Akka.
 */

package org.apache.pekko.util

import org.apache.pekko.annotation.InternalApi

import java.security.SecureRandom

/**
 * An efficient collision free threadsafe generator of pseudo random
 * shorts based on https://en.wikipedia.org/wiki/Double_hashing#Enhanced_double_hashing
 * using SecureRandom as a seed.
 *
 * The avoidance of collisions is achieved by using a Short Array buffer
 * that is prefilled with a dimension of 65536 (~ 131072 bytes in size per
 * instance of [[UniqueRandomShortProvider]]). This means it will loop
 * through all possible 65536 values before starting to re-generating
 * the same numbers again.
 */

@InternalApi
private[pekko] class UniqueRandomShortProvider {
  import UniqueRandomShortProvider._
  val r = new SecureRandom()

  private var index = r.nextLong
  private var increment = r.nextLong
  private var count = 1L
  private var limit = 0
  private val buffer: Array[Short] = new Array[Short](FillerLimit)
  private val fillerOffset = 0
  resetBuffer()

  private def resetBuffer(): Unit = {
    System.arraycopy(Filler, fillerOffset, buffer, 0, buffer.length)
    limit = buffer.length
  }
  def nextId(): Short = {
    if (limit == 0)
      resetBuffer()

    val idx = UniqueRandomShortProvider.mod(index, limit)
    limit = limit - 1
    // Update index and handle wrapping
    index -= increment
    // Incorporate the counter into the increment to create a
    // tetrahedral number additional term, and handle wrapping.
    increment -= {
      count += 1
      count - 1
    }
    val result = buffer(idx)
    val src = idx + 1
    if (src < buffer.length) {
      val len = buffer.length - src
      System.arraycopy(buffer, src, buffer, idx, len)
    }
    result
  }
}

@InternalApi
private[pekko] object UniqueRandomShortProvider {
  private val FillerLimit = 0x10000

  // This is lazy in order to avoid allocation unless someone
  // actually calls `UniqueRandomShortProvider.nextId()` since
  // MaxLimit is actually of non negligible size.
  private lazy val Filler = {
    val a = new Array[Short](FillerLimit)
    var i = 1
    while (i < FillerLimit) {
      a(i) = i.toShort
      i += 1
    }
    a
  }

  /**
   * Performs a modulus calculation on an unsigned long and an integer divisor.
   *
   * @param dividend a unsigned long value to calculate the modulus of.
   * @param divisor the divisor for the modulus calculation.
   * @return the remainder or modulus value.
   */
  private def mod(dividend: Long, divisor: Int): Int = {
    // See Hacker's Delight (2nd ed), section 9.3.
    // Assume divisor is positive.
    // Divide half the unsigned number and then double the quotient result.
    val quotient = (dividend >>> 1) / divisor << 1
    val remainder = dividend - quotient * divisor
    // remainder in [0, 2 * divisor)
    (if (remainder >= divisor) remainder - divisor
     else remainder).toInt
  }
}
