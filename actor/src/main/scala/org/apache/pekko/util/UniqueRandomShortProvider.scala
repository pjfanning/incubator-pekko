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
import org.apache.pekko.io.dns.IdGenerator

import java.security.SecureRandom
import java.util.concurrent.{ Semaphore, TimeUnit }

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
  private val lockTimeoutSeconds = 10
  private val lock = new Semaphore(1)
  private val secureRandom = new SecureRandom()
  private var index = secureRandom.nextLong()
  private var increment = secureRandom.nextLong()
  private var count = 1L

  final def nextId(): Short = {
    if (lock.tryAcquire(lockTimeoutSeconds, TimeUnit.SECONDS)) {
      try {
        val result = (0xFFFFFFFF & index).asInstanceOf[Short]
        index -= increment

        // Incorporate the counter into the increment to create a
        // tetrahedral number additional term.
        increment -= {
          count += 1
          count - 1
        }

        result
      } finally {
        lock.release()
      }
    } else {
      // failover to using a secure random if we can't acquire the lock
      IdGenerator.random(secureRandom).nextId()
    }
  }
}
