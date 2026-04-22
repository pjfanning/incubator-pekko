/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * license agreements; and to You under the Apache License, version 2.0:
 *
 *   https://www.apache.org/licenses/LICENSE-2.0
 *
 * This file is part of the Apache Pekko project, which was derived from Akka.
 */

package org.apache.pekko

package object util {
  type ByteString = org.apache.pekko.bytestring.ByteString
  val ByteString = org.apache.pekko.bytestring.ByteString

  type CompactByteString = org.apache.pekko.bytestring.CompactByteString
  val CompactByteString = org.apache.pekko.bytestring.CompactByteString

  type ByteStringBuilder = org.apache.pekko.bytestring.ByteStringBuilder

  type ByteIterator = org.apache.pekko.bytestring.ByteIterator
  val ByteIterator = org.apache.pekko.bytestring.ByteIterator
}
