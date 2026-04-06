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

package org.apache.pekko.util

import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.should.Matchers

class SWARUtilSpec extends AnyWordSpec with Matchers {

  val testData = Array[Byte](0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)

  "SWARUtil" must {
    "getLong" in {
      SWARUtil.getLong(testData, 0) should ===(0x0001020304050607L)
      SWARUtil.getLong(testData, 0, true) should ===(0x0001020304050607L)
      SWARUtil.getLong(testData, 0, false) should ===(0x0706050403020100L)
      SWARUtil.getLongBEWithoutMethodHandle(testData, 0) should ===(0x0001020304050607L)
      SWARUtil.getLongLEWithoutMethodHandle(testData, 0) should ===(0x0706050403020100L)
      SWARUtil.getLong(testData, 8) should ===(0x08090A0B0C0D0E0FL)
      SWARUtil.getLong(testData, 8, true) should ===(0x08090A0B0C0D0E0FL)
      SWARUtil.getLong(testData, 8, false) should ===(0x0F0E0D0C0B0A0908L)
      SWARUtil.getLongBEWithoutMethodHandle(testData, 8) should ===(0x08090A0B0C0D0E0FL)
      SWARUtil.getLongLEWithoutMethodHandle(testData, 8) should ===(0x0F0E0D0C0B0A0908L)
    }
    "getInt" in {
      SWARUtil.getInt(testData, 0) should ===(0x00010203)
      SWARUtil.getInt(testData, 0, true) should ===(0x00010203)
      SWARUtil.getInt(testData, 0, false) should ===(0x03020100)
      SWARUtil.getIntBEWithoutMethodHandle(testData, 0) should ===(0x00010203)
      SWARUtil.getIntLEWithoutMethodHandle(testData, 0) should ===(0x03020100)
      SWARUtil.getInt(testData, 4) should ===(0x04050607)
      SWARUtil.getInt(testData, 4, true) should ===(0x04050607)
      SWARUtil.getInt(testData, 4, false) should ===(0x07060504)
      SWARUtil.getIntBEWithoutMethodHandle(testData, 4) should ===(0x04050607)
      SWARUtil.getIntLEWithoutMethodHandle(testData, 4) should ===(0x07060504)
    }
    "getShort" in {
      SWARUtil.getShort(testData, 0) should ===(0x0001.toShort)
      SWARUtil.getShort(testData, 0, true) should ===(0x0001.toShort)
      SWARUtil.getShort(testData, 0, false) should ===(0x0100.toShort)
      SWARUtil.getShortBEWithoutMethodHandle(testData, 0) should ===(0x0001.toShort)
      SWARUtil.getShortLEWithoutMethodHandle(testData, 0) should ===(0x0100.toShort)
      SWARUtil.getShort(testData, 2) should ===(0x0203.toShort)
      SWARUtil.getShort(testData, 2, true) should ===(0x0203.toShort)
      SWARUtil.getShort(testData, 2, false) should ===(0x0302.toShort)
      SWARUtil.getShortBEWithoutMethodHandle(testData, 2) should ===(0x0203.toShort)
      SWARUtil.getShortLEWithoutMethodHandle(testData, 2) should ===(0x0302.toShort)
    }
    "applyMask with 8-byte aligned data" in {
      val data = Array[Byte](0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08)
      SWARUtil.applyMask(data, 0, data.length, 0x0A0B0C0D)
      // each byte XOR'd with the corresponding mask byte cycling m0=0x0A, m1=0x0B, m2=0x0C, m3=0x0D
      data should ===(Array[Byte](
        (0x01 ^ 0x0A).toByte, (0x02 ^ 0x0B).toByte, (0x03 ^ 0x0C).toByte, (0x04 ^ 0x0D).toByte,
        (0x05 ^ 0x0A).toByte, (0x06 ^ 0x0B).toByte, (0x07 ^ 0x0C).toByte, (0x08 ^ 0x0D).toByte))
    }
    "applyMask with non-aligned length" in {
      val data = Array[Byte](0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07)
      SWARUtil.applyMask(data, 0, data.length, 0x01020304)
      data should ===(Array[Byte](
        (0x01 ^ 0x01).toByte, (0x02 ^ 0x02).toByte, (0x03 ^ 0x03).toByte, (0x04 ^ 0x04).toByte,
        (0x05 ^ 0x01).toByte, (0x06 ^ 0x02).toByte, (0x07 ^ 0x03).toByte))
    }
    "applyMask with offset" in {
      val data = Array[Byte](0x00, 0x01, 0x02, 0x03, 0x04, 0x05)
      SWARUtil.applyMask(data, 1, 5, 0xFF000000)
      // only bytes 1..4 are masked; byte 0 and byte 5 are untouched
      data(0) should ===(0x00.toByte)
      data(1) should ===((0x01 ^ 0xFF).toByte)
      data(2) should ===((0x02 ^ 0x00).toByte)
      data(3) should ===((0x03 ^ 0x00).toByte)
      data(4) should ===((0x04 ^ 0x00).toByte)
      data(5) should ===(0x05.toByte)
    }
    "applyMask with empty range" in {
      val data = Array[Byte](0x01, 0x02, 0x03)
      SWARUtil.applyMask(data, 1, 1, 0xDEADBEEF)
      // nothing should change
      data should ===(Array[Byte](0x01, 0x02, 0x03))
    }
  }
