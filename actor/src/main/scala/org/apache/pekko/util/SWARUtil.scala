/*
 * Copyright 2024 The Netty Project
 *
 * The Netty Project licenses this file to you under the Apache License, version 2.0 (the
 * "License"); you may not use this file except in compliance with the License. You may obtain a
 * copy of the License at:
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License
 * is distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express
 * or implied. See the License for the specific language governing permissions and limitations under
 * the License.
 */

package org.apache.pekko.util

import java.lang.invoke.MethodHandles

import org.apache.pekko.annotation.InternalApi

/**
 * SWAR (SIMD Within A Register) utility class. Internal Use Only.
 * <p>
 * Copied from the Netty Project.
 * https://github.com/netty/netty/blob/d28a0fc6598b50fbe8f296831777cf4b653a475f/common/src/main/java/io/netty/util/internal/SWARUtil.java
 * </p>
 * <p>
 * Multi-byte reads use [[java.lang.invoke.MethodHandles#byteArrayViewVarHandle]], which allows
 * reading several bytes from a byte array as a single typed value (e.g. `short`, `int`,
 * or `long`) in one operation rather than reading and shifting each byte individually.
 * </p>
 * <p>
 * The JDK itself uses the same technique.  Since Java 17, `jdk.internal.util.ByteArray` (big
 * endian) and `jdk.internal.util.ByteArrayLittleEndian` (little endian) use
 * `MethodHandles.byteArrayViewVarHandle` for every primitive type, and those helpers back the
 * public APIs of `java.io.DataInputStream` (`readShort`, `readInt`,
 * `readLong`, etc.) and `java.util.UUID` construction from bytes.
 * </p>
 * <h3>Why this is faster than byte-by-byte shifts</h3>
 * <ul>
 *   <li><b>Single native load instruction</b> – on x86/x64 and AArch64 the HotSpot JIT intrinsifies
 *       the VarHandle access into a single `MOVZX`, `MOV`, or `LDR` instruction
 *       that reads the full value directly from memory, whereas manual byte-shift code requires
 *       multiple load-and-shift-and-or sequences that are harder for the JIT to collapse.</li>
 *   <li><b>Consolidated bounds check</b> – a single range check covers the entire multi-byte read;
 *       individual `array(i)` accesses each carry their own implicit bounds check.</li>
 *   <li><b>No alignment requirement</b> – unlike `sun.misc.Unsafe` the VarHandle variant
 *       works correctly on unaligned offsets, so callers do not need to pad or copy data to satisfy
 *       alignment constraints.</li>
 *   <li><b>SWAR arithmetic</b> – reading a full `long` with a single VarHandle call means
 *       eight bytes arrive in one register, enabling SWAR patterns that test all eight bytes in
 *       parallel (see [[applyPattern]]).</li>
 * </ul>
 * <p>
 * A runtime `try/catch` guards each VarHandle creation; if the JVM does not support the API
 * (e.g. older Android runtimes) the code falls back to explicit byte-by-byte shift implementations
 * (`getLongBEWithoutMethodHandle`, etc.) so behaviour is always correct.
 * </p>
 */
@InternalApi
private[pekko] object SWARUtil {

  private val (longBeArrayView, longBeArrayViewSupported) =
    try {
      (MethodHandles.byteArrayViewVarHandle(
          classOf[Array[Long]], java.nio.ByteOrder.BIG_ENDIAN),
        true)
    } catch {
      case _: Throwable => (null, false)
    }

  private val (longLeArrayView, longLeArrayViewSupported) =
    try {
      (MethodHandles.byteArrayViewVarHandle(
          classOf[Array[Long]], java.nio.ByteOrder.LITTLE_ENDIAN),
        true)
    } catch {
      case _: Throwable => (null, false)
    }

  private val (intBeArrayView, intBeArrayViewSupported) =
    try {
      (MethodHandles.byteArrayViewVarHandle(
          classOf[Array[Int]], java.nio.ByteOrder.BIG_ENDIAN),
        true)
    } catch {
      case _: Throwable => (null, false)
    }

  private val (intLeArrayView, intLeArrayViewSupported) =
    try {
      (MethodHandles.byteArrayViewVarHandle(
          classOf[Array[Int]], java.nio.ByteOrder.LITTLE_ENDIAN),
        true)
    } catch {
      case _: Throwable => (null, false)
    }

  private val (shortBeArrayView, shortBeArrayViewSupported) =
    try {
      (MethodHandles.byteArrayViewVarHandle(
          classOf[Array[Short]], java.nio.ByteOrder.BIG_ENDIAN),
        true)
    } catch {
      case _: Throwable => (null, false)
    }

  private val (shortLeArrayView, shortLeArrayViewSupported) =
    try {
      (MethodHandles.byteArrayViewVarHandle(
          classOf[Array[Short]], java.nio.ByteOrder.LITTLE_ENDIAN),
        true)
    } catch {
      case _: Throwable => (null, false)
    }

  /**
   * Compiles given byte into a long pattern suitable for SWAR operations.
   */
  def compilePattern(byteToFind: Byte): Long = (byteToFind & 0xFFL) * 0x101010101010101L

  /**
   * Applies a compiled pattern to given word.
   * Returns a word where each byte that matches the pattern has the highest bit set.
   *
   * @param word    the word to apply the pattern to
   * @param pattern the pattern to apply
   * @return a word where each byte that matches the pattern has the highest bit set
   */
  def applyPattern(word: Long, pattern: Long): Long = {
    val input = word ^ pattern
    val tmp = (input & 0x7F7F7F7F7F7F7F7FL) + 0x7F7F7F7F7F7F7F7FL
    ~(tmp | input | 0x7F7F7F7F7F7F7F7FL)
  }

  /**
   * Returns the index of the first occurrence of byte that specificied in the pattern.
   * If no pattern is found, returns 8. Currently only supports big endian.
   *
   * @param word     the return value of {@link #applyPattern(long, long)}
   * @return the index of the first occurrence of the specified pattern in the specified word.
   * If no pattern is found, returns 8.
   */
  def getIndex(word: Long): Int =
    java.lang.Long.numberOfLeadingZeros(word) >>> 3

  /**
   * Returns the long value at the specified index in the given byte array.
   * Uses big-endian byte order. Uses a VarHandle byte array view if supported.
   * Does not range check - assumes caller has checked bounds.
   *
   * @param array the byte array to read from
   * @param index the index to read from
   * @return the long value at the specified index
   */
  def getLong(array: Array[Byte], index: Int): Long = {
    if (longBeArrayViewSupported) {
      longBeArrayView.get(array, index)
    } else {
      getLongBEWithoutMethodHandle(array, index)
    }
  }

  /**
   * Returns the long value at the specified index in the given byte array.
   * Uses a VarHandle byte array view if supported.
   * Does not range check - assumes caller has checked bounds.
   *
   * @param array the byte array to read from
   * @param index the index to read from
   * @return the long value at the specified index
   */
  def getLong(array: Array[Byte], index: Int, bigEndian: Boolean): Long = {
    if (bigEndian) {
      getLong(array, index)
    } else if (longLeArrayViewSupported) {
      longLeArrayView.get(array, index)
    } else {
      getLongLEWithoutMethodHandle(array, index)
    }
  }

  /**
   * Returns the int value at the specified index in the given byte array.
   * Uses big-endian byte order. Uses a VarHandle byte array view if supported.
   * Does not range check - assumes caller has checked bounds.
   *
   * @param array the byte array to read from
   * @param index the index to read from
   * @return the int value at the specified index
   */
  def getInt(array: Array[Byte], index: Int): Int = {
    if (intBeArrayViewSupported) {
      intBeArrayView.get(array, index)
    } else {
      getIntBEWithoutMethodHandle(array, index)
    }
  }

  /**
   * Returns the int value at the specified index in the given byte array.
   * Uses a VarHandle byte array view if supported.
   * Does not range check - assumes caller has checked bounds.
   *
   * @param array the byte array to read from
   * @param index the index to read from
   * @param bigEndian whether to use big-endian or little-endian byte order
   * @return the int value at the specified index
   */
  def getInt(array: Array[Byte], index: Int, bigEndian: Boolean): Int = {
    if (bigEndian) {
      getInt(array, index)
    } else if (intLeArrayViewSupported) {
      intLeArrayView.get(array, index)
    } else {
      getIntLEWithoutMethodHandle(array, index)
    }
  }

  private[pekko] def getLongBEWithoutMethodHandle(array: Array[Byte], index: Int): Long = {
    (array(index).toLong & 0xFF) << 56 |
    (array(index + 1).toLong & 0xFF) << 48 |
    (array(index + 2).toLong & 0xFF) << 40 |
    (array(index + 3).toLong & 0xFF) << 32 |
    (array(index + 4).toLong & 0xFF) << 24 |
    (array(index + 5).toLong & 0xFF) << 16 |
    (array(index + 6).toLong & 0xFF) << 8 |
    (array(index + 7).toLong & 0xFF)
  }

  private[pekko] def getLongLEWithoutMethodHandle(array: Array[Byte], index: Int): Long = {
    (array(index).toLong & 0xFF) |
    (array(index + 1).toLong & 0xFF) << 8 |
    (array(index + 2).toLong & 0xFF) << 16 |
    (array(index + 3).toLong & 0xFF) << 24 |
    (array(index + 4).toLong & 0xFF) << 32 |
    (array(index + 5).toLong & 0xFF) << 40 |
    (array(index + 6).toLong & 0xFF) << 48 |
    (array(index + 7).toLong & 0xFF) << 56
  }

  private[pekko] def getIntBEWithoutMethodHandle(array: Array[Byte], index: Int): Int = {
    (array(index) & 0xFF) << 24 |
    (array(index + 1) & 0xFF) << 16 |
    (array(index + 2) & 0xFF) << 8 |
    (array(index + 3) & 0xFF)
  }

  private[pekko] def getIntLEWithoutMethodHandle(array: Array[Byte], index: Int): Int = {
    (array(index) & 0xFF) |
    (array(index + 1) & 0xFF) << 8 |
    (array(index + 2) & 0xFF) << 16 |
    (array(index + 3) & 0xFF) << 24
  }

  /**
   * Returns the short value at the specified index in the given byte array.
   * Uses big-endian byte order. Uses a VarHandle byte array view if supported.
   * Does not range check - assumes caller has checked bounds.
   *
   * @param array the byte array to read from
   * @param index the index to read from
   * @return the short value at the specified index
   */
  def getShort(array: Array[Byte], index: Int): Short = {
    if (shortBeArrayViewSupported) {
      shortBeArrayView.get(array, index).asInstanceOf[Short]
    } else {
      getShortBEWithoutMethodHandle(array, index)
    }
  }

  /**
   * Returns the short value at the specified index in the given byte array.
   * Uses a VarHandle byte array view if supported.
   * Does not range check - assumes caller has checked bounds.
   *
   * @param array the byte array to read from
   * @param index the index to read from
   * @param bigEndian whether to use big-endian or little-endian byte order
   * @return the short value at the specified index
   */
  def getShort(array: Array[Byte], index: Int, bigEndian: Boolean): Short = {
    if (bigEndian) {
      getShort(array, index)
    } else if (shortLeArrayViewSupported) {
      shortLeArrayView.get(array, index).asInstanceOf[Short]
    } else {
      getShortLEWithoutMethodHandle(array, index)
    }
  }

  private[pekko] def getShortBEWithoutMethodHandle(array: Array[Byte], index: Int): Short =
    ((array(index) & 0xFF) << 8 | (array(index + 1) & 0xFF)).toShort

  private[pekko] def getShortLEWithoutMethodHandle(array: Array[Byte], index: Int): Short =
    ((array(index) & 0xFF) | (array(index + 1) & 0xFF) << 8).toShort

  /**
   * Applies a 4-byte XOR mask in-place to `array[from, until)`.
   * `maskValue` must be pre-rotated so that its most-significant byte (bits 24–31)
   * applies to `array(from)`.
   *
   * When the big-endian `long` VarHandle is supported, eight bytes are processed per
   * iteration using a single load and store; otherwise four bytes are processed per
   * iteration.  Trailing bytes (fewer than one full group) are handled individually.
   *
   * Does not range-check: the caller is responsible for ensuring that
   * `from >= 0` and `until <= array.length`.
   *
   * @param array     the byte array to modify in-place
   * @param from      start index (inclusive)
   * @param until     end index (exclusive)
   * @param maskValue the 4-byte mask as a big-endian `Int`
   */
  def applyMask(array: Array[Byte], from: Int, until: Int, maskValue: Int): Unit = {
    val len = until - from
    val m0 = ((maskValue >> 24) & 0xFF).toByte
    val m1 = ((maskValue >> 16) & 0xFF).toByte
    val m2 = ((maskValue >> 8) & 0xFF).toByte
    val m3 = (maskValue & 0xFF).toByte

    var offset = from

    if (longBeArrayViewSupported) {
      // Expand 4-byte mask to 8-byte mask for SWAR long XOR (big-endian: m0 is MSB)
      val maskLong = (maskValue.toLong << 32) | (maskValue.toLong & 0xFFFFFFFFL)
      val last = from + (len & ~7)
      while (offset < last) {
        val word = longBeArrayView.get(array, offset).asInstanceOf[Long]
        longBeArrayView.set(array, offset, word ^ maskLong)
        offset += 8
      }
    } else {
      val last = from + (len & ~3)
      while (offset < last) {
        array(offset) = (array(offset) ^ m0).toByte
        array(offset + 1) = (array(offset + 1) ^ m1).toByte
        array(offset + 2) = (array(offset + 2) ^ m2).toByte
        array(offset + 3) = (array(offset + 3) ^ m3).toByte
        offset += 4
      }
    }

    // Handle remaining 0–7 bytes (8-byte path) or 0–3 bytes (4-byte fallback)
    // After processing (8k) bytes, the mask is still aligned to m0 (since 8 % 4 == 0)
    val remaining = until - offset
    if (remaining > 0) {
      array(offset) = (array(offset) ^ m0).toByte
      if (remaining > 1) {
        array(offset + 1) = (array(offset + 1) ^ m1).toByte
        if (remaining > 2) {
          array(offset + 2) = (array(offset + 2) ^ m2).toByte
          if (remaining > 3) {
            array(offset + 3) = (array(offset + 3) ^ m3).toByte
            if (remaining > 4) {
              array(offset + 4) = (array(offset + 4) ^ m0).toByte
              if (remaining > 5) {
                array(offset + 5) = (array(offset + 5) ^ m1).toByte
                if (remaining > 6) {
                  array(offset + 6) = (array(offset + 6) ^ m2).toByte
                }
              }
            }
          }
        }
      }
    }
  }

}
