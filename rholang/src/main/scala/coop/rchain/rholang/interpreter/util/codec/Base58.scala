/**
  * The MIT License (MIT)
  *
  * Copyright (c) 2016 Ratio Club, Inc.
  *
  * Permission is hereby granted, free of charge, to any person obtaining a copy
  * of this software and associated documentation files (the "Software"), to deal
  * in the Software without restriction, including without limitation the rights
  * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  * copies of the Software, and to permit persons to whom the Software is
  * furnished to do so, subject to the following conditions:
  *
  * The above copyright notice and this permission notice shall be included in all
  * copies or substantial portions of the Software.
  *
  * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
  * SOFTWARE.
  *
  * The code below was copied from https://github.com/multiformats/scala-multihash/blob/master/src/main/scala/io/mediachain/multihash/Base58.scala
  * and some refactoring was done and decode return type was changed to Option[Array[Byte]]
  */
package coop.rchain.rholang.interpreter.util.codec
import java.math.BigInteger

import scala.annotation.tailrec

object Base58 {
  val alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"
  val maxDigit = BigInteger.valueOf(58L)
  // char -> value
  private val digitMap = alphabet.zipWithIndex.toMap

  /**
    *
    * @param input binary data
    * @return the base-58 representation of input
    */
  def encode(input: Array[Byte]): String =
    if (input.isEmpty) ""
    else {
      val big     = new BigInteger(1, input.toArray)
      val builder = new StringBuilder

      @tailrec
      def go(current: BigInteger): Unit = current match {
        case BigInteger.ZERO => ()
        case _ =>
          val Array(x, remainder) = current.divideAndRemainder(maxDigit)
          val _                   = builder.append(alphabet.charAt(remainder.intValue))
          go(x)
      }
      go(big)
      input.takeWhile(_ == 0).foreach(_ => builder.append(alphabet.charAt(0)))
      builder.toString().reverse
    }

  /**
    *
    * @param input base-58 encoded data
    * @return the decoded data
    */
  def decode(input: String): Option[Array[Byte]] = {
    val (zeroChars, trim) = input.to(LazyList).span(_ == alphabet(0))
    val zeroes            = zeroChars.map(_ => 0: Byte).toArray

    if (trim.isEmpty)
      Some(zeroes)
    else {
      def getDigit(c: Char) =
        digitMap.get(c).map(x => BigInteger.valueOf(x.toLong))

      def accumulate(aOpt: Option[BigInteger], ch: Char) =
        for {
          a     <- aOpt
          digit <- getDigit(ch)
        } yield a.multiply(maxDigit).add(digit)

      trim
        .foldLeft(Option(BigInteger.ZERO))(accumulate)
        .map(decoded => zeroes ++ decoded.toByteArray.dropWhile(_ == 0))
    }
  }
}
