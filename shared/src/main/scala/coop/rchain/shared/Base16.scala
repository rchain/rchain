package coop.rchain.shared

import scala.util.Try
import javax.xml.bind.DatatypeConverter

object Base16 {
  def encode(input: Array[Byte]): String = bytes2hex(input, None)

  /**
    * Decodes an input string by ignoring the non-hex characters. It always succeeds.
    * @param input
    * @return
    */
  def unsafeDecode(input: String): Array[Byte] = decode(input, "[^0-9A-Fa-f]").get

  /**
    * Decodes an input string by ignoring the separator characters
    * @param input
    * @param separatorsRx the regex matching the allowed separators
    * @return None if any non-hex and non-separator characters are encountered in the input
    *         Some otherwise
    */
  def decode(input: String, separatorsRx: String = ""): Option[Array[Byte]] =
    Try {
      val digitsOnly = input.replaceAll(separatorsRx, "")
      val padded =
        if (digitsOnly.length % 2 == 0) digitsOnly
        else "0" + digitsOnly
      DatatypeConverter.parseHexBinary(padded)
    }.toOption

  private def bytes2hex(bytes: Array[Byte], sep: Option[String]): String =
    bytes.map("%02x".format(_)).mkString(sep.getOrElse(""))

}
