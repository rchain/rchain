package coop.rchain.crypto.codec
import scala.util.Try

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
  def decode(input: String, separatorsRx : String = ""): Option[Array[Byte]] = {
    val paddedInput =
      if (input.length % 2 == 0) input
      else "0" + input

    hex2bytes(paddedInput, separatorsRx)
  }

  private def bytes2hex(bytes: Array[Byte], sep: Option[String]): String =
    bytes.map("%02x".format(_)).mkString(sep.getOrElse(""))

  private def hex2bytes(hex: String, separatorsRx : String): Option[Array[Byte]] =
    Try {
      val digitsOnly = hex.replaceAll(separatorsRx, "")

      digitsOnly
        .sliding(2, 2)
        .toArray
        .map(Integer.parseInt(_, 16).toByte)
    }.toOption
}
