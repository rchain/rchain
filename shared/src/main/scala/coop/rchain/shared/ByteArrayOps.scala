package coop.rchain.shared

object ByteArrayOps {
  implicit class RichByteArray(bytes: Array[Byte]) {
    def toHex: String = bytes.map("%02x".format(_)).mkString("")
  }
}
