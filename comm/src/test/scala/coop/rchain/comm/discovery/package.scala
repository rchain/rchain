package coop.rchain.comm

package object discovery {

  implicit class BinaryStringToByte(val str: String) extends AnyVal {
    def b: Byte = (Integer.parseInt(str, 2) & 0xFF).toByte
  }

}
