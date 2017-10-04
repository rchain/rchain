package coop.rchain.rosette.parser

package object bytecode {
  implicit class RichInt(byte: Int) {
    def hasByteFormat(bits: String): Boolean =
      Integer.parseInt(bits, 2) == byte

    def matches(bits: String): Boolean = {
      val min = bits.map(c => if (c != '0' && c != '1') '0' else c)
      val max = bits.map(c => if (c != '0' && c != '1') '1' else c)

      if (byte >= Integer.parseInt(min, 2) && byte <= Integer.parseInt(max, 2))
        true
      else false
    }

    def slice(m: Int, n: Int): Int = {
      val range = n - m

      if (range > 0) {
        (byte >> m) & ((1 << range) - 1)
      } else {
        0
      }
    }

    def concat(low: Int): Int = (byte << 8) | low

    def bit(n: Int): Boolean = if ((byte & (1 << n)) != 0) true else false

    def lowNibble: Int = byte & 0x0F

    def highNibble: Int = byte >> 4
  }
}
