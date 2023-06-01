package coop.rchain.rholang.interpreter.registry

import org.lightningj.util.ZBase32

import scala.annotation.tailrec

object Registry {
  def buildURI(arr: Array[Byte]): String = {
    val fullKey = new Array[Byte](34)
    Array.copy(arr, 0, fullKey, 0, 32)
    val crc = CRC14.compute(fullKey.view.slice(0, 32).toIndexedSeq)
    fullKey(32) = (crc & 0xff).toByte
    fullKey(33) = ((crc & 0xff00) >>> 6).toByte
    "rho:id:" + ZBase32.encodeToString(fullKey, 270)
  }

  object CRC14 {
    val INIT_REMAINDER: Short = 0
    def update(rem: Short, b: Byte): Short = {
      @tailrec
      def loop(i: Int, rem: Short): Short =
        if (i < 8) {
          val shiftRem: Short = (rem << 1).toShort
          if ((shiftRem & 0x4000) != 0)
            loop(i + 1, (shiftRem ^ 0x4805).toShort)
          else
            loop(i + 1, shiftRem)
        } else {
          rem
        }
      loop(0, (rem ^ (b << 6).toShort).toShort)
    }

    def compute(b: IndexedSeq[Byte]) =
      b.foldLeft(INIT_REMAINDER)(update(_, _))
  }
}
