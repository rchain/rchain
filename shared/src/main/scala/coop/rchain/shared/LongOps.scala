package coop.rchain.shared
import java.lang.Long.numberOfLeadingZeros
import java.util.Locale

object LongOps {

  implicit class RichLong(value: Long) {

    /**
      * Formats size in bytes to human-readable format
      * Avoids use of slow math operations and loops
      *
      * Assumes 1 KB = 1024 bytes, not 1000 bytes as some other methods do
      */
    def toHumanReadableSize: String =
      if (value < 1024) {
        value + " B"
      } else {
        //division and then multiplication by 10 resets trailing bits
        val z: Int = (63 - numberOfLeadingZeros(value)) / 10
        "%.1f %sB".formatLocal(Locale.US, value.toDouble / (1L << (z * 10)), " KMGTPE".charAt(z))
      }
  }
}
