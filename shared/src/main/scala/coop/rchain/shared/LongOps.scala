package coop.rchain.shared
import java.lang.Long.numberOfLeadingZeros
import java.util.Locale

object LongOps {

  /**
    * Formats size in bytes to human-readable format
    * Avoids use of slow math operations and loops
    *
    * Assumes 1 KB = 1024 bytes, not 1000 bytes as some other methods do
    */
  def toHumanReadableSize(value: Long): String =
    if (value < 1024) {
      value + " B"
    } else {
      val z = 63 - numberOfLeadingZeros(value)
      "%.1f %sB".formatLocal(Locale.US, value.toDouble / (1L << z), " KMGTPE".charAt(z / 10))
    }
}
