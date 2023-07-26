package coop.rchain.models.rholangN

/** *
  * Nothing can be received from a (quoted) bundle with `readFlag = false`.
  * Likeise nothing can be sent to a (quoted) bundle with `writeFlag = false`.
  *
  * If both flags are set to false, bundle allows only for equivalance check.
  *
  * @param writeFlag flag indicating whether bundle is writeable
  * @param readFlag flag indicating whether bundle is readable
  */
final class BundleN(val body: ParN, val writeFlag: Boolean, val readFlag: Boolean) extends OtherN {
  def merge(other: BundleN): BundleN = {
    val wFlag = writeFlag && other.writeFlag
    val rFlag = readFlag && other.readFlag
    BundleN(other.body, wFlag, rFlag)
  }
}
object BundleN {
  def apply(body: ParN, writeFlag: Boolean, readFlag: Boolean): BundleN =
    new BundleN(body, writeFlag, readFlag)
}
