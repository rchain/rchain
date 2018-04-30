package coop.rchain.models

object BundleOps {
  implicit class BundleEnhance(b: Bundle) {
    def merge(other: Bundle): Bundle =
      other.copy(readFlag = b.readFlag && other.readFlag,
                 writeFlag = b.writeFlag && other.writeFlag)
  }
}
