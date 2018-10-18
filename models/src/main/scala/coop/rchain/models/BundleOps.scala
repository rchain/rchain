package coop.rchain.models

import cats.Show

object BundleOps {
  implicit class BundleEnhance(b: Bundle) {
    def merge(other: Bundle): Bundle =
      other.copy(
        readFlag = b.readFlag && other.readFlag,
        writeFlag = b.writeFlag && other.writeFlag
      )
  }

  implicit val showInstance: Show[Bundle] = Show.show[Bundle] { bundle =>
    val sign =
      if (bundle.readFlag && bundle.writeFlag) ""
      else if (bundle.readFlag && !bundle.writeFlag) "-"
      else if (!bundle.readFlag && bundle.writeFlag) "+"
      else if (!bundle.readFlag && !bundle.writeFlag) "0"
    "%-8s".format(s"bundle$sign")
  }
}
