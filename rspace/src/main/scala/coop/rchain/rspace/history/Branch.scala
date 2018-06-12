package coop.rchain.rspace.history

import scodec.Codec
import scodec.codecs.utf8

case class Branch(name: String)

object Branch {

  implicit val codecBranch: Codec[Branch] =
    utf8.as[Branch]
}
