package coop.rchain.rspace.history

import scodec.Codec
import scodec.codecs.utf8

case class Branch(name: String)

object Branch {

  val MASTER: Branch = Branch("master")

  val REPLAY: Branch = Branch("replay")

  implicit val codecBranch: Codec[Branch] =
    utf8.as[Branch]
}
