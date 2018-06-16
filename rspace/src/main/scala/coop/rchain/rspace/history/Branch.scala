package coop.rchain.rspace.history

import scodec.Codec
import scodec.codecs.utf8

case class Branch(name: String)

object Branch {

  val master: Branch = Branch("master")

  val replay: Branch = Branch("replay")

  implicit val codecBranch: Codec[Branch] =
    utf8.as[Branch]
}
