package coop.rchain.rspace

import scodec.Codec
import scodec.Codec._
import scodec.codecs._

case class PointerBlock(toVector: Vector[Option[Blake2b256Hash]]) {

  require(toVector.length == PointerBlock.length)

  def updated(tuples: List[(Int, Option[Blake2b256Hash])]): PointerBlock =
    PointerBlock(tuples.foldLeft(toVector) { (vec, curr) =>
      vec.updated(curr._1, curr._2)
    })

  def children: Vector[(Int, Blake2b256Hash)] =
    toVector.zipWithIndex.collect { case (Some(hash), idx) => (idx, hash) }
}

object PointerBlock {

  val length = 256

  def create(): PointerBlock = PointerBlock(Vector.fill(PointerBlock.length)(None))

  implicit val codecPointerBlock: Codec[PointerBlock] =
    vectorOfN(
      provide(256),
      optional(ignore(size = 7) ~> bool, Blake2b256Hash.codecBlake2b256Hash)
    ).as[PointerBlock]
}
