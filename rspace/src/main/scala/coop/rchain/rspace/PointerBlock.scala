package coop.rchain.rspace

import scodec.Codec
import scodec.Codec._
import scodec.codecs._

class PointerBlock private (val toVector: Vector[Option[Blake2b256Hash]]) {

  require(toVector.length == PointerBlock.length, "A PointerBlock's length must be 256")

  def updated(tuples: List[(Int, Option[Blake2b256Hash])]): PointerBlock =
    new PointerBlock(tuples.foldLeft(toVector) { (vec, curr) =>
      vec.updated(curr._1, curr._2)
    })

  def children: Vector[(Int, Blake2b256Hash)] =
    toVector.zipWithIndex.collect { case (Some(hash), idx) => (idx, hash) }

  override def equals(obj: scala.Any): Boolean = obj match {
    case pb: PointerBlock => pb.toVector == toVector
    case _                => false
  }

  override def hashCode(): Int = toVector.hashCode()
}

object PointerBlock {

  val length = 256

  def create(): PointerBlock = new PointerBlock(Vector.fill(length)(None))

  def fromVector(vector: Vector[Option[Blake2b256Hash]]): PointerBlock = new PointerBlock(vector)

  implicit val codecPointerBlock: Codec[PointerBlock] =
    vectorOfN(
      provide(length),
      optional(ignore(size = 7) ~> bool, Blake2b256Hash.codecBlake2b256Hash)
    ).as[PointerBlock]
}
