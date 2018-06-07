package coop.rchain.rspace.history

import coop.rchain.rspace.Blake2b256Hash
import scodec.Codec
import scodec.Codec._
import scodec.codecs._

class PointerBlock private (val toVector: Vector[Child]) {

  require(toVector.length == PointerBlock.length, "A PointerBlock's length must be 256")

  def updated(tuples: List[(Int, Child)]): PointerBlock =
    new PointerBlock(tuples.foldLeft(toVector) { (vec, curr) =>
      vec.updated(curr._1, curr._2)
    })

  def children: Vector[(Int, Blake2b256Hash)] =
    toVector.zipWithIndex.collect {
      case (NodeChild(hash), idx) => (idx, hash)
      case (LeafChild(hash), idx) => (idx, hash)
    }

  override def toString: String = s"PointerBlock(toVector: ${toVector.toString})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case pb: PointerBlock => pb.toVector == toVector
    case _                => false
  }

  override def hashCode(): Int = toVector.hashCode()
}

object PointerBlock {

  val length = 256

  def create(): PointerBlock = new PointerBlock(Vector.fill(length)(EmptyChild))

  def fromVector(vector: Vector[Child]): PointerBlock = new PointerBlock(vector)
  implicit val codecPointerBlock: Codec[PointerBlock] =
    vectorOfN(
      provide(length),
      codecChild
    ).as[PointerBlock]

  def unapply(arg: PointerBlock): Option[Vector[Child]] = Option(arg.toVector)
}
