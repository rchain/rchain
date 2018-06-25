package coop.rchain.rspace.history

import scodec.Codec
import scodec.Codec._
import scodec.codecs._

class PointerBlock private (val toVector: Vector[Pointer]) {

  require(toVector.length == PointerBlock.length, "A PointerBlock's length must be 256")

  def updated(tuples: List[(Int, Pointer)]): PointerBlock =
    new PointerBlock(tuples.foldLeft(toVector) { (vec, curr) =>
      vec.updated(curr._1, curr._2)
    })

  def updated(tuples: (Int, Pointer)*): PointerBlock =
    updated(tuples.toList)

  def children: Vector[NonEmptyPointer] =
    toVector.collect {
      case p: NonEmptyPointer => p
    }

  def childrenWithIndex: Vector[(NonEmptyPointer, Int)] =
    toVector.zipWithIndex.collect {
      case (p: NonEmptyPointer, i) => (p, i)
    }

  override def toString: String = s"PointerBlock(toVector: ${childrenWithIndex})"

  override def equals(obj: scala.Any): Boolean = obj match {
    case pb: PointerBlock => pb.toVector == toVector
    case _                => false
  }

  override def hashCode(): Int = toVector.hashCode()
}

object PointerBlock {

  val length = 256

  def create(): PointerBlock = new PointerBlock(Vector.fill(length)(EmptyPointer))

  def create(first: (Int, Pointer)): PointerBlock =
    PointerBlock.create().updated(List(first))

  def create(first: (Int, Pointer), second: (Int, Pointer)): PointerBlock =
    PointerBlock.create().updated(List(first, second))

  def fromVector(vector: Vector[Pointer]): PointerBlock = new PointerBlock(vector)
  implicit val codecPointerBlock: Codec[PointerBlock] =
    vectorOfN(
      provide(length),
      codecPointer
    ).as[PointerBlock]

  def unapply(arg: PointerBlock): Option[Vector[Pointer]] = Option(arg.toVector)
}
