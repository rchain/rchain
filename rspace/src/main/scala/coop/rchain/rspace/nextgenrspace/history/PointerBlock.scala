package coop.rchain.rspace.nextgenrspace.history
import coop.rchain.rspace.Blake2b256Hash
import coop.rchain.rspace.nextgenrspace.history.History.codecTrie
import scodec.Codec
import scodec.bits.BitVector
import scodec.codecs.{provide, vectorOfN}
import coop.rchain.shared.AttemptOps._

final case class PointerBlock private (toVector: Vector[Trie]) {
  def updated(tuples: List[(Int, Trie)]): PointerBlock =
    new PointerBlock(tuples.foldLeft(toVector) { (vec, curr) =>
      vec.updated(curr._1, curr._2)
    })

  def countNonEmpty: Int = toVector.count(_ != EmptyTrie)
}

object PointerBlock {
  val length = 256

  def create(): PointerBlock = new PointerBlock(Vector.fill(length)(EmptyTrie))

  def create(first: (Int, Trie)): PointerBlock =
    PointerBlock.create().updated(List(first))

  def create(first: (Int, Trie), second: (Int, Trie)): PointerBlock =
    PointerBlock.create().updated(List(first, second))

  implicit val codecPointerBlock: Codec[PointerBlock] =
    vectorOfN(
      provide(length),
      codecTrie
    ).as[PointerBlock]

  def hash(pb: PointerBlock): Blake2b256Hash =
    codecPointerBlock
      .encode(pb)
      .map((vector: BitVector) => Blake2b256Hash.create(vector.toByteArray))
      .get

  def unapply(arg: PointerBlock): Option[Vector[Trie]] = Option(arg.toVector)
}
