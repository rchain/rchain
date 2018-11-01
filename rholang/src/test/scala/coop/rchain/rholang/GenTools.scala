package coop.rchain.rholang
import org.scalacheck.Gen

object GenTools {
  val identifierGen: Gen[String] = nonemptyString(Gen.alphaChar, 256)

  def nonemptyString(g: Gen[Char], size: Int): Gen[String] = Gen.nonEmptyListOf(g).map(_.mkString)

  def oneOf[T](gs: Seq[Gen[T]]): Gen[T] =
    if (gs.nonEmpty)
      Gen.choose(0, gs.size - 1).flatMap(gs(_))
    else
      throw new IllegalArgumentException("oneOf called on empty generator collection")

  def nonemptySubSeq[T](items: Seq[T]): Gen[Seq[T]] =
    for {
      count  <- Gen.choose(1, items.length)
      output <- Gen.pick(count, items)
    } yield output

  def nonemptyLimitedList[T](maxLength: Int, gen: Gen[T]): Gen[List[T]] =
    for {
      length <- Gen.choose(1, maxLength)
      output <- Gen.listOfN(length, gen)
    } yield output
}
