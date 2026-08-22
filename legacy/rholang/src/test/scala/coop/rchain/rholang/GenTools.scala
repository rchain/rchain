package coop.rchain.rholang
import cats.data.NonEmptyList
import org.scalacheck.Gen

object GenTools {

  val identifierGen: Gen[String] = nonemptyString(256)

  def nonemptyString(size: Int): Gen[String] = Gen.nonEmptyListOf(Gen.alphaChar).map(_.mkString)

  def oneOf[T](gs: NonEmptyList[Gen[T]]): Gen[T] =
    Gen.choose(0, gs.size - 1).flatMap(gs.toList(_))

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
