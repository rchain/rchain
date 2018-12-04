package coop.rchain.rholang.interpreter.matcher

import cats.Eq
import cats.data.WriterT
import cats.laws.discipline.{MonadTests, MonoidKTests}
import cats.tests.CatsSuite
import coop.rchain.rholang.interpreter.matcher.StreamT.{SCons, Step}
import monix.eval.Coeval
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers}

class StreamTSpec extends FlatSpec with Matchers {

  behavior of "StreamT"

  it must "not trigger spurious evaluation of underlying stream for a lazy monad" in {
    val stream: Stream[Int] = Stream.cons(1, ???)

    var step2: Coeval[Step[Coeval, Int]] = null

    noException shouldBe thrownBy {
      stream.head
      val wrappedStep1 = StreamT.fromStream(Coeval.now(stream))
      val step1        = wrappedStep1.next.value().asInstanceOf[SCons[Coeval, Int]]
      assert(step1.head == 1)
      val wrappedStep2 = step1.tail
      step2 = wrappedStep2.next
    }

    assertThrows[NotImplementedError] {
      step2.value()
    }
  }

}

class StreamTLawsSpec extends CatsSuite {

  type Safe[A]          = Coeval[A]
  type Effect[A]        = WriterT[Safe, String, A]
  type StreamTEffect[A] = StreamT[Effect, A]

  implicit val stringArb = Arbitrary(Gen.oneOf("", "a", "b", "c"))

  implicit def arbEff[A: Arbitrary]: Arbitrary[Effect[A]] =
    Arbitrary(for {
      s <- Arbitrary.arbitrary[String]
      a <- Arbitrary.arbitrary[A]
    } yield WriterT[Safe, String, A](Coeval.now(s -> a)))

  implicit def arbStreamEff[A: Arbitrary]: Arbitrary[StreamTEffect[A]] =
    Arbitrary(
      Gen.oneOf(
        for {
          s <- Arbitrary.arbitrary[Effect[A]]
        } yield StreamT.liftF(s),
        for {
          s <- Arbitrary.arbitrary[Effect[Stream[A]]]
        } yield StreamT.fromStream(s)
      )
    )

  implicit def eqEff[A: Eq]: Eq[Effect[A]]       = Eq.by(x => (x.value.value))
  implicit def eqFA[A: Eq]: Eq[StreamTEffect[A]] = Eq.by(StreamT.run[Effect, A])

  checkAll("StreamT.MonadLaws", MonadTests[StreamTEffect].monad[Int, Int, String])
  checkAll("StreamT.MonoidKLaws", MonoidKTests[StreamTEffect].monoidK[Int])

}
