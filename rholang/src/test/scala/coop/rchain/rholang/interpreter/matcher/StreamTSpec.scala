package coop.rchain.rholang.interpreter.matcher

import cats.{Eq, Id}
import cats.data.WriterT
import cats.laws.discipline.{MonadTests, MonoidKTests}
import cats.tests.CatsSuite
import monix.eval.Coeval
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers}

class StreamTSpec extends FlatSpec with Matchers {

  behavior of "StreamT"

  it must "not trigger spurious evaluation of underlying stream for a lazy monad" in {
    val stream: Stream[Int] = Stream.cons(1, ???)
    var wrapped: StreamT[Coeval, Int] = null

    noException shouldBe thrownBy {
      stream.head
      wrapped = StreamT.fromStream[Coeval, Int](Coeval.now(stream))
    }

    val next = wrapped.next

    assertThrows[NotImplementedError] {
      next.value() //would be equal to SCons(1, StreamT(Monad[F].pure(SCons(???, empty))))
      //FIXME make the stream not evaluate the next element until absolutely needed
      // by requiring [F: Defer]
    }
  }

}

class StreamTLawsSpec extends CatsSuite {

  type Safe[A] = Coeval[A]
  type Effect[A] = WriterT[Safe, String, A]
  type StreamTEffect[A] = StreamT[Effect, A]

  implicit val stringArb = Arbitrary(Gen.oneOf("","a", "b", "c"))

  implicit def arbEff[A : Arbitrary]: Arbitrary[Effect[A]] = Arbitrary(for {
    s <- Arbitrary.arbitrary[String]
    a <- Arbitrary.arbitrary[A]
  } yield WriterT[Safe, String, A](Coeval.now(s -> a)))

  implicit def arbStreamEff[A : Arbitrary]: Arbitrary[StreamTEffect[A]] = Arbitrary(Gen.oneOf(
    for {
      s <- Arbitrary.arbitrary[Effect[A]]
    } yield StreamT.liftF(s),
    for {
      s <- Arbitrary.arbitrary[Effect[Stream[A]]]
    } yield StreamT.fromStream(s)
  ))

  implicit def eqEff[A: Eq]: Eq[Effect[A]] = Eq.by(x => (x.value.value))
  implicit def eqFA[A: Eq]: Eq[StreamTEffect[A]] = Eq.by(StreamT.run[Effect, A])

  checkAll("StreamT.MonadLaws", MonadTests[StreamTEffect].monad[Int, Int, String])
  checkAll("StreamT.MonoidKLaws", MonoidKTests[StreamTEffect].monoidK[Int])

}
