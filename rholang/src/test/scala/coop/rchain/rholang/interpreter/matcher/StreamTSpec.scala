package coop.rchain.rholang.interpreter.matcher

import cats.arrow.FunctionK
import cats.data.{EitherT, WriterT}
import cats.syntax.all._
import cats.effect.laws.discipline.{BracketTests, SyncTests}
import cats.laws.discipline.{AlternativeTests, MonadErrorTests, MonadTests}
import cats.mtl.laws.discipline.MonadLayerControlTests
import cats.{~>, Eq, Monad}
import coop.rchain.catscontrib.laws.discipline.MonadTransTests
import coop.rchain.rholang.StackSafetySpec
import coop.rchain.rholang.interpreter.matcher.StreamT.{SCons, Step}
import monix.eval.Coeval
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import cats.instances.AllInstances
import cats.syntax.AllSyntax

class StreamTSpec extends AnyFlatSpec with Matchers {

  val maxDepth = StackSafetySpec.findMaxRecursionDepth()

  behavior of "StreamT"

  it must "allow for lazy computation of the stream's head and shape" in {
    val stack  = StreamT.liftF[Coeval, Int](Coeval.delay(???))
    val result = StreamT.run(stack)

    assertThrows[NotImplementedError] {
      result.value()
    }
  }

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

  private def hugeStream[F[_]: Monad](n: Int, acc: StreamT[F, Int]): StreamT[F, Int] = n match {
    case 0 => acc
    case _ => hugeStream(n - 1, StreamT[F, Int](Monad[F].pure(SCons(n, acc))))
  }

  it must "be stacksafe for a stacksafe F when calling StreamT.run[F]" in {
    val huge = hugeStream[Coeval](maxDepth - 1, StreamT.pure(maxDepth))

    assert(StreamT.run(huge).value() == Stream.range(1, maxDepth + 1))
  }

  it must "be stacksafe for a stacksafe F when calling StreamT.fromStream[F]" in {
    val reference = Stream.range(0, maxDepth)

    val huge = StreamT.fromStream(Coeval.now(reference))

    assert(StreamT.run(huge).value() == reference)
  }

  it must "be stacksafe for a stacksafe F when calling Monad[StreamT[F, *]].flatMap" in {

    def hugeFlatMap[F[_]: Monad](n: Int): F[Int] = n match {
      case 0 => n.pure[F]
      case _ => n.pure[F].flatMap(x => hugeFlatMap[F](x - 1))
    }

    val huge = hugeFlatMap[StreamT[Coeval, *]](maxDepth)

    assert(StreamT.run(huge).value() == Stream(0))
  }

  it must "be stacksafe for a stacksafe F when calling MonoidK[StreamT[F, *]].combineK" in {
    type StreamTCoeval[A] = StreamT[Coeval, A]

    val huge: StreamTCoeval[Int] = hugeStream(maxDepth - 1, StreamT.pure(maxDepth))
    val reference                = Stream.range(1, maxDepth + 1)

    val combined = huge.combineK(huge)

    assert(StreamT.run(combined).value() == (reference ++ reference))
  }

  it must "be stacksafe and lazy for a stacksafe F when calling dropTail" in {
    type StreamTCoeval[A] = StreamT[Coeval, A]

    val reference                    = Stream(1)
    val head: StreamTCoeval[Int]     = StreamT.fromStream(Coeval.now(reference))
    val throwing: StreamTCoeval[Int] = StreamT.liftF[Coeval, Int](Coeval.delay(???))
    val combined                     = head.combineK(throwing)

    val noTail = StreamT.dropTail(combined)

    assert(StreamT.run(noTail).value() == reference)
  }
}

class StreamTLawsSpec
    extends AnyFunSuite
    with LowPriorityDerivations
    with AllInstances
    with AllSyntax {

  type Safe[A]          = Coeval[A]
  type Err[A]           = EitherT[Safe, String, A]
  type Effect[A]        = WriterT[Err, String, A]
  type StreamTEffect[A] = StreamT[Effect, A]

  implicit val stringArb = Arbitrary(Gen.oneOf("", "a", "b", "c"))

  implicit def arbEff[A: Arbitrary]: Arbitrary[Effect[A]] =
    Arbitrary(for {
      s <- Arbitrary.arbitrary[String]
      a <- Arbitrary.arbitrary[A]
    } yield WriterT[Err, String, A](EitherT.liftF(Coeval.now(s -> a))))

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

  implicit val arbFunctionKStream: Arbitrary[Stream ~> Stream] =
    Arbitrary(
      Gen.oneOf(
        new (Stream ~> Stream) {
          def apply[A](fa: Stream[A]): Stream[A] = Stream.Empty
        },
        FunctionK.id[Stream]
      )
    )

  implicit def eqEff[A: Eq]: Eq[Effect[A]]       = Eq.by(x => x.value.value.attempt())
  implicit def eqFA[A: Eq]: Eq[StreamTEffect[A]] = Eq.by(StreamT.run[Effect, A])

  implicit def eqT: Eq[Throwable] = Eq.allEqual

  def checkProps(props: Seq[(String, Prop)]): Unit =
    for {
      (testName, prop) <- props
      _                = info(testName)
      _                = prop.check
    } yield ()

  test("StreamT.MonadLaws") { checkProps(MonadTests[StreamTEffect].monad[Int, Int, String].props) }

  test("StreamT.AlternativeLaws") {
    checkProps(AlternativeTests[StreamTEffect].alternative[Int, Int, String].props)
  }

  test("StreamT.MonadTransLaws") {
    checkProps(MonadTransTests[StreamT].monadTrans[Effect, Int, String].props)
  }
  test("StreamT.MonadLayerControlLaws") {
    checkProps(
      MonadLayerControlTests[StreamTEffect, Effect, Stream]
        .monadLayerControl[Int, String]
        .props
    )
  }

  test("StreamT.MonadErrorLaws") {
    checkProps(MonadErrorTests[StreamTEffect, String].monadError[Int, Int, String].props)
  }

  test("StreamT.MonadErrorUnitLaws") {
    checkProps(MonadErrorTests[StreamTEffect, Unit].monadError[Int, Int, String].props)
  }

  test("StreamT.SyncLaws") { checkProps(SyncTests[StreamTEffect].sync[Int, Int, String].props) }

  test("StreamT.SyncLaws.from") {
    val fromEffect =
      λ[Effect ~> StreamTEffect[*]](
        e => StreamT.liftF(e)
      )
    checkProps(
      BracketTests[StreamTEffect[*], Throwable]
        .bracketTrans[Effect, Int, Int](
          fromEffect
        )
        .props
    )
  }
}

trait LowPriorityDerivations {

  implicit def arbFunctionK[K[_]]: Arbitrary[K ~> K] =
    Arbitrary(Gen.const(FunctionK.id[K]))

}
