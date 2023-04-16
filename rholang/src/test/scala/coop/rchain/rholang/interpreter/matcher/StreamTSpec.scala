package coop.rchain.rholang.interpreter.matcher

import cats.arrow.FunctionK
import cats.data.{EitherT, WriterT}
import cats.syntax.all._
import cats.effect.laws._
import cats.laws.discipline.{AlternativeTests, MonadErrorTests, MonadTests}
import cats.mtl.laws.discipline.MonadLayerControlTests
import cats.{~>, effect, Eq, Eval, Monad}
import coop.rchain.catscontrib.laws.discipline.MonadTransTests
import coop.rchain.rholang.StackSafetySpec
import coop.rchain.rholang.interpreter.matcher.StreamT.{SCons, Step}
import cats.effect.Sync
import cats.effect.kernel.Sync.Type
import org.scalacheck.{Arbitrary, Gen, Prop}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import cats.instances.AllInstances
import cats.syntax.AllSyntax
import coop.rchain.catscontrib.effect.implicits.sEval
import org.scalacheck.ScalacheckShapeless.arbitrarySingletonType

class StreamTSpec extends AnyFlatSpec with Matchers {

  val maxDepth = StackSafetySpec.findMaxRecursionDepth()

  behavior of "StreamT"

  it must "allow for lazy computation of the stream's head and shape" in {
    val stack  = StreamT.liftF[Eval, Int](Sync[Eval].delay(???))
    val result = StreamT.run(stack)

    assertThrows[NotImplementedError] {
      result.value
    }
  }

  it must "not trigger spurious evaluation of underlying stream for a lazy monad" in {
    val stream: Stream[Int] = Stream.cons(1, ???)

    var step2: Eval[Step[Eval, Int]] = null

    noException shouldBe thrownBy {
      stream.head
      val wrappedStep1 = StreamT.fromStream(Eval.now(stream))
      val step1        = wrappedStep1.next.value.asInstanceOf[SCons[Eval, Int]]
      assert(step1.head == 1)
      val wrappedStep2 = step1.tail
      step2 = wrappedStep2.next
    }

    assertThrows[NotImplementedError] {
      step2.value
    }
  }

  private def hugeStream[F[_]: Monad](n: Int, acc: StreamT[F, Int]): StreamT[F, Int] = n match {
    case 0 => acc
    case _ => hugeStream(n - 1, StreamT[F, Int](Monad[F].pure(SCons(n, acc))))
  }

  it must "be stacksafe for a stacksafe F when calling StreamT.run[F]" in {
    val huge = hugeStream[Eval](maxDepth - 1, StreamT.pure(maxDepth))

    assert(StreamT.run(huge).value == Stream.range(1, maxDepth + 1))
  }

  it must "be stacksafe for a stacksafe F when calling StreamT.fromStream[F]" in {
    val reference = Stream.range(0, maxDepth)

    val huge = StreamT.fromStream(Eval.now(reference))

    assert(StreamT.run(huge).value == reference)
  }

  it must "be stacksafe for a stacksafe F when calling Monad[StreamT[F, *]].flatMap" in {

    def hugeFlatMap[F[_]: Monad](n: Int): F[Int] = n match {
      case 0 => n.pure[F]
      case _ => n.pure[F].flatMap(x => hugeFlatMap[F](x - 1))
    }

    implicit val x: Monad[StreamT[Eval, *]] =
      coop.rchain.rholang.interpreter.matcher.StreamT.streamTMonad[Eval]
    val huge = hugeFlatMap[StreamT[Eval, *]](maxDepth)

    assert(StreamT.run(huge).value == Stream(0))
  }

  it must "be stacksafe for a stacksafe F when calling MonoidK[StreamT[F, *]].combineK" in {
    type StreamTEval[A] = StreamT[Eval, A]

    val huge: StreamTEval[Int] = hugeStream(maxDepth - 1, StreamT.pure(maxDepth))
    val reference              = Stream.range(1, maxDepth + 1)

    val combined = huge.combineK(huge)

    assert(StreamT.run(combined).value == (reference ++ reference))
  }

  it must "be stacksafe and lazy for a stacksafe F when calling dropTail" in {
    type StreamTEval[A] = StreamT[Eval, A]

    val reference                  = Stream(1)
    val head: StreamTEval[Int]     = StreamT.fromStream(Eval.now(reference))
    val throwing: StreamTEval[Int] = StreamT.liftF[Eval, Int](Sync[Eval].delay(???))
    val combined                   = head.combineK(throwing)

    val noTail = StreamT.dropTail(combined)

    assert(StreamT.run(noTail).value == reference)
  }
}

class StreamTLawsSpec
    extends AnyFunSuite
    with LowPriorityDerivations
    with AllInstances
    with AllSyntax {

  type Safe[A]          = Eval[A]
  type Err[A]           = EitherT[Safe, String, A]
  type Effect[A]        = WriterT[Err, String, A]
  type StreamTEffect[A] = StreamT[Effect, A]

  implicit val stringArb = Arbitrary(Gen.oneOf("", "a", "b", "c"))

  implicit def arbEff[A: Arbitrary]: Arbitrary[Effect[A]] =
    Arbitrary(for {
      s <- Arbitrary.arbitrary[String]
      a <- Arbitrary.arbitrary[A]
    } yield WriterT[Err, String, A](EitherT.liftF(Eval.now(s -> a))))

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

  implicit val a: Arbitrary[Sync.Type] = Arbitrary[Sync.Type](
    Gen.oneOf(Seq(Type.Delay, Type.Blocking, Type.InterruptibleMany, Type.InterruptibleOnce))
  )
  // TODO this was not required to CE2, whats this for?
  implicit val x: StreamTEffect[Boolean] => Prop = (x: StreamTEffect[Boolean]) => Prop(true)

  test("StreamT.SyncLaws") { checkProps(SyncTests[StreamTEffect].sync[Int, Int, String].props) }
}

trait LowPriorityDerivations {

  implicit def arbFunctionK[K[_]]: Arbitrary[K ~> K] =
    Arbitrary(Gen.const(FunctionK.id[K]))

}
