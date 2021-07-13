package org.scalacheck

import java.util.concurrent.TimeoutException

import cats.data._
import cats.implicits._
import cats.laws.discipline.MonadTests
import cats.mtl.laws.discipline.MonadStateTests
import cats.tests.CatsSuite
import cats.{Defer, Eq, Monad}
import coop.rchain.metrics.Metrics
import coop.rchain.models.Expr.ExprInstance.{GBool, GInt, GString}
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar, Wildcard}
import coop.rchain.models.Var.WildcardMsg
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.accounting.{Cost, CostAccounting}
import coop.rchain.rholang.interpreter.{Interpreter, PrettyPrinter, Runtime, TestRuntime}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalacheck.rng.Seed
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.annotation.tailrec
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

class SubSpec extends FlatSpec with Matchers with PropertyChecks {

  behavior of "StateGen"

  def print(e: New): String = PrettyPrinter().buildString(e)

  import GenInstances._

  type BindCount     = Int
  type VarCount      = Int
  type Size          = Int
  type Env           = (BindCount, VarCount, Size)
  type EnvT[F[_], A] = ReaderT[F, Env, A]
  type ArbEnv[A]     = ArbF[EnvT, A]

  object ArbEnv extends GenericArb[EnvT] {
    override def defer                               = Defer[EnvT[Gen, ?]]
    override def monad                               = Monad[EnvT[Gen, ?]]
    override def liftF[A](gen: Gen[A]): EnvT[Gen, A] = ReaderT.liftF(gen)
    def ask: EnvT[Gen, Env]                          = ReaderT.ask[Gen, Env]
    def askBindCount: EnvT[Gen, BindCount]           = ReaderT.ask[Gen, Env].map(_._1)
    def askVarCount: EnvT[Gen, VarCount]             = ReaderT.ask[Gen, Env].map(_._2)
    def askSize: EnvT[Gen, Size]                     = ReaderT.ask[Gen, Env].map(_._3)
  }

  case class Shape(breadth: Int, depths: List[Size]) {
    assert(breadth == depths.length)
  }

  val setSize      = (size: Size) => (env: Env) => (env._1, env._2, size)
  val setBindCount = (bindCount: BindCount) => (env: Env) => (bindCount, env._2, env._3)
  val setVarCount  = (varCount: VarCount) => (env: Env) => (env._1, varCount, env._3)
  val decreaseSize = (env: Env) => (env._1, env._2, env._3 - 1)
  val addFreeVars = (freeCount: Int) =>
    (env: Env) => (env._1 + freeCount, env._2 + freeCount, env._3)

  implicit val arbFExpr: ArbF[EnvT, Expr] = ArbF[EnvT, Expr](Defer[EnvT[Gen, ?]].defer {
    val genInt: Gen[GInt]       = Gen.chooseNum(-5, 5).map(i => GInt(i.toLong))
    val genBool: Gen[GBool]     = Gen.oneOf(GBool(true), GBool(false))
    val genString: Gen[GString] = Gen.alphaStr.map(GString)
    ArbEnv.liftF(
      Gen
        .oneOf(
          genInt,
          genString,
          genBool
        )
        .map(Expr(_))
    )
  })

  implicit val arbFSend: ArbF[EnvT, Send] = ArbF[EnvT, Send](Defer[EnvT[Gen, ?]].defer {
    for {
      name <- genName
      expr <- ArbF.arbF[EnvT, Expr]
    } yield Send(chan = EVar(BoundVar(name)), data = List(expr))
  })

  implicit val arbFReceiveBind: ArbF[EnvT, ReceiveBind] =
    ArbF[EnvT, ReceiveBind](Defer[EnvT[Gen, ?]].defer {
      for {
        name                 <- genName
        r                    <- genPattern(name)
        (pattern, freeCount) = r
      } yield
        ReceiveBind(patterns = List(pattern), source = EVar(BoundVar(name)), freeCount = freeCount)
    })

  implicit val arbFReceive: ArbF[EnvT, Receive] = ArbF[EnvT, Receive](Defer[EnvT[Gen, ?]].defer {
    for {
      bind <- ArbF.arbF[EnvT, ReceiveBind]
      matchGen = ReaderT
        .local(addFreeVars(bind.freeCount) andThen decreaseSize)(ArbF.arbF[EnvT, Match])
        .asPar()

      parGen = ReaderT.local(addFreeVars(bind.freeCount) andThen decreaseSize)(ArbF.arbF[EnvT, Par])

      body         <- frequency((3, parGen), (1, matchGen))
      isPersistent <- ArbEnv.liftF(Gen.oneOf(true, false))
    } yield Receive(binds = List(bind), body = body, persistent = isPersistent)
  })

  implicit val arbFMatch: ArbF[EnvT, Match] = ArbF[EnvT, Match](Defer[EnvT[Gen, ?]].defer {
    for {
      target <- genName
      par    <- ReaderT.local(decreaseSize)(ArbF.arbF[EnvT, Par])
      // TODO: Add more match cases
      wildcardCase = MatchCase(pattern = EVar(Wildcard(WildcardMsg())), source = par)
    } yield Match(target = EVar(BoundVar(target)), cases = List(wildcardCase))
  })

  implicit val arbFPar: ArbF[EnvT, Par] = ArbF[EnvT, Par](Defer[EnvT[Gen, ?]].defer {
    for {
      size <- ArbEnv.askSize
      par <- if (size > 0) {
              for {
                // Split size between receives, sends and news
                sizes     <- splitSize(3, size)
                nReceives = sizes.head
                nSends    = sizes(1)
                nNews     = sizes(2)

                receives  <- genShaped[Receive](nReceives)
                sends     <- genShaped[Send](nSends)
                bindCount <- ArbEnv.liftF(Gen.chooseNum(1, 3))
                varCount  <- ArbEnv.askVarCount
                news <- ReaderT.local(
                         setBindCount(bindCount) andThen setVarCount(bindCount + varCount)
                       )(genShaped[New](nNews))
              } yield Par(sends = sends, receives = receives, news = news)
            } else nil
    } yield par
  })

  implicit val arbFNew: ArbF[EnvT, New] = ArbF[EnvT, New](Defer[EnvT[Gen, ?]].defer {
    for {
      bindCount <- ArbEnv.askBindCount
      par       <- ArbF.arbF[EnvT, Par]
    } yield New(bindCount = bindCount, p = par)
  })

  private def splitSize(chunks: Int, size: Size): EnvT[Gen, List[Size]] =
    for {
      boundaries <- if (chunks < size)
                     ArbEnv
                       .liftF(Gen.pick(chunks - 1, 0 until size))
                       .map(0 +: _ :+ size)
                       .map(_.sorted)
                   else
                     ArbEnv
                       .liftF(Gen.pick(size - 1, 0 until size))
                       .map(0 +: _ :+ size)
                       .map(_.sorted)
                       .map(List.fill(chunks - size)(0) ++ _)
      sizes = boundaries.sliding(2).map(pair => pair(1) - pair.head)
    } yield Random.shuffle(sizes.toList)

  private def genShaped[T](size: Size)(implicit arbFT: ArbF[EnvT, T]): EnvT[Gen, List[T]] =
    if (size > 0) {
      for {
        shape <- genShape(size)
        listOfT <- shape.depths
                    .map(d => ReaderT.local(setSize(d))(ArbF.arbF[EnvT, T]))
                    .sequence
      } yield listOfT
    } else emptyList[T]

  // Decides how given size is split up between breadth and depth
  private def genShape(size: Size): EnvT[Gen, Shape] = ArbEnv.liftF(
    for {
      breadth  <- Gen.chooseNum(1, size)
      leftover = size - breadth
      depths   = split(List.fill(leftover)(1), breadth).map(_.sum)
    } yield Shape(breadth, depths)
  )

  private def genName: EnvT[Gen, Int] =
    for {
      varCount <- ArbEnv.askVarCount
      name     <- ArbEnv.liftF(Gen.chooseNum(0, varCount - 1))
    } yield name

  private def genPattern(name: BindCount): EnvT[Gen, (Par, Int)] =
    ArbEnv.liftF(Gen.const((EVar(FreeVar(0)), 1)))

  private def frequency[T](gs: (Int, EnvT[Gen, T])*): EnvT[Gen, T] = {
    def zip(listT: Seq[T], ints: Seq[Int]): List[(Int, Gen[T])] =
      ints.zip(listT.map(t => Gen.const(t))).toList
    val sequenced   = gs.map { case (_, envT) => envT }.toList.sequence
    val frequencies = gs.map { case (i, _) => i }
    sequenced.flatMapF(listT => Gen.frequency(zip(listT, frequencies): _*))
  }

  // Taken from: https://stackoverflow.com/questions/40958670/split-a-list-into-a-fixed-number-of-random-sized-sub-lists
  private def split[T](list: List[T], chunks: Int): List[List[T]] = {
    @tailrec
    def split(list: List[T], chunks: Int, size: Int, result: List[List[T]]): List[List[T]] =
      if (chunks == 0) result
      else if (chunks == 1) list +: result
      else {
        val avg    = size / chunks
        val rand   = (1.0 + Random.nextGaussian / 3) * avg
        val index  = (rand.toInt max 1) min (size - chunks)
        val (h, t) = list splitAt index
        split(t, chunks - 1, size - index, h +: result)
      }
    split(list, chunks, list.size, Nil).reverse
  }

  private def nil: EnvT[Gen, Par] = ArbEnv.liftF(Gen.const(Par()))

  private def emptyList[T]: EnvT[Gen, List[T]] = ArbEnv.liftF(Gen.const(List[T]()))

  implicit class RichMatch(val a: EnvT[Gen, Match]) {
    def asPar(): EnvT[Gen, Par] = a.map(m => Par(matches = List(m)))
  }

  case class ValidExp(e: New)

  implicit def validExp(implicit ev: ArbEnv[New]): Arbitrary[ValidExp] =
    Arbitrary(
      // Run with 5 names introduced by initial `new` and a size of 10
      ev.arb.run((5, 5, 10)).map(ValidExp)
    )

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    PropertyCheckConfiguration(sizeRange = 200, minSize = 50, minSuccessful = 1000)

  it should "work" in {

    forAll { v: ValidExp =>
      println(print(v.e))
      println("=============")
    }

  }

  it should "execute without errors" in {
    forAll { v: ValidExp =>
      try {
        success(print(v.e)).runSyncUnsafe(3.seconds)
      } catch {
        case _: TimeoutException => succeed
      }
    }
  }

  def success(term: String): Task[Assertion] = {
    implicit val logF: Log[Task]            = new Log.NOPLog[Task]
    implicit val noopMetrics: Metrics[Task] = new Metrics.MetricsNOP[Task]

    for {
      runtime <- TestRuntime.create[Task, Task.Par]()
      _       <- runtime.reducer.setPhlo(Cost.UNSAFE_MAX)
      _       <- Runtime.injectEmptyRegistryRoot[Task](runtime.space, runtime.replaySpace)
      cost    <- CostAccounting.emptyCost[Task]
      res     <- {
        implicit val c = cost
        Interpreter[Task].evaluate(runtime, term)
      }
    } yield assert(res.errors.isEmpty)
  }
}

object EqInstances {
  def sampledCogenEq[A](trials: Int)(implicit ev: Arbitrary[A]): Eq[Cogen[A]] =
    new Eq[Cogen[A]] {
      def eqv(x: Cogen[A], y: Cogen[A]): Boolean = {
        val gen: Gen[A]            = ev.arbitrary
        val params: Gen.Parameters = Gen.Parameters.default
        // Loop Function which checks that the seeds from perturbing
        // given cogens create equivalent seeds for x iterations
        // to consider them equal
        def loop(count: Int, retries: Int, seed: Seed): Boolean =
          if (retries <= 0) sys.error("Generator Function Failed")
          else if (count <= 0) true // If make it through count all equal these are equal
          else {
            val rx = gen.doApply(params, seed) // Get Value
            rx.retrieve.fold(
              loop(count, retries - 1, rx.seed) // Loop As Necessary
            ) { a =>
              val seed = Seed.random
              val sx   = x.perturb(seed, a)
              val sy   = y.perturb(seed, a)
              if (sx != sy) false // If they are not equivalent
              else loop(count - 1, retries, rx.seed) // Another trial
            }
          }
        // Initiate Loop
        loop(trials, trials, Seed.random)
      }
    }
  def sampledGenEq[A: Eq](trials: Int): Eq[Gen[A]] = Eq.instance[Gen[A]] {
    case (x, y) =>
      val params = Gen.Parameters.default
      def loop(count: Int, seed: Seed): Boolean =
        if (count <= 0) true
        else {
          // Leave this so the inequality creates the eq
          val tx = Try(x.doApply(params, seed))
          val ty = Try(y.doApply(params, seed))
          (tx, ty) match {
            case (Failure(_), Failure(_)) =>
              // They both failed, good, keep going
              loop(count - 1, Seed.random)
            case (Success(rx), Success(ry)) =>
              if (rx.retrieve != ry.retrieve) false
              else loop(count - 1, seed.next)
            case _ =>
              false
          }
        }
      loop(trials, Seed.random)
  }

}

trait ScalaCheckSetup {
//
  implicit def genEq[A: Eq]: Eq[Gen[A]] =
    EqInstances.sampledGenEq(1000)

  implicit def cogenEq[A: Arbitrary]: Eq[Cogen[A]] =
    EqInstances.sampledCogenEq(1000)

  implicit lazy val arbitrarySeed: Arbitrary[Seed] =
    Arbitrary(Gen.choose(Long.MinValue, Long.MaxValue).map(n => Seed(n)))

  implicit lazy val cogenSeed: Cogen[Seed] =
    Cogen[Long].contramap(_.long._1)

//  implicit def arbitraryNonEmptyList[A: Arbitrary]: Arbitrary[NonEmptyList[A]] =
//    Arbitrary(
//      (Arbitrary.arbitrary[A], Arbitrary.arbitrary[List[A]]).mapN(NonEmptyList(_, _))
//    )

  // Better Arbitrary Gen
  implicit def arbitraryGen[A: Arbitrary]: Arbitrary[Gen[A]] = {
    val simple = Gen.const(Arbitrary.arbitrary[A])
    val complex = Arbitrary.arbitrary[Seed => Seed].map { f =>
      Gen.gen((params, seed) => Arbitrary.arbitrary[A].doApply(params, f(seed)))
    }
    Arbitrary(Gen.oneOf(simple, complex))
  }
  //
  //  implicit def arbitraryCogen[A: Cogen]: Arbitrary[Cogen[A]] =
  //    Arbitrary(Arbitrary.arbitrary[Seed => Seed].map { f =>
  //      Cogen((seed, a) => f(Cogen[A].perturb(seed, a)))
  //    })
}

class GenLaws extends CatsSuite with ScalaCheckSetup {
  import GenInstances._

  type SGen[A] = StateT[Gen, Int, A]

  implicit def arbFAStaetT[A: Arbitrary]: Arbitrary[SGen[A]] =
    Arbitrary[SGen[A]](
      Arbitrary
        .arbitrary[A]
        .flatMap(
          a =>
            Gen.oneOf[SGen[A]](
              StateT.get[Gen, Int].as(a),
              StateT.modify[Gen, Int](_ + 1).as(a),
              StateT.modify[Gen, Int](_ - 1).as(a),
              StateT.modify[Gen, Int](_ * -1).as(a)
            )
        )
    )

  implicit def eqFA[A: Eq]: Eq[SGen[A]] =
//    implicit def eqGenA: Eq[Gen[A]] = EqInstances.sampledGenEq(1000)
    Eq.by(_.run(0))

  // Tests Alternative
//  checkAll("Gen", AlternativeTests[Gen].alternative[Int, Int, Int])
  // Tests Monad
  checkAll("Gen", MonadTests[Gen].monad[Int, Int, Int])
  checkAll("Monad StateT Gen", MonadTests[SGen].monad[Int, Int, Int])

  import cats.mtl.implicits._

  checkAll("MonadStaete", MonadStateTests[SGen, Int].monadState[Int])

  // Tests FunctorFilter
  //  checkAll("Gen.FunctorFilterLaws", FunctorFilterTests[Gen].functorFilter[Int, Int, Int])
  //
  //  // Tests Monoid for Inner Given Monoid
  //  checkAll("Gen[String]", MonoidTests[Gen[String]].monoid)
  //  // Tests Low Priority Semigroup
  //  checkAll("Gen[NonEmptyList[Int]]", SemigroupTests[Gen[NonEmptyList[Int]]].semigroup)
}

object GenInstances {
  implicit val genInstances: Monad[Gen] with Defer[Gen] = new Monad[Gen] with Defer[Gen] {
    // Members declared in cats.Applicative
    override def pure[A](x: A): Gen[A] =
      Gen.const(x)

    // Members declared in cats.FlatMap
    override def flatMap[A, B](fa: Gen[A])(f: A => Gen[B]): Gen[B] =
      fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => Gen[Either[A, B]]): Gen[B] =
      GenShims.tailRecM(a)(f)

    override def defer[A](fa: => Gen[A]): Gen[A] = {
      import org.scalacheck.derive.GenExtra._
      Gen.delay(fa).failOnStackOverflow
    }
  }

}

object GenShims {

  type P = Gen.Parameters

  import Gen.{R, gen, r}

  def tailRecM[A, B](a0: A)(fn: A => Gen[Either[A, B]]): Gen[B] = {

    @tailrec
    def tailRecMR(a: A, seed: Seed, labs: Set[String])(fn: (A, Seed) => R[Either[A, B]]): R[B] = {
      val re       = fn(a, seed)
      val nextLabs = labs | re.labels
      re.retrieve match {
        case None           => r(None, re.seed).copy(l = nextLabs)
        case Some(Right(b)) => r(Some(b), re.seed).copy(l = nextLabs)
        case Some(Left(a))  => tailRecMR(a, re.seed, nextLabs)(fn)
      }
    }

    // This is the "Reader-style" appoach to making a stack-safe loop:
    // we put one outer closure around an explicitly tailrec loop
    gen[B] { (p: P, seed: Seed) =>
      tailRecMR(a0, seed, Set.empty) { (a, seed) =>
        fn(a).doApply(p, seed)
      }
    }
  }
}
