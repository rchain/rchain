package coop.rchain.rholang.interpreter

import cats.Parallel
import cats.effect.{IO, Sync}
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.models.Expr.ExprInstance.{EVarBody, GString}
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.RhoType.RhoName
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.RhoRuntime.{RhoISpace, RhoTuplespace}
import coop.rchain.rholang.interpreter.accounting._
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage._
import coop.rchain.rspace._
import coop.rchain.rspace.internal.{Datum, Row}
import coop.rchain.shared.Log
import coop.rchain.store.InMemoryStoreManager
import org.scalactic.TripleEqualsSupport
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.concurrent.duration._
import cats.effect.Ref
import cats.effect.unsafe.implicits.global

class CostAccountingReducerTest extends AnyFlatSpec with Matchers with TripleEqualsSupport {

  implicit val noopSpan: Span[IO]   = Span.noop
  implicit val metrics: Metrics[IO] = new Metrics.MetricsNOP[IO]
  implicit val ms: Metrics.Source   = Metrics.BaseSource

  behavior of "Cost accounting in Reducer"

  def createDispatcher[M[_]: Sync: Parallel: _cost](
      tuplespace: RhoTuplespace[M],
      dispatchTable: => Map[Long, Seq[ListParWithRandom] => M[Unit]],
      urnMap: Map[String, Par]
  ): (Dispatch[M, ListParWithRandom, TaggedContinuation], Reduce[M]) = {
    val emptyMergeableRef = Ref.unsafe[M, Set[Par]](Set.empty)
    val dummyMergeableTag = RhoName(Array[Byte]())
    RholangAndScalaDispatcher(
      tuplespace,
      dispatchTable,
      urnMap,
      emptyMergeableRef,
      dummyMergeableTag
    )
  }

  it should "charge for the successful substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val substTerm         = term(Expr(GString("1")))
    val termCost          = Chargeable[Par].cost(substTerm)
    val initCost          = Cost(1000)

    (for {
      cost <- CostAccounting.initialCost[IO](initCost)
      res <- {
        implicit val c = cost
        Substitute.charge(IO(substTerm), Cost(10000)).attempt
      }
      _         = assert(res === Right(substTerm))
      finalCost <- cost.get
      _         = assert(finalCost === (initCost - Cost(termCost)))
    } yield ())
      .timeout(5.seconds)
      .unsafeRunSync()
  }

  it should "charge for failed substitution" in {
    val term: Expr => Par = expr => Par(bundles = Seq(Bundle(Par(exprs = Seq(expr)))))
    val varTerm           = term(Expr(EVarBody(EVar(Var(FreeVar(0))))))
    val originalTermCost  = Chargeable[Par].cost(varTerm)
    val initCost          = Cost(1000)

    (for {
      cost <- CostAccounting.initialCost[IO](initCost)
      res <- {
        implicit val c = cost
        Substitute
          .charge(IO.raiseError[Par](new RuntimeException("")), Cost(originalTermCost))
          .attempt
      }
      _         = assert(res.isLeft)
      finalCost <- cost.get
      _         = assert(finalCost === (initCost - Cost(originalTermCost)))
    } yield ())
      .timeout(5.seconds)
      .unsafeRunSync()
  }

  it should "stop if OutOfPhloError is returned from RSpace" in {

    val iSpace = new ISpaceStub[
      IO,
      Par,
      BindPattern,
      ListParWithRandom,
      TaggedContinuation
    ] {
      override def produce(
          channel: Par,
          data: ListParWithRandom,
          persist: Boolean
      ): IO[
        Option[
          (ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[Par, ListParWithRandom]])
        ]
      ] =
        IO.raiseError[Option[
          (ContResult[Par, BindPattern, TaggedContinuation], Seq[Result[Par, ListParWithRandom]])
        ]](OutOfPhlogistonsError)
    }

    implicit val rand        = Blake2b512Random.defaultRandom
    implicit val cost        = CostAccounting.initialCost[IO](Cost(1000)).unsafeRunSync()
    val (_, chargingReducer) = createDispatcher(iSpace, Map.empty, Map.empty)
    val send                 = Send(Par(exprs = Seq(GString("x"))), Seq(Par()))
    val test                 = chargingReducer.inj(send).attempt.unsafeRunSync()
    assert(test === Left(OutOfPhlogistonsError))
  }

  it should "stop interpreter threads as soon as deploy runs out of phlo" ignore {
    // Given
    // new x in { @x!("a") | @x!("b") }
    // and not enough phlos to reduce successfully
    // only one of the branches we should be persisted in the tuplespace

    val channel: Par = GPrivateBuilder("x")

    val a: Par = GString("a")
    val b: Par = GString("b")
    val program =
      Par(sends = Seq(Send(channel, Seq(a)), Send(channel, Seq(b))))

    implicit val rand          = Blake2b512Random(Array.empty[Byte])
    implicit val logF: Log[IO] = Log.log[IO]
    implicit val kvm           = InMemoryStoreManager[IO]()

    def testImplementation(pureRSpace: RhoISpace[IO]): IO[
      (
          Either[Throwable, Unit],
          Map[Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]]
      )
    ] = {

      implicit val cost = CostAccounting.emptyCost[IO].unsafeRunSync()

      lazy val (_, reducer) =
        createDispatcher(pureRSpace, Map.empty, Map.empty)

      def plainSendCost(p: Par): Cost = {
        val storageCost      = accounting.storageCostProduce(channel, ListParWithRandom(Seq(p)))
        val substitutionCost = Cost(Chargeable[Par].cost(channel)) + Cost(Chargeable[Par].cost(p))
        substitutionCost + storageCost + SEND_EVAL_COST
      }
      val sendACost = plainSendCost(a)
      val sendBCost = plainSendCost(b)

      val initPhlos = sendACost + sendBCost - SEND_EVAL_COST - Cost(1)

      for {
        _           <- cost.set(initPhlos)
        result      <- reducer.inj(program).attempt
        mappedSpace <- pureRSpace.toMap
      } yield (result, mappedSpace)
    }

    def data(p: Par, rand: Blake2b512Random) = Row(
      List(Datum.create(channel, ListParWithRandom(Seq(p), rand), false)),
      List()
    )

    def stored(
        map: Map[Seq[Par], Row[BindPattern, ListParWithRandom, TaggedContinuation]],
        p: Par,
        rand: Blake2b512Random
    ): Boolean =
      map.get(List(channel)) === Some(data(p, rand))

    (for {
      res           <- mkRhoISpace[IO].flatMap(testImplementation)
      (result, map) = res
      _             = assert(result === Left(OutOfPhlogistonsError))
      _             = assert(stored(map, a, rand.splitByte(0)) || stored(map, b, rand.splitByte(1)))
    } yield ())
      .timeout(5.seconds)
      .unsafeRunSync()

  }
}
