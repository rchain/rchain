package coop.rchain.rholang.interpreter.storage

import cats.Foldable
import cats.effect.{Resource, Sync}
import cats.mtl.FunctorRaise
import cats.implicits.catsStdInstancesForList
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.mtl.implicits._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.metrics
import coop.rchain.metrics.Metrics
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.Runtime.{RhoISpace, RhoPureSpace}
import coop.rchain.rholang.interpreter.accounting.{CostAccounting, _}
import coop.rchain.rholang.interpreter.errors
import coop.rchain.rholang.interpreter.errors.{InterpreterError, OutOfPhlogistonsError}
import coop.rchain.rholang.interpreter.storage.ChargingRSpace._
import coop.rchain.rholang.interpreter.storage.ChargingRSpaceTest.{ChargingRSpace, _}
import coop.rchain.rholang.interpreter.storage.implicits.matchListPar
import coop.rchain.rspace.{Match, _}
import coop.rchain.shared.Log
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.{fixture, Matchers, Outcome}

import scala.collection.immutable
import scala.concurrent.duration._

class ChargingRSpaceTest extends fixture.FlatSpec with TripleEqualsSupport with Matchers {

  behavior of "ChargingRSpace"

  it should "charge for storing data in tuplespace" in { fixture =>
    val TestFixture(chargingRSpace, cost) = fixture
    val channels                          = channelsN(1)
    val patterns                          = patternsN(1)
    val cont                              = continuation()
    val storageCost                       = ChargingRSpace.storageCostConsume(channels, patterns, cont)
    val minimumPhlos                      = storageCost

    val test = for {
      _         <- cost.set(minimumPhlos)
      _         <- chargingRSpace.consume(channels, patterns, cont, false)
      phlosLeft <- cost.get
      _         = phlosLeft.value shouldBe 0
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund if data doesn't stay in tuplespace" in { fixture =>
    val TestFixture(chargingRSpace, cost) = fixture
    val channels                          = channelsN(1)
    val patterns                          = patternsN(1)
    val cont                              = continuation()
    val consumeStorageCost                = ChargingRSpace.storageCostConsume(channels, patterns, cont)
    val data                              = NilPar
    val matchCost                         = actualMatchCost(patterns, Seq(data)).runSyncUnsafe()
    val produceStorageCost                = ChargingRSpace.storageCostProduce(channels.head, data)
    val minimumPhlos                      = produceStorageCost + consumeStorageCost + matchCost

    val test = for {
      _                 <- cost.set(minimumPhlos)
      _                 <- chargingRSpace.produce(channels.head, data, false)
      phlosAfterProduce <- cost.inspect(identity)
      _                 = phlosAfterProduce shouldBe (minimumPhlos - produceStorageCost)
      res               <- chargingRSpace.consume(channels, patterns, cont, false)
      phlosLeft         <- cost.inspect(identity)
      _                 = phlosLeft.value shouldBe (consumeStorageCost.value + produceStorageCost.value)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "fail with OutOfPhloError when deploy runs out of it" in { fixture =>
    val TestFixture(chargingRSpace, cost) = fixture
    val channel                           = channelsN(1).head
    val data                              = NilPar
    val produceStorageCost                = ChargingRSpace.storageCostProduce(channel, data)

    val test = for {
      _ <- cost.set(produceStorageCost - Cost(1))
      _ <- chargingRSpace.produce(channel, data, false)
    } yield ()

    val outOfPhloTest = test.attempt.runSyncUnsafe(1.second)
    assert(outOfPhloTest === Left(OutOfPhlogistonsError))

    val costTest = cost.get.runSyncUnsafe(1.second)
    assert(costTest.value === -1)
  }

  it should "charge COMM on a join properly when parts of the join are deployed separately" in {
    // first deploy:
    // for(x <- @x; y <- @y) { P }
    // second deploy:
    // @x!(data)
    // third deploy:
    // @y!(data)
    // last deployment should be refunded with the cost of storing two previous deployments
    fixture =>
      val TestFixture(chargingRSpace, cost) = fixture
      val channels                          = channelsN(2)
      val patterns                          = patternsN(2)
      val cont                              = continuation()
      val data                              = NilPar
      val matchCost                         = actualMatchCost(patterns, Seq(data, data)).runSyncUnsafe()
      val firstProdCost                     = ChargingRSpace.storageCostProduce(channels(0), data)
      val secondProdCost                    = ChargingRSpace.storageCostProduce(channels(1), data)
      val joinCost                          = ChargingRSpace.storageCostConsume(channels, patterns, cont)
      val minimumPhlos                      = firstProdCost + secondProdCost + joinCost + matchCost

      val test = for {
        _                   <- cost.set(minimumPhlos)
        _                   <- chargingRSpace.consume(channels, patterns, cont, false)
        phlosAfterConsume   <- cost.inspect(identity)
        _                   = phlosAfterConsume shouldBe (minimumPhlos - joinCost)
        res                 <- chargingRSpace.produce(channels(0), data, false)
        phlosAfterFirstSend <- cost.inspect(identity)
        _                   = phlosAfterFirstSend shouldBe (phlosAfterConsume - firstProdCost)
        //FIXME(mateusz.gorski): This should be expected cost but rspace is doing match on join even if only one of the channels has data
        //_                 = phlosAfterFirstSend shouldBe (phlosAfterConsume - firstProdCost + RSPACE_MATCH_COST)
        _         <- chargingRSpace.produce(channels(1), data, false)
        phlosLeft <- cost.inspect(identity)
        _         = phlosLeft.value shouldBe (minimumPhlos - matchCost).value
      } yield ()

      test.runSyncUnsafe(1.second)
  }

  it should "not charge for storage if linear terms create a COMM" in { fixture =>
    // for(x <- @x) | @x!(10)
    // we should not charge for storing any of the terms
    val TestFixture(chargingRSpace, cost) = fixture
    val channels                          = channelsN(1)
    val patterns                          = patternsN(1)
    val cont                              = continuation()

    val data      = ListParWithRandom().withPars(Vector(GInt(1)))
    val matchCost = actualMatchCost(patterns, Seq(data)).runSyncUnsafe()
    val consumeStorageCost = storageCostConsume(
      channels,
      patterns,
      cont
    )
    val produceStorageCost = storageCostProduce(channels.head, data)

    val initPhlos = consumeStorageCost + produceStorageCost + matchCost

    val test = for {
      _         <- cost.set(initPhlos)
      _         <- chargingRSpace.consume(channels, patterns, cont, false)
      _         <- chargingRSpace.produce(channels.head, data, false)
      phlosLeft <- cost.get
      _         = phlosLeft.value shouldBe (consumeStorageCost + produceStorageCost).value
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "charge for storing persistent produce that create a COMM" in { fixture =>
    // for(x <- @x) { P } | @x!!(100)
    // we should charge for storing non-linear produce
    val TestFixture(chargingRSpace, cost) = fixture
    val channels                          = channelsN(1)
    val pattern                           = BindPattern(Vector(EVar(FreeVar(0))))
    val cont                              = continuation()

    val data        = ListParWithRandom().withPars(Vector(GInt(1)))
    val matchCost   = actualMatchCost(Seq(pattern), Seq(data)).runSyncUnsafe()
    val produceCost = storageCostProduce(channels.head, data)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- cost.set(initPhlos)
      _         <- chargingRSpace.consume(channels, List(pattern), cont, false)
      _         <- chargingRSpace.produce(channels.head, data, true)
      phlosLeft <- cost.get
      _         = phlosLeft.value shouldBe (initPhlos - produceCost - matchCost).value
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "charge for storing persistent consume that create a COMM" in { fixture =>
    // for(x <= @x) { P } | @x!(100)
    // we should charge for storing non-linear continuation
    val TestFixture(chargingRSpace, cost) = fixture
    val channels                          = channelsN(1)
    val pattern                           = patternsN(1)
    val cont                              = continuation()

    val data        = ListParWithRandom().withPars(Vector(GInt(1)))
    val matchCost   = actualMatchCost(pattern, Seq(data)).runSyncUnsafe()
    val consumeCost = storageCostConsume(channels, pattern, cont)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- cost.set(initPhlos)
      _         <- chargingRSpace.consume(channels, pattern, cont, true)
      _         <- chargingRSpace.produce(channels.head, data, false)
      phlosLeft <- cost.get
      _         = phlosLeft.value shouldBe (initPhlos - consumeCost - matchCost).value
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund for linear data in join" in { fixture =>
    // idea for the test is that we have persistent and non persistent produce in first deploy:
    // @"x"!!(1) | @"y"!(10)
    // and consume on joined channels in another:
    // for(x <- @"x"; y <- @"y") { … }
    // In this case we shouldn't charge for storing consume and refund for removing produce on @"y"

    val TestFixture(chargingRSpace, cost) = fixture
    val List(x, y)                        = channelsN(2)
    val patterns                          = patternsN(2)
    val cont                              = continuation()

    val dataX        = ListParWithRandom().withPars(Vector(GInt(1)))
    val dataY        = ListParWithRandom().withPars(Vector(GInt(10)))
    val matchCost    = actualMatchCost(patterns, Seq(dataX, dataY)).runSyncUnsafe()
    val produceYCost = ChargingRSpace.storageCostProduce(y, dataY)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- cost.set(initPhlos)
      _         <- chargingRSpace.produce(x, dataX, persist = true)
      _         <- chargingRSpace.produce(y, dataY, persist = false)
      _         <- cost.modify(_ => initPhlos)
      _         <- chargingRSpace.consume(List(x, y), patterns, cont, false)
      phlosLeft <- cost.get
      _         = phlosLeft.value shouldBe (initPhlos + produceYCost - matchCost).value
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund for removing consume" in { fixture =>
    // first deploy:
    // for(x <- @x) { P }
    // second deploy:
    // @x!(100)
    // we should refund for removing continuation from tuplespace
    val TestFixture(chargingRSpace, cost) = fixture
    val List(x)                           = channelsN(1)
    val patterns                          = patternsN(1)
    val cont                              = continuation()

    val data        = ListParWithRandom().withPars(Vector(GInt(1)))
    val matchCost   = actualMatchCost(patterns, Seq(data)).runSyncUnsafe()
    val consumeCost = ChargingRSpace.storageCostConsume(List(x), patterns, cont)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- cost.set(initPhlos)
      _         <- chargingRSpace.consume(List(x), patterns, cont, false)
      _         <- cost.modify(_ => initPhlos)
      _         <- chargingRSpace.produce(x, data, persist = false)
      phlosLeft <- cost.get
      _         = phlosLeft.value shouldBe (initPhlos + consumeCost - matchCost).value
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund for removing produce" in { fixture =>
    // first deploy:
    // @x!(100)
    // second deploy:
    // for(x <- @x) { P }
    // we should refund for removing @x!(100) from tuplespace
    val TestFixture(chargingRSpace, cost) = fixture
    val List(x)                           = channelsN(1)
    val patterns                          = patternsN(1)
    val cont                              = continuation()

    val data        = ListParWithRandom().withPars(Vector(GInt(1)))
    val matchCost   = actualMatchCost(patterns, Seq(data)).runSyncUnsafe()
    val produceCost = ChargingRSpace.storageCostProduce(x, data)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- cost.set(initPhlos)
      _         <- chargingRSpace.produce(x, data, persist = false)
      _         <- cost.modify(_ => initPhlos)
      _         <- chargingRSpace.consume(List(x), patterns, cont, false)
      phlosLeft <- cost.get
      _         = phlosLeft.value shouldBe (initPhlos + produceCost - matchCost).value
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund for clearing tuplespace" in { fixture =>
    // first deploy:
    // @x!(100) | @y!(10) | for(x <- @x; y <- @y; z <- @z) { P }
    // second deploy:
    // @z!(1)
    // since second deploy triggers continuation we should refund with the cost of storing first deploy
    val TestFixture(chargingRSpace, cost) = fixture
    val List(x, y, z)                     = channelsN(3)
    val patterns                          = patternsN(3)
    val cont                              = continuation()

    val dataX        = ListParWithRandom().withPars(Vector(GInt(1)))
    val dataY        = ListParWithRandom().withPars(Vector(GInt(10)))
    val dataZ        = ListParWithRandom().withPars(Vector(GInt(100)))
    val matchCost    = actualMatchCost(patterns, Seq(dataX, dataY, dataZ)).runSyncUnsafe()
    val produceXCost = ChargingRSpace.storageCostProduce(x, dataX)
    val produceYCost = ChargingRSpace.storageCostProduce(y, dataY)
    val consumeCost  = ChargingRSpace.storageCostConsume(List(x, y, z), patterns, cont)

    val initPhlos = Cost(10000)

    val test = for {
      _         <- cost.set(initPhlos)
      _         <- chargingRSpace.produce(x, dataX, false)
      _         <- chargingRSpace.produce(y, dataY, false)
      _         <- chargingRSpace.consume(List(x, y, z), patterns, cont, false)
      _         <- cost.modify(_ => initPhlos)
      _         <- chargingRSpace.produce(z, dataZ, false)
      phlosLeft <- cost.get
      _         = phlosLeft.value shouldBe (initPhlos + produceXCost + produceYCost + consumeCost - matchCost).value
    } yield ()

    test.runSyncUnsafe(5.seconds)
  }

  override type FixtureParam = TestFixture

  override protected def withFixture(test: OneArgTest): Outcome = {
    val cost: _cost[Task] =
      loggingCost(CostAccounting.empty[Task].runSyncUnsafe(1.second), noOpCostLog)
    def mkChargingRspace(rhoISpace: RhoISpace[Task]): Task[ChargingRSpace] = {
      val s = implicitly[Sync[Task]]
      Task.delay(ChargingRSpace.pureRSpace(rhoISpace)(s, cost))
    }

    val chargingRSpaceResource =
      mkRhoISpace[Task]("rchain-charging-rspace-test-")
        .flatMap(rhoISpace => Resource.make(mkChargingRspace(rhoISpace))(_.close()))

    chargingRSpaceResource
      .use(chargingRSpace => Task.delay { test(TestFixture(chargingRSpace, cost)) })
      .runSyncUnsafe(10.seconds)
  }

  private def actualMatchCost(
      patterns: Seq[BindPattern],
      data: Seq[ListParWithRandom]
  ): Task[Cost] = {
    import cats.implicits._
    implicit val _cost: _cost[Task] =
      loggingCost(CostAccounting.of[Task](Cost(1000)).runSyncUnsafe(1.second), noOpCostLog)
    for {
      initPhlos <- _cost.inspect(identity)
      _ <- patterns.zip(data).toList.traverse {
            case (pattern, pars) => matchListPar[Task].get(pattern, pars)
          }
      phlosLeft <- _cost.inspect(identity)
    } yield initPhlos - phlosLeft
  }
}

object ChargingRSpaceTest {
  type ChargingRSpace = RhoPureSpace[Task]
  final case class TestFixture(chargingRSpace: ChargingRSpace, cost: _cost[Task])

  val NilPar                 = ListParWithRandom().withPars(Seq(Par()))
  val rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])

  def channelsN(n: Int): List[Par] =
    (1 to n).map(x => byteName(x.toByte)).toList

  private def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))

  def patternsN(n: Int): List[BindPattern] =
    (1 to n)
      .map(
        _ => BindPattern(Vector(EVar(Var(FreeVar(0)))), freeCount = 1)
      )
      .toList

  def continuation(
      par: Par = Par().withExprs(Seq(GInt(1))),
      r: Blake2b512Random = rand
  ): TaggedContinuation =
    TaggedContinuation(ParBody(ParWithRandom(par, r)))

  implicit val logF: Log[Task]            = new Log.NOPLog[Task]
  implicit val noopMetrics: Metrics[Task] = new metrics.Metrics.MetricsNOP[Task]
}
