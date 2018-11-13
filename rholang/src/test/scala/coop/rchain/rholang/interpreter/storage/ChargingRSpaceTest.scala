package coop.rchain.rholang.interpreter.storage

import java.nio.file.Files

import cats.effect.{Resource, Sync}
import com.google.protobuf.ByteString
import coop.rchain.catscontrib.TaskContrib._
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.Resources.mkRhoISpace
import coop.rchain.rholang.interpreter.Runtime.{RhoContext, RhoISpace, RhoPureSpace}
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccounting, _}
import coop.rchain.rholang.interpreter.errors
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.ChargingRSpaceTest.{ChargingRSpace, _}
import coop.rchain.rspace.history.Branch
import coop.rchain.rspace.{Match, _}
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.{fixture, Matchers, Outcome}
import coop.rchain.rholang.interpreter.storage.ChargingRSpace._

import scala.collection.immutable
import scala.concurrent.duration._

class ChargingRSpaceTest extends fixture.FlatSpec with TripleEqualsSupport with Matchers {

  behavior of "ChargingRSpace"

  it should "charge for storing data in tuplespace" in { fixture =>
    val TestFixture(chargingRSpace, costAlg) = fixture
    val channels                             = channelsN(1)
    val patterns                             = patternsN(1)
    val cont                                 = continuation()
    val storageCost                          = ChargingRSpace.storageCostConsume(channels, patterns, cont)
    val minimumPhlos                         = storageCost

    val test = for {
      _         <- costAlg.set(CostAccount(0, minimumPhlos))
      _         <- chargingRSpace.consume(channels, patterns, cont, false)
      phlosLeft <- costAlg.get()
      // we expect Cost = 1 because there will be no match
      // so we will pay only for the storage
      _ = phlosLeft.cost shouldBe Cost(0)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund if data doesn't stay in tuplespace" in { fixture =>
    val TestFixture(chargingRSpace, costAlg) = fixture
    val channels                             = channelsN(1)
    val patterns                             = patternsN(1)
    val cont                                 = continuation()
    val consumeStorageCost                   = ChargingRSpace.storageCostConsume(channels, patterns, cont)
    val data                                 = NilPar
    val produceStorageCost                   = ChargingRSpace.storageCostProduce(channels.head, data)
    val minimumPhlos                         = produceStorageCost + consumeStorageCost + RSPACE_MATCH_COST

    val test = for {
      _                 <- costAlg.set(CostAccount(0, minimumPhlos))
      _                 <- chargingRSpace.produce(channels.head, data, false)
      phlosAfterProduce <- costAlg.get()
      _                 = phlosAfterProduce.cost shouldBe (minimumPhlos - produceStorageCost)
      res               <- chargingRSpace.consume(channels, patterns, cont, false)
      phlosLeft         <- costAlg.get()
      _                 = phlosLeft.cost shouldBe (consumeStorageCost + produceStorageCost)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "fail with OutOfPhloError when deploy runs out of it" in { fixture =>
    val TestFixture(chargingRSpace, costAlg) = fixture
    val channel                              = channelsN(1).head
    val data                                 = NilPar
    val produceStorageCost                   = ChargingRSpace.storageCostProduce(channel, data)

    val test = for {
      _ <- costAlg.set(CostAccount(0, produceStorageCost - Cost(1)))
      _ <- chargingRSpace.produce(channel, data, false)
    } yield ()

    val outOfPhloTest = test.attempt.runSyncUnsafe(1.second)
    assert(outOfPhloTest === Left(OutOfPhlogistonsError))

    val costAlgTest = costAlg.get().runSyncUnsafe(1.second)
    assert(costAlgTest === CostAccount(1, Cost(-1)))
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
      val TestFixture(chargingRSpace, costAlg) = fixture
      val channels                             = channelsN(2)
      val patterns                             = patternsN(2)
      val cont                                 = continuation()
      val data                                 = NilPar
      val firstProdCost                        = ChargingRSpace.storageCostProduce(channels(0), data)
      val secondProdCost                       = ChargingRSpace.storageCostProduce(channels(1), data)
      val joinCost                             = ChargingRSpace.storageCostConsume(channels, patterns, cont)
      val minimumPhlos                         = firstProdCost + secondProdCost + joinCost + (RSPACE_MATCH_COST * 2)

      val test = for {
        _                   <- costAlg.set(CostAccount(0, minimumPhlos))
        _                   <- chargingRSpace.consume(channels, patterns, cont, false)
        phlosAfterConsume   <- costAlg.get()
        _                   = phlosAfterConsume.cost shouldBe (minimumPhlos - joinCost)
        res                 <- chargingRSpace.produce(channels(0), data, false)
        phlosAfterFirstSend <- costAlg.get()
        _                   = phlosAfterFirstSend.cost shouldBe (phlosAfterConsume - firstProdCost).cost
        //FIXME(mateusz.gorski): This should be expected cost but rspace is doing match on join even if only one of the channels has data
        //_                 = phlosAfterFirstSend.cost shouldBe (phlosAfterConsume - firstProdCost + RSPACE_MATCH_COST.cost).cost
        _         <- chargingRSpace.produce(channels(1), data, false)
        phlosLeft <- costAlg.get()
        _         = phlosLeft.cost shouldBe (minimumPhlos - (RSPACE_MATCH_COST * 2))
      } yield ()

      test.runSyncUnsafe(1.second)
  }

  it should "not charge for storage if linear terms create a COMM" in { fixture =>
    // for(x <- @x) | @x!(10)
    // we should not charge for storing any of the terms
    val TestFixture(chargingRSpace, costAlg) = fixture
    val channels                             = channelsN(1)
    val patterns                             = patternsN(1)
    val cont                                 = continuation()

    val data = ListParWithRandom().withPars(Vector(GInt(1)))
    val consumeStorageCost = storageCostConsume(
      channels,
      patterns,
      cont
    )
    val produceStorageCost = storageCostProduce(channels.head, data)

    val initPhlos = consumeStorageCost + produceStorageCost + RSPACE_MATCH_COST

    val test = for {
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.consume(channels, patterns, cont, false)
      _         <- chargingRSpace.produce(channels.head, data, false)
      phlosLeft <- costAlg.get()
      _         = phlosLeft.cost shouldBe (consumeStorageCost + produceStorageCost)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "charge for storing persistent produce that create a COMM" in { fixture =>
    // for(x <- @x) { P } | @x!!(100)
    // we should charge for storing non-linear produce
    val TestFixture(chargingRSpace, costAlg) = fixture
    val channels                             = channelsN(1)
    val pattern                              = BindPattern(Vector(EVar(FreeVar(0))))
    val cont                                 = continuation()

    val data        = ListParWithRandom().withPars(Vector(GInt(1)))
    val produceCost = storageCostProduce(channels.head, data)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.consume(channels, List(pattern), cont, false)
      _         <- chargingRSpace.produce(channels.head, data, true)
      phlosLeft <- costAlg.get()
      _         = phlosLeft.cost shouldBe (initPhlos - produceCost - RSPACE_MATCH_COST)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "charge for storing persistent consume that create a COMM" in { fixture =>
    // for(x <= @x) { P } | @x!(100)
    // we should charge for storing non-linear continuation
    val TestFixture(chargingRSpace, costAlg) = fixture
    val channels                             = channelsN(1)
    val pattern                              = patternsN(1)
    val cont                                 = continuation()

    val data        = ListParWithRandom().withPars(Vector(GInt(1)))
    val consumeCost = storageCostConsume(channels, pattern, cont)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.consume(channels, pattern, cont, true)
      _         <- chargingRSpace.produce(channels.head, data, false)
      phlosLeft <- costAlg.get()
      _         = phlosLeft.cost shouldBe (initPhlos - consumeCost - RSPACE_MATCH_COST)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund for linear data in join" in { fixture =>
    // idea for the test is that we have persistent and non persistent produce in first deploy:
    // @"x"!!(1) | @"y"!(10)
    // and consume on joined channels in another:
    // for(x <- @"x"; y <- @"y") { … }
    // In this case we shouldn't charge for storing consume and refund for removing produce on @"y"

    val TestFixture(chargingRSpace, costAlg) = fixture
    val List(x, y)                           = channelsN(2)
    val patterns                             = patternsN(2)
    val cont                                 = continuation()

    val dataX = ListParWithRandom().withPars(Vector(GInt(1)))
    val dataY = ListParWithRandom().withPars(Vector(GInt(10)))

    val produceYCost = ChargingRSpace.storageCostProduce(y, dataY)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.produce(x, dataX, persist = true)
      _         <- chargingRSpace.produce(y, dataY, persist = false)
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.consume(List(x, y), patterns, cont, false)
      phlosLeft <- costAlg.get()
      _         = phlosLeft.cost shouldBe (initPhlos + produceYCost - (RSPACE_MATCH_COST * 2))
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund for removing consume" in { fixture =>
    // first deploy:
    // for(x <- @x) { P }
    // second deploy:
    // @x!(100)
    // we should refund for removing continuation from tuplespace
    val TestFixture(chargingRSpace, costAlg) = fixture
    val List(x)                              = channelsN(1)
    val patterns                             = patternsN(1)
    val cont                                 = continuation()

    val data = ListParWithRandom().withPars(Vector(GInt(1)))

    val consumeCost = ChargingRSpace.storageCostConsume(List(x), patterns, cont)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.consume(List(x), patterns, cont, false)
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.produce(x, data, persist = false)
      phlosLeft <- costAlg.get()
      _         = phlosLeft.cost shouldBe (initPhlos + consumeCost - RSPACE_MATCH_COST)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund for removing produce" in { fixture =>
    // first deploy:
    // @x!(100)
    // second deploy:
    // for(x <- @x) { P }
    // we should refund for removing @x!(100) from tuplespace
    val TestFixture(chargingRSpace, costAlg) = fixture
    val List(x)                              = channelsN(1)
    val patterns                             = patternsN(1)
    val cont                                 = continuation()

    val data = ListParWithRandom().withPars(Vector(GInt(1)))

    val produceCost = ChargingRSpace.storageCostProduce(x, data)

    val initPhlos = Cost(1000)

    val test = for {
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.produce(x, data, persist = false)
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.consume(List(x), patterns, cont, false)
      phlosLeft <- costAlg.get()
      _         = phlosLeft.cost shouldBe (initPhlos + produceCost - RSPACE_MATCH_COST)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund for clearing tuplespace" in { fixture =>
    // first deploy:
    // @x!(100) | @y!(10) | for(x <- @x; y <- @y; z <- @z) { P }
    // second deploy:
    // @z!(1)
    // since second deploy triggers continuation we should refund with the cost of storing first deploy
    val TestFixture(chargingRSpace, costAlg) = fixture
    val List(x, y, z)                        = channelsN(3)
    val patterns                             = patternsN(3)
    val cont                                 = continuation()

    val dataX = ListParWithRandom().withPars(Vector(GInt(1)))
    val dataY = ListParWithRandom().withPars(Vector(GInt(10)))
    val dataZ = ListParWithRandom().withPars(Vector(GInt(100)))

    val produceXCost = ChargingRSpace.storageCostProduce(x, dataX)
    val produceYCost = ChargingRSpace.storageCostProduce(y, dataY)
    val consumeCost  = ChargingRSpace.storageCostConsume(List(x, y, z), patterns, cont)

    val initPhlos = Cost(10000)

    val test = for {
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.produce(x, dataX, false)
      _         <- chargingRSpace.produce(y, dataY, false)
      _         <- chargingRSpace.consume(List(x, y, z), patterns, cont, false)
      _         <- costAlg.set(CostAccount(0, initPhlos))
      _         <- chargingRSpace.produce(z, dataZ, false)
      phlosLeft <- costAlg.get()
      _         = phlosLeft.cost shouldBe (initPhlos + produceXCost + produceYCost + consumeCost - (RSPACE_MATCH_COST * 3))
    } yield ()

    test.runSyncUnsafe(5.seconds)
  }

  override type FixtureParam = TestFixture

  override protected def withFixture(test: OneArgTest): Outcome = {
    val costAlg = CostAccounting.unsafe[Task](CostAccount(0))

    def mkChargingRspace(rhoISpace: RhoISpace[Task]): Task[ChargingRSpace] = {
      val pureRSpace = ChargingRSpaceTest.createTestISpace(rhoISpace)
      val s          = implicitly[Sync[Task]]
      Task.delay(ChargingRSpace.pureRSpace(s, costAlg, pureRSpace))
    }

    val chargingRSpaceResource =
      mkRhoISpace[Task]("rchain-charging-rspace-test-")
        .flatMap(rhoISpace => Resource.make(mkChargingRspace(rhoISpace))(_.close()))

    chargingRSpaceResource
      .use(chargingRSpace => Task.delay { test(TestFixture(chargingRSpace, costAlg)) })
      .runSyncUnsafe(10.seconds)
  }
}

object ChargingRSpaceTest {
  type ChargingRSpace = RhoPureSpace[Task]
  final case class TestFixture(chargingRSpace: ChargingRSpace, costAlg: CostAccounting[Task])

  val RSPACE_MATCH_PCOST     = 100L
  val RSPACE_MATCH_COST      = Cost(RSPACE_MATCH_PCOST)
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

  // This test ISpace wraps regular RhoISpace but adds predictable match cost
  def createTestISpace(rspace: RhoISpace[Task]): RhoISpace[Task] = new RhoISpace[Task] {
    override def consume(
        channels: immutable.Seq[Par],
        patterns: immutable.Seq[BindPattern],
        continuation: TaggedContinuation,
        persist: Boolean,
        sequenceNumber: Int
    )(
        implicit m: Match[
          BindPattern,
          errors.OutOfPhlogistonsError.type,
          ListParWithRandom,
          ListParWithRandomAndPhlos
        ]
    ): Task[Either[errors.OutOfPhlogistonsError.type, Option[
      (
          ContResult[Par, BindPattern, TaggedContinuation],
          immutable.Seq[Result[ListParWithRandomAndPhlos]]
      )
    ]]] =
      rspace
        .consume(channels, patterns, continuation, persist)
        .map(_.map {
          _.map {
            case (cont, data) =>
              cont -> data.map(r => r.copy(value = r.value.withCost(RSPACE_MATCH_PCOST)))
          }
        })

    override def produce(
        channel: Par,
        data: ListParWithRandom,
        persist: Boolean,
        sequenceNumber: Int
    )(
        implicit m: Match[
          BindPattern,
          errors.OutOfPhlogistonsError.type,
          ListParWithRandom,
          ListParWithRandomAndPhlos
        ]
    ): Task[Either[errors.OutOfPhlogistonsError.type, Option[
      (
          ContResult[Par, BindPattern, TaggedContinuation],
          immutable.Seq[Result[ListParWithRandomAndPhlos]]
      )
    ]]] =
      rspace
        .produce(channel, data, persist)
        .map(_.map {
          _.map {
            case (cont, data) =>
              cont -> data.map(r => r.copy(value = r.value.withCost(RSPACE_MATCH_PCOST)))
          }
        })

    override def close(): Task[Unit] = rspace.close()
    override val store: IStore[Par, BindPattern, ListParWithRandom, TaggedContinuation] =
      rspace.store
    override def install(
        channels: immutable.Seq[Par],
        patterns: immutable.Seq[BindPattern],
        continuation: TaggedContinuation
    )(
        implicit m: Match[
          BindPattern,
          errors.OutOfPhlogistonsError.type,
          ListParWithRandom,
          ListParWithRandomAndPhlos
        ]
    ): Task[Option[(TaggedContinuation, immutable.Seq[ListParWithRandomAndPhlos])]] = ???
    override def createCheckpoint(): Task[Checkpoint]                               = ???
    override def reset(root: Blake2b256Hash): Task[Unit]                            = ???
    override def retrieve(
        root: Blake2b256Hash,
        channelsHash: Blake2b256Hash
    ): Task[Option[internal.GNAT[Par, BindPattern, ListParWithRandom, TaggedContinuation]]] =
      ???
    override def getData(channel: Par): Task[immutable.Seq[internal.Datum[ListParWithRandom]]] =
      ???
    override def getWaitingContinuations(
        channels: immutable.Seq[Par]
    ): Task[immutable.Seq[internal.WaitingContinuation[BindPattern, TaggedContinuation]]] = ???
    override def clear(): Task[Unit]                                                      = ???
  }

}
