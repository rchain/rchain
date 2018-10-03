package coop.rchain.rholang.interpreter.storage

import java.nio.file.Files

import cats.Id
import cats.effect.Sync
import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.models.TaggedContinuation.TaggedCont.ParBody
import coop.rchain.models.Var.VarInstance.FreeVar
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.Runtime.{RhoContext, RhoISpace, RhoPureSpace}
import coop.rchain.rholang.interpreter.accounting.{CostAccount, CostAccountingAlg, _}
import coop.rchain.rholang.interpreter.errors
import coop.rchain.rholang.interpreter.errors.OutOfPhlogistonsError
import coop.rchain.rholang.interpreter.storage.ChargingRSpaceTest._
import coop.rchain.rspace.{Match, _}
import coop.rchain.rspace.history.Branch
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalactic.TripleEqualsSupport
import org.scalatest.{fixture, Matchers, Outcome}

import scala.collection.immutable
import scala.concurrent.duration._

class ChargingRSpaceTest extends fixture.FlatSpec with TripleEqualsSupport with Matchers {

  behavior of "ChargingRSpace"

  it should "charge for storing data in tuplespace" in { fixture =>
    val TestFixture(chargingRSpace, costAlg, pureRSpace) = fixture
    val channels                                         = channelsN(1)
    val patterns                                         = patternsN(1)
    val cont                                             = continuation()
    val storageCost                                      = ChargingRSpace.storageCostConsume(channels, patterns, cont)
    val minimumPhlos                                     = storageCost
    setInitPhlos(costAlg, minimumPhlos)

    val test = for {
      _         <- chargingRSpace.consume(channels, patterns, cont, false)
      phlosLeft <- costAlg.get()
      // we expect Cost = 1 because there will be no match
      // so we will pay only for the storage
      _ = phlosLeft.cost shouldBe Cost(0)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "refund if data doesn't stay in tuplespace" in { fixture =>
    val TestFixture(chargingRSpace, costAlg, pureRSpace) = fixture
    val channels                                         = channelsN(1)
    val patterns                                         = patternsN(1)
    val cont                                             = continuation()
    val consumeStorageCost                               = ChargingRSpace.storageCostConsume(channels, patterns, cont)
    val data                                             = NilPar
    val produceStorageCost                               = ChargingRSpace.storageCostProduce(channels.head, data)
    val minimumPhlos                                     = produceStorageCost + consumeStorageCost + RSPACE_MATCH_COST

    setInitPhlos(costAlg, minimumPhlos)

    val test = for {
      _                 <- chargingRSpace.produce(channels.head, data, false)
      phlosAfterProduce <- costAlg.get()
      _                 = phlosAfterProduce.cost shouldBe (minimumPhlos - produceStorageCost)
      res               <- chargingRSpace.consume(channels, patterns, cont, false)
      phlosLeft         <- costAlg.get()
      // we expect Cost(15), because we will be refunded for storing the consume
      _ = phlosLeft.cost shouldBe (consumeStorageCost)
    } yield ()

    test.runSyncUnsafe(1.second)
  }

  it should "fail with OutOfPhloError when deploy runs out of it" in { fixture =>
    val TestFixture(chargingRSpace, costAlg, pureRSpace) = fixture
    val channel                                          = channelsN(1).head
    val data                                             = NilPar
    val produceStorageCost                               = ChargingRSpace.storageCostProduce(channel, data)

    setInitPhlos(costAlg, produceStorageCost - Cost(1))

    val test = for {
      _ <- chargingRSpace.produce(channel, data, false)
    } yield ()

    val outOfPhloTest = test.attempt.runSyncUnsafe(1.second)
    assert(outOfPhloTest === Left(OutOfPhlogistonsError))

    val costAlgTest = costAlg.get().runSyncUnsafe(1.second)
    assert(costAlgTest === CostAccount(1, Cost(-1)))
  }

  it should "charge COMM on a join properly when parts of the join are deployed separately" in {
    fixture =>
      val TestFixture(chargingRSpace, costAlg, pureRSpace) = fixture
      val channels                                         = channelsN(2)
      val patterns                                         = patternsN(2)
      val cont                                             = continuation()
      val data                                             = NilPar
      val firstProdCost                                    = ChargingRSpace.storageCostProduce(channels(0), data)
      val secondProdCost                                   = ChargingRSpace.storageCostProduce(channels(1), data)
      val joinCost                                         = ChargingRSpace.storageCostConsume(channels, patterns, cont)
      val minimumPhlos                                     = firstProdCost + secondProdCost + joinCost + (RSPACE_MATCH_COST * 2)

      setInitPhlos(costAlg, minimumPhlos)

      val test = for {
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
        _         = phlosLeft.cost shouldBe (secondProdCost)
      } yield ()

      test.runSyncUnsafe(1.second)
  }

  type ChargingRSpace = RhoPureSpace[Task]

  override protected def withFixture(test: OneArgTest): Outcome = {
    implicit val costAlg    = CostAccountingAlg.unsafe[Task](CostAccount(0))
    implicit val pureRSpace = ChargingRSpaceTest.createTestISpace()
    implicit val s          = implicitly[Sync[Task]]
    val chargingRSpace      = ChargingRSpace.pureRSpace(s, costAlg, pureRSpace)
    try {
      test(TestFixture(chargingRSpace, costAlg, pureRSpace))
    } finally {
      pureRSpace.close()
    }
  }
  final case class TestFixture(
      chargingRSpace: ChargingRSpace,
      costAlg: CostAccountingAlg[Task],
      pureRSpace: RhoISpace
  )

  override type FixtureParam = TestFixture
}

object ChargingRSpaceTest {
  val RSPACE_MATCH_PCOST     = 100L
  val RSPACE_MATCH_COST      = Cost(RSPACE_MATCH_PCOST)
  val NilPar                 = ListParWithRandom().withPars(Seq(Par()))
  val rand: Blake2b512Random = Blake2b512Random(Array.empty[Byte])

  private def byteName(b: Byte): Par = GPrivate(ByteString.copyFrom(Array[Byte](b)))

  def channelsN(n: Int): List[Par] =
    (1 to n).map(x => byteName(x.toByte)).toList
  def patternsN(n: Int): List[BindPattern] =
    (1 to n)
      .map(
        _ => BindPattern(Vector(EVar(Var(FreeVar(0)))))
      )
      .toList
  def continuation(par: Par = Par(), r: Blake2b512Random = rand): TaggedContinuation =
    TaggedContinuation(ParBody(ParWithRandom(par, r)))

  def setInitPhlos(costAlg: CostAccountingAlg[Task], init: Cost): Unit =
    costAlg.set(CostAccount(0, init)).runSyncUnsafe(1.second)

  // This test ISpace wraps regular RhoISpace but adds predictable match cost
  def createTestISpace(): RhoISpace = new RhoISpace {
    private val rspace = createRhoISpace()

    override def consume(
        channels: immutable.Seq[Par],
        patterns: immutable.Seq[BindPattern],
        continuation: TaggedContinuation,
        persist: Boolean
    )(
        implicit m: Match[
          BindPattern,
          errors.OutOfPhlogistonsError.type,
          ListParWithRandom,
          ListParWithRandomAndPhlos
        ]
    ): Id[Either[errors.OutOfPhlogistonsError.type, Option[
      (TaggedContinuation, immutable.Seq[ListParWithRandomAndPhlos])
    ]]] =
      rspace
        .consume(channels, patterns, continuation, persist)
        .map(_.map { case (cont, data) => cont -> data.map(_.withCost(RSPACE_MATCH_PCOST)) })

    override def produce(channel: Par, data: ListParWithRandom, persist: Boolean)(
        implicit m: Match[
          BindPattern,
          errors.OutOfPhlogistonsError.type,
          ListParWithRandom,
          ListParWithRandomAndPhlos
        ]
    ): Id[Either[errors.OutOfPhlogistonsError.type, Option[
      (TaggedContinuation, immutable.Seq[ListParWithRandomAndPhlos])
    ]]] =
      rspace
        .produce(channel, data, persist)
        .map(_.map { case (cont, pars) => cont -> pars.map(_.withCost(RSPACE_MATCH_PCOST)) })

    override def close(): Id[Unit] = rspace.close()
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
    ): Id[Option[(TaggedContinuation, immutable.Seq[ListParWithRandomAndPhlos])]] = ???
    override def createCheckpoint(): Id[Checkpoint]                               = ???
    override def reset(root: Blake2b256Hash): Id[Unit]                            = ???
    override def retrieve(
        root: Blake2b256Hash,
        channelsHash: Blake2b256Hash
    ): Id[Option[internal.GNAT[Par, BindPattern, ListParWithRandom, TaggedContinuation]]] =
      ???
    override def getData(channel: Par): immutable.Seq[internal.Datum[ListParWithRandom]] =
      ???
    override def getWaitingContinuations(
        channels: immutable.Seq[Par]
    ): immutable.Seq[internal.WaitingContinuation[BindPattern, TaggedContinuation]] = ???
    override def clear(): Id[Unit]                                                  = ???
  }

  def createRhoISpace(): RhoISpace = {
    implicit val syncF: Sync[Id] = coop.rchain.catscontrib.effect.implicits.syncId
    import coop.rchain.rholang.interpreter.storage.implicits._
    val dbDir               = Files.createTempDirectory("rchain-charging-rspace-test-")
    val context: RhoContext = Context.createFineGrained(dbDir, 1024L * 1024L * 4)
    val space: RhoISpace = RSpace.create[
      Id,
      Par,
      BindPattern,
      OutOfPhlogistonsError.type,
      ListParWithRandom,
      ListParWithRandomAndPhlos,
      TaggedContinuation
    ](context, Branch("test"))
    space
  }

}
