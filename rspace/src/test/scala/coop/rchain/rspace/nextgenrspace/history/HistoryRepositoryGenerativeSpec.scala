package coop.rchain.rspace.nextgenrspace.history

import cats.Applicative
import coop.rchain.rspace.{
  DeleteContinuations,
  DeleteData,
  DeleteJoins,
  HotStoreAction,
  InsertContinuations,
  InsertData,
  InsertJoins,
  Serialize
}
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.test.ArbitraryInstances.{arbitraryDatumString, _}
import coop.rchain.shared.GeneratorUtils.distinctListOf
import monix.eval.Task
import org.scalacheck.{Arbitrary, Gen}
import org.scalatest.{FlatSpec, Matchers, OptionValues}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scodec.Codec
import monix.execution.Scheduler.Implicits.global
import cats.implicits._

import scala.concurrent.duration._

class HistoryRepositoryGenerativeSpec
    extends FlatSpec
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks
    with InMemoryHistoryRepositoryTestBase {

  implicit val propertyCheckConfigParam: PropertyCheckConfiguration =
    PropertyCheckConfiguration(minSuccessful = 10)

  implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toCodec
  implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toCodec
  implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toCodec

  "HistoryRepository" should "accept all HotStoreActions" in forAll(
    distinctListOf(arbitraryHotStoreActions)
  ) { actions: List[HotStoreAction] =>
    val emptyHistory =
      new History[Task](emptyRootHash, inMemHistoryStore, inMemPointerBlockStore)
    val repository: HistoryRepository[Task, String, Pattern, String, StringsCaptor] =
      HistoryRepositoryImpl.apply[Task, String, Pattern, String, StringsCaptor](
        emptyHistory,
        inMemColdStore
      )
    (for {
      _ <- actions.foldLeftM(repository) { (repo, action) =>
            for {
              next <- repo.process(action :: Nil)
              _    <- checkActionResult(action, next)
            } yield next
          }
    } yield ()).runSyncUnsafe(20.seconds)
  }

  def checkActionResult(
      action: HotStoreAction,
      repo: HistoryRepository[Task, String, Pattern, String, StringsCaptor]
  ): Task[Unit] =
    action match {
      case InsertData(channel: String, data) =>
        for {
          fetched <- repo.getData(channel)
          _       = fetched shouldBe data
        } yield ()
      case InsertJoins(channel: String, joins) =>
        for {
          fetched <- repo.getJoins(channel)
          _       = fetched shouldBe joins
        } yield ()
      case InsertContinuations(channels, conts) =>
        for {
          fetched <- repo.getContinuations(channels.asInstanceOf[Seq[String]])
          _       = fetched shouldBe conts
        } yield ()
      case DeleteData(channel: String) =>
        for {
          fetched <- repo.getData(channel)
          _       = fetched shouldBe empty
        } yield ()
      case DeleteJoins(channel: String) =>
        for {
          fetched <- repo.getJoins(channel)
          _       = fetched shouldBe empty
        } yield ()
      case DeleteContinuations(channels) =>
        for {
          fetched <- repo.getContinuations(channels.asInstanceOf[Seq[String]])
          _       = fetched shouldBe empty
        } yield ()
      case _ => Applicative[Task].pure(())
    }

  implicit def arbitraryHotStoreActions: Arbitrary[HotStoreAction] = Arbitrary(
    Gen.frequency(
      (10, arbitraryInsertContinuation.arbitrary),
      (10, arbitraryInsertData.arbitrary),
      (10, arbitraryInsertJoins.arbitrary),
      (10, arbitraryDeleteContinuation.arbitrary),
      (10, arbitraryDeleteData.arbitrary),
      (10, arbitraryDeleteJoins.arbitrary)
    )
  )

  def stringGen: Gen[String] = Arbitrary.arbitrary[String]

  def arbitraryInsertData: Arbitrary[HotStoreAction] = Arbitrary(
    for {
      channel <- Arbitrary.arbitrary[String]
      data    <- distinctListOf(arbitraryDatumString)
    } yield InsertData(channel, data)
  )

  def arbitraryInsertContinuation: Arbitrary[HotStoreAction] = Arbitrary(
    for {
      channels <- distinctListOf(Arbitrary(stringGen))
      data     <- distinctListOf(arbitraryWaitingContinuation)
    } yield InsertContinuations(channels, data)
  )

  def arbitraryInsertJoins: Arbitrary[HotStoreAction] = Arbitrary(
    for {
      channel <- Arbitrary.arbitrary[String]
      data    <- distinctListOf(Arbitrary(distinctListOf(Arbitrary(stringGen))))
    } yield InsertJoins(channel, data)
  )

  def arbitraryDeleteContinuation: Arbitrary[HotStoreAction] = Arbitrary(
    for {
      channels <- distinctListOf(Arbitrary(stringGen))
    } yield DeleteContinuations(channels)
  )

  def arbitraryDeleteData: Arbitrary[HotStoreAction] = Arbitrary(
    for {
      channel <- Arbitrary.arbitrary[String]
    } yield DeleteData(channel)
  )

  def arbitraryDeleteJoins: Arbitrary[HotStoreAction] = Arbitrary(
    for {
      channel <- Arbitrary.arbitrary[String]
    } yield DeleteJoins(channel)
  )
}
