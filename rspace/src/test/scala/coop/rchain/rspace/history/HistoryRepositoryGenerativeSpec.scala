package coop.rchain.rspace.history

import cats.effect.Sync
import java.nio.file.{Files, Path}

import coop.rchain.rspace.{
  DeleteContinuations,
  DeleteData,
  DeleteJoins,
  HotStoreAction,
  InsertContinuations,
  InsertData,
  InsertJoins
}
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.test.ArbitraryInstances.{arbitraryDatumString, _}
import monix.eval.Task
import org.scalacheck.{Arbitrary, Gen, Shrink}
import org.scalatest.{Assertion, BeforeAndAfterAll, FlatSpec, Matchers, OptionValues}
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import scodec.Codec
import monix.execution.Scheduler.Implicits.global
import cats.implicits._
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.rspace.state.instances.{RSpaceExporterImpl, RSpaceImporterImpl}
import org.lmdbjava.EnvFlags
import coop.rchain.shared.PathOps._
import coop.rchain.shared.Serialize

import scala.concurrent.duration._

class LMDBHistoryRepositoryGenerativeSpec
    extends HistoryRepositoryGenerativeDefinition
    with BeforeAndAfterAll {

  val dbDir: Path = Files.createTempDirectory("rchain-storage-test-")

  def lmdbConfig =
    StoreConfig(
      Files.createTempDirectory(dbDir, "test-"),
      1024L * 1024L * 4096L,
      2,
      2048,
      List(EnvFlags.MDB_NOTLS)
    )

  override def repo: Task[HistoryRepository[Task, String, Pattern, String, StringsCaptor]] =
    for {
      historyLmdbStore <- StoreInstances.lmdbStore[Task](lmdbConfig)
      historyStore     = HistoryStoreInstances.historyStore(historyLmdbStore)
      coldLmdbStore    <- StoreInstances.lmdbStore[Task](lmdbConfig)
      coldStore        = ColdStoreInstances.coldStore(coldLmdbStore)
      rootsLmdbStore   <- StoreInstances.lmdbStore[Task](lmdbConfig)
      rootsStore       = RootsStoreInstances.rootsStore(rootsLmdbStore)
      rootRepository   = new RootRepository[Task](rootsStore)
      emptyHistory     = HistoryInstances.merging(History.emptyRootHash, historyStore)
      exporter         = RSpaceExporterImpl[Task](historyLmdbStore, coldLmdbStore, rootsLmdbStore)
      importer         = RSpaceImporterImpl[Task](historyLmdbStore, coldLmdbStore, rootsLmdbStore)
      repository: HistoryRepository[Task, String, Pattern, String, StringsCaptor] = HistoryRepositoryImpl
        .apply[Task, String, Pattern, String, StringsCaptor](
          emptyHistory,
          rootRepository,
          coldStore,
          exporter,
          importer
        )
    } yield repository

  protected override def afterAll(): Unit =
    dbDir.recursivelyDelete()
}

class InmemHistoryRepositoryGenerativeSpec
    extends HistoryRepositoryGenerativeDefinition
    with InMemoryHistoryRepositoryTestBase {

  override def repo: Task[HistoryRepository[Task, String, Pattern, String, StringsCaptor]] = {
    val emptyHistory =
      HistoryInstances.merging[Task](History.emptyRootHash, inMemHistoryStore)
    val repository: HistoryRepository[Task, String, Pattern, String, StringsCaptor] =
      HistoryRepositoryImpl.apply[Task, String, Pattern, String, StringsCaptor](
        emptyHistory,
        rootRepository,
        inMemColdStore,
        emptyExporter,
        emptyImporter
      )
    repository.pure[Task]
  }
}

abstract class HistoryRepositoryGenerativeDefinition
    extends FlatSpec
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks {

  implicit val codecString: Codec[String]   = implicitly[Serialize[String]].toSizeHeadCodec
  implicit val codecP: Codec[Pattern]       = implicitly[Serialize[Pattern]].toSizeHeadCodec
  implicit val codecK: Codec[StringsCaptor] = implicitly[Serialize[StringsCaptor]].toSizeHeadCodec

  implicit def noShrink[T]: Shrink[T] = Shrink.shrinkAny

  def repo: Task[HistoryRepository[Task, String, Pattern, String, StringsCaptor]]

  "HistoryRepository" should "accept all HotStoreActions" in forAll(
    minSize(1),
    sizeRange(50),
    minSuccessful(10)
  ) { actions: List[HotStoreAction] =>
    repo
      .flatMap { repository =>
        actions
          .foldLeftM(repository) { (repo, action) =>
            for {
              next <- repo.checkpoint(action :: Nil)
              _    <- checkActionResult(action, next)
            } yield next
          }
      }
      .runSyncUnsafe(20.seconds)
  }

  def checkData(seq: Seq[Datum[String]], data: Seq[Datum[Any]]): Assertion =
    seq should contain theSameElementsAs data

  def checkJoins(seq: Seq[Seq[String]], joins: Seq[Seq[Any]]): Assertion =
    seq.toSet.map((v: Seq[String]) => v.toSet) should contain theSameElementsAs joins.toSet.map(
      (v: Seq[Any]) => v.toSet
    )

  def checkContinuations(
      seq: Seq[WaitingContinuation[Pattern, StringsCaptor]],
      conts: Seq[WaitingContinuation[Any, Any]]
  ): Assertion =
    seq should contain theSameElementsAs conts

  def checkActionResult(
      action: HotStoreAction,
      repo: HistoryRepository[Task, String, Pattern, String, StringsCaptor]
  ): Task[Unit] =
    action match {
      case InsertData(channel: String, data) =>
        repo.getData(channel).map(checkData(_, data))
      case InsertJoins(channel: String, joins) =>
        repo.getJoins(channel).map(checkJoins(_, joins))
      case InsertContinuations(channels, conts) =>
        repo.getContinuations(channels.asInstanceOf[Seq[String]]).map(checkContinuations(_, conts))
      case DeleteData(channel: String) =>
        repo.getData(channel).map(_ shouldBe empty)
      case DeleteJoins(channel: String) =>
        repo.getJoins(channel).map(_ shouldBe empty)
      case DeleteContinuations(channels) =>
        repo.getContinuations(channels.asInstanceOf[Seq[String]]).map(_ shouldBe empty)
      case _ => Sync[Task].raiseError(new RuntimeException("unknown action"))
    }

  implicit def limitedArbitraryHotStoreActions: Arbitrary[List[HotStoreAction]] =
    Arbitrary(Gen.listOf(arbitraryHotStoreActions.arbitrary))

  def arbitraryHotStoreActions: Arbitrary[HotStoreAction] = Arbitrary(
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
  def distinctStrings: Gen[List[String]] =
    Gen.sized(sz => Gen.containerOfN[Set, String](sz, stringGen).map(_.toList))

  def genDatumString: Gen[Datum[String]] = arbitraryDatumString.arbitrary
  def genContinuation: Gen[WaitingContinuation[Pattern, StringsCaptor]] =
    arbitraryWaitingContinuation.arbitrary

  def arbitraryInsertData: Arbitrary[InsertData[String, String]] = Arbitrary(
    for {
      channel <- Arbitrary.arbitrary[String]
      sz      <- Gen.size
      data    <- Gen.containerOfN[Set, Datum[String]](sz, genDatumString).map(_.toList)
    } yield InsertData(channel, data)
  )

  def arbitraryInsertContinuation: Arbitrary[InsertContinuations[String, Pattern, StringsCaptor]] =
    Arbitrary(
      for {
        channels <- distinctStrings
        sz       <- Gen.size
        data <- Gen
                 .containerOfN[Set, WaitingContinuation[Pattern, StringsCaptor]](
                   sz,
                   genContinuation
                 )
                 .map(_.toList)
      } yield InsertContinuations(channels, data)
    )

  def arbitraryInsertJoins: Arbitrary[InsertJoins[String]] = Arbitrary(
    for {
      channel <- Arbitrary.arbitrary[String]
      data    <- Gen.listOf(distinctStrings)
    } yield InsertJoins(channel, data)
  )

  def arbitraryDeleteContinuation: Arbitrary[DeleteContinuations[String]] = Arbitrary(
    for {
      channels <- distinctStrings
    } yield DeleteContinuations(channels)
  )

  def arbitraryDeleteData: Arbitrary[DeleteData[String]] = Arbitrary(
    for {
      channel <- Arbitrary.arbitrary[String]
    } yield DeleteData(channel)
  )

  def arbitraryDeleteJoins: Arbitrary[DeleteJoins[String]] = Arbitrary(
    for {
      channel <- Arbitrary.arbitrary[String]
    } yield DeleteJoins(channel)
  )
}
