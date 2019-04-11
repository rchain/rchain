package coop.rchain.rspace.nextgenrspace.history

import java.nio.charset.StandardCharsets

import coop.rchain.rspace.{
  Blake2b256Hash,
  DeleteContinuations,
  DeleteData,
  DeleteJoins,
  HotStoreAction,
  InsertContinuations,
  InsertData,
  InsertJoins,
  Serialize
}
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers, OptionValues}

import scala.concurrent.duration._
import monix.execution.Scheduler.Implicits.global
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.rspace.nextgenrspace.history.TestData.randomBlake
import coop.rchain.rspace.trace.{Consume, Produce}
import scodec.Codec
import scodec.bits.ByteVector

import scala.collection.concurrent.TrieMap
import scala.util.Random
import cats.implicits._

class HistoryRepositorySpec
    extends FlatSpec
    with Matchers
    with OptionValues
    with InMemoryHistoryTestBase {

  implicit object stringSerialize extends Serialize[String] {

    def encode(a: String): ByteVector =
      ByteVector.view(a.getBytes(StandardCharsets.UTF_8))

    def decode(bytes: ByteVector): Either[Throwable, String] =
      Right(new String(bytes.toArray, StandardCharsets.UTF_8))
  }

  implicit val codecString: Codec[String] = implicitly[Serialize[String]].toCodec
  type TestHistoryRepository = HistoryRepository[Task, String, String, String, String]

  "HistoryRepository" should "process insert one datum" in withEmptyRepository { repo =>
    val testDatum = datum(1)
    val data      = InsertData[String, String](testChannelDataPrefix, testDatum :: Nil)
    for {
      nextRepo <- repo.process(data :: Nil)
      data     <- nextRepo.getData(testChannelDataPrefix)
      fetched  = data.head
      _        = fetched shouldBe testDatum
    } yield ()
  }

  val testChannelDataPrefix          = "channel-data"
  val testChannelJoinsPrefix         = "channel-joins"
  val testChannelContinuationsPrefix = "channel-continuations"

  it should "process insert and delete of thirty mixed elements" in withEmptyRepository { repo =>
    val data  = (0 to 10).map(insertDatum).toList
    val joins = (0 to 10).map(insertJoin).toList
    val conts = (0 to 10)
      .map(insertContinuation)
      .toList
    val elems: List[HotStoreAction] = Random.shuffle(data ++ joins ++ conts)

    val dataDelete                           = data.map(d => DeleteData[String](d.channel))
    val joinsDelete                          = joins.map(j => DeleteJoins[String](j.channel))
    val contsDelete                          = conts.map(c => DeleteContinuations[String](c.channels))
    val deleteElements: List[HotStoreAction] = dataDelete ++ joinsDelete ++ contsDelete

    for {
      nextRepo             <- repo.process(elems)
      fetchedData          <- data.traverse(d => nextRepo.getData(d.channel))
      _                    = fetchedData shouldBe data.map(_.data)
      fetchedContinuations <- conts.traverse(d => nextRepo.getContinuations(d.channels))
      _                    = fetchedContinuations shouldBe conts.map(_.continuations)
      fetchedJoins         <- joins.traverse(d => nextRepo.getJoins(d.channel))
      _                    = fetchedJoins shouldBe joins.map(_.joins)

      deletedRepo <- nextRepo.process(deleteElements)

      fetchedData          <- data.traverse(d => nextRepo.getData(d.channel))
      _                    = fetchedData shouldBe data.map(_.data)
      fetchedContinuations <- conts.traverse(d => nextRepo.getContinuations(d.channels))
      _                    = fetchedContinuations shouldBe conts.map(_.continuations)
      fetchedJoins         <- joins.traverse(d => nextRepo.getJoins(d.channel))
      _                    = fetchedJoins shouldBe joins.map(_.joins)

      fetchedData          <- data.traverse(d => deletedRepo.getData(d.channel))
      _                    = fetchedData.flatten shouldBe empty
      fetchedContinuations <- conts.traverse(d => deletedRepo.getContinuations(d.channels))
      _                    = fetchedContinuations.flatten shouldBe empty
      fetchedJoins         <- joins.traverse(d => deletedRepo.getJoins(d.channel))
      _                    = fetchedJoins.flatten shouldBe empty
    } yield ()
  }

  def insertDatum(s: Any): InsertData[String, String] =
    InsertData(testChannelDataPrefix + s, datum(s) :: Nil)

  def insertJoin(s: Any): InsertJoins[String] =
    InsertJoins(testChannelJoinsPrefix + s, join(s))

  def insertContinuation(s: Any): InsertContinuations[String, String, String] =
    InsertContinuations(testChannelContinuationsPrefix + s :: Nil, continuation(s) :: Nil)

  def join(s: Any): Seq[Seq[String]] =
    ("abc" + s :: "def" + s :: Nil) :: ("wer" + s :: "tre" + s :: Nil) :: Nil

  def continuation(s: Any): WaitingContinuation[String, String] =
    WaitingContinuation[String, String](
      "pattern-" + s :: Nil,
      "cont-" + s,
      true,
      Consume(randomBlake :: Nil, randomBlake, 0)
    )

  def datum(s: Any): Datum[String] =
    Datum[String]("data-" + s, false, Produce(randomBlake, randomBlake, 0))

  protected def withEmptyRepository(f: TestHistoryRepository => Task[Unit]): Unit = {
    val emptyHistory =
      new History[Task](emptyRootHash, inMemHistoryStore, inMemPointerBlockStore)
    val repo: TestHistoryRepository =
      HistoryRepositoryImpl[Task, String, String, String, String](emptyHistory, inMemColdStore)
    f(repo).runSyncUnsafe(20.seconds)
  }

  def inMemColdStore: ColdStore[Task] = new ColdStore[Task] {
    val data: TrieMap[Blake2b256Hash, PersistedData] = TrieMap.empty

    override def put(hash: Blake2b256Hash, d: PersistedData): Task[Unit] =
      Task.delay { data.put(hash, d) }

    override def get(hash: Blake2b256Hash): Task[Option[PersistedData]] =
      Task.delay { data.get(hash) }
  }
}
