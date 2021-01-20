package coop.rchain.rspace.history

import java.nio.ByteBuffer
import java.nio.file.{Files, Path}

import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.rspace.channelStore.instances.ChannelStoreImpl.ChannelStoreImpl
import coop.rchain.rspace.history.TestData.{randomBlake, zerosBlake}
import coop.rchain.rspace.internal.{Datum, WaitingContinuation}
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
import coop.rchain.rspace.trace.{Consume, Log, Produce}
import coop.rchain.rspace.{
  util,
  Blake2b256Hash,
  DeleteContinuations,
  DeleteData,
  DeleteJoins,
  HotStoreAction,
  InsertContinuations,
  InsertData,
  InsertJoins
}
import coop.rchain.shared.Log
import coop.rchain.state.TrieNode
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.lmdbjava.EnvFlags
import org.scalatest.{FlatSpec, Matchers, OptionValues}
import scodec.Codec
import scodec.bits.{BitVector, ByteVector}

import scala.collection.SortedSet
import scala.collection.concurrent.TrieMap
import scala.concurrent.duration._
import scala.util.Random
import cats.implicits._
import coop.rchain.rspace.channelStore.instances.ChannelStoreImpl.ChannelStoreImpl
import coop.rchain.rspace.state.{RSpaceExporter, RSpaceImporter}
import coop.rchain.rspace.util.stringSerialize
import coop.rchain.state.TrieNode
import org.lmdbjava.EnvFlags
import scodec.Codec
import scodec.bits.BitVector

import scala.collection.SortedSet

class HistoryRepositorySpec
    extends FlatSpec
    with Matchers
    with OptionValues
    with InMemoryHistoryRepositoryTestBase {

  type TestHistoryRepository = HistoryRepository[Task, String, String, String, String]

  "HistoryRepository" should "process insert one datum" in withEmptyRepository { repo =>
    val testDatum = datum(1)
    val data      = InsertData[String, String](testChannelDataPrefix, testDatum :: Nil)
    for {
      nextRepo <- repo.checkpoint(data :: Nil)
      data     <- nextRepo.getData(testChannelDataPrefix)
      fetched  = data.head
      _        = fetched shouldBe testDatum
    } yield ()
  }

  val testChannelDataPrefix          = "channel-data"
  val testChannelJoinsPrefix         = "channel-joins"
  val testChannelContinuationsPrefix = "channel-continuations"

  it should "allow insert of joins, datum and continuation on same channel" in withEmptyRepository {
    repo =>
      val channel          = testChannelDataPrefix
      val testDatum        = datum(1)
      val data             = InsertData[String, String](channel, testDatum :: Nil)
      val testJoins        = join(1)
      val joins            = InsertJoins[String](channel, testJoins)
      val testContinuation = continuation(1)
      val continuations =
        InsertContinuations[String, String, String](channel :: Nil, testContinuation :: Nil)
      for {
        nextRepo            <- repo.checkpoint(data :: joins :: continuations :: Nil)
        fetchedData         <- nextRepo.getData(channel)
        fetchedContinuation <- nextRepo.getContinuations(channel :: Nil)
        fetchedJoins        <- nextRepo.getJoins(channel)
        _                   = fetchedData should have size 1
        _                   = fetchedData.head shouldBe testDatum
        _                   = fetchedContinuation should have size 1
        _                   = fetchedContinuation.head shouldBe testContinuation
        _                   = fetchedJoins should have size 2
        _                   = fetchedJoins.toSet.flatten.flatten should contain theSameElementsAs joins.joins.toSet.flatten.flatten
      } yield ()
  }

  it should "process insert and delete of thirty mixed elements" in withEmptyRepository { repo =>
    val data  = (0 to 10).map(insertDatum).toVector
    val joins = (0 to 10).map(insertJoin).toVector
    val conts = (0 to 10)
      .map(insertContinuation)
      .toVector
    val elems: Vector[HotStoreAction] = Random.shuffle(data ++ joins ++ conts)

    val dataDelete                             = data.map(d => DeleteData[String](d.channel))
    val joinsDelete                            = joins.map(j => DeleteJoins[String](j.channel))
    val contsDelete                            = conts.map(c => DeleteContinuations[String](c.channels))
    val deleteElements: Vector[HotStoreAction] = dataDelete ++ joinsDelete ++ contsDelete

    for {
      nextRepo             <- repo.checkpoint(elems.toList)
      fetchedData          <- data.traverse(d => nextRepo.getData(d.channel))
      _                    = fetchedData shouldBe data.map(_.data)
      fetchedContinuations <- conts.traverse(d => nextRepo.getContinuations(d.channels))
      _                    = fetchedContinuations shouldBe conts.map(_.continuations)
      fetchedJoins         <- joins.traverse(d => nextRepo.getJoins(d.channel))
      allJoins             = fetchedJoins.toSet.flatten.flatten
      expectedJoins        = joins.toSet.flatMap((j: InsertJoins[String]) => j.joins).flatten
      _                    = allJoins should contain theSameElementsAs expectedJoins
      deletedRepo          <- nextRepo.checkpoint(deleteElements.toList)

      fetchedData          <- data.traverse(d => nextRepo.getData(d.channel))
      _                    = fetchedData shouldBe data.map(_.data)
      fetchedContinuations <- conts.traverse(d => nextRepo.getContinuations(d.channels))
      _                    = fetchedContinuations shouldBe conts.map(_.continuations)
      fetchedJoins         <- joins.traverse(d => nextRepo.getJoins(d.channel))
      allJoins             = fetchedJoins.toSet.flatten.flatten
      _                    = allJoins should contain theSameElementsAs expectedJoins

      fetchedData          <- data.traverse(d => deletedRepo.getData(d.channel))
      _                    = fetchedData.flatten shouldBe empty
      fetchedContinuations <- conts.traverse(d => deletedRepo.getContinuations(d.channels))
      _                    = fetchedContinuations.flatten shouldBe empty
      fetchedJoins         <- joins.traverse(d => deletedRepo.getJoins(d.channel))
      _                    = fetchedJoins.flatten shouldBe empty
    } yield ()
  }

  it should "not allow switching to a not existing root" in withEmptyRepository { repo =>
    repo.reset(zerosBlake).attempt.map {
      case Left(RuntimeException("unknown root")) => ()
      case _                                      => fail("Expected a failure")
    }
  }

  it should "record next root as valid" in withEmptyRepository { repo =>
    val testDatum = datum(1)
    val data      = InsertData[String, String](testChannelDataPrefix, testDatum :: Nil)
    for {
      nextRepo <- repo.checkpoint(data :: Nil)
      _        <- repo.reset(History.emptyRootHash)
      _        <- repo.reset(nextRepo.history.root)
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
      SortedSet.empty,
      Consume(randomBlake :: Nil, randomBlake, true)
    )

  def datum(s: Any): Datum[String] =
    Datum[String]("data-" + s, false, Produce(randomBlake, randomBlake, false))

  protected def withEmptyRepository(f: TestHistoryRepository => Task[Unit]): Unit = {
    implicit val codecString: Codec[String] = util.stringCodec
    implicit val log                        = new Log.NOPLog[Task]
    val emptyHistory                        = HistoryInstances.merging[Task](History.emptyRootHash, inMemHistoryStore)
    val pastRoots                           = rootRepository
    val lmdbConfig = StoreConfig(
      Files.createTempDirectory("test-"),
      1024L * 1024L * 4096L,
      2,
      2048,
      List(EnvFlags.MDB_NOTLS)
    )

    (for {
      _                <- pastRoots.commit(History.emptyRootHash)
      channelLMDBStore <- StoreInstances.lmdbStore[Task](lmdbConfig)
      channelStore     = new ChannelStoreImpl(channelLMDBStore, stringSerialize, codecString)
      repo = HistoryRepositoryImpl[Task, String, String, String, String](
        emptyHistory,
        pastRoots,
        inMemColdStore,
        emptyExporter,
        emptyImporter,
        channelStore,
        stringSerialize
      )
      _ <- f(repo)
    } yield ()).runSyncUnsafe(20.seconds)
  }
}

object RuntimeException {
  def unapply(arg: RuntimeException): Option[String] = Option(arg.getMessage)
}

trait InMemoryHistoryRepositoryTestBase extends InMemoryHistoryTestBase {
  def inmemRootsStore =
    new RootsStore[Task] {
      var roots: Set[Blake2b256Hash]               = Set.empty
      var maybeCurrentRoot: Option[Blake2b256Hash] = None

      override def currentRoot(): Task[Option[Blake2b256Hash]] =
        Task.delay {
          maybeCurrentRoot
        }

      override def validateAndSetCurrentRoot(key: Blake2b256Hash): Task[Option[Blake2b256Hash]] =
        Task.delay {
          if (roots.contains(key)) {
            maybeCurrentRoot = Some(key)
            maybeCurrentRoot
          } else {
            None
          }
        }

      override def recordRoot(key: Blake2b256Hash): Task[Unit] =
        Task.delay {
          maybeCurrentRoot = Some(key)
          roots += key
        }

      override def close(): Task[Unit] = Task.delay(())
    }

  def rootRepository =
    new RootRepository[Task](inmemRootsStore)

  def inMemStore = new Store[Task] {
    val data: TrieMap[ByteBuffer, ByteBuffer] = TrieMap.empty

    override def close(): Task[Unit] = Task.delay(())

    override def get(key: Blake2b256Hash): Task[Option[BitVector]] = Task.delay {
      data.get(key.bytes.toByteBuffer).map(BitVector(_))
    }

    override def get(key: ByteBuffer): Task[Option[BitVector]] = Task.delay {
      data.get(key).map(BitVector(_))
    }

    override def get[T](
        keys: Seq[Blake2b256Hash],
        fromBuffer: ByteBuffer => T
    ): Task[Seq[Option[T]]] = Task {
      keys.map(k => get(k.bytes.toByteBuffer).runSyncUnsafe().map(b => fromBuffer(b.toByteBuffer)))
    }

    override def put(data: Seq[(Blake2b256Hash, BitVector)]): Task[Unit] = Task {
      data.map { case (k, v) => put(k, v).runSyncUnsafe() }
      ()
    }

    override def put(key: Blake2b256Hash, value: BitVector): Task[Unit] = Task {
      data.put(key.bytes.toByteBuffer, value.toByteBuffer)
    }

    override def put(key: ByteBuffer, value: ByteBuffer): Task[Unit] = Task { data.put(key, value) }

    override def put[T](keys: Seq[(Blake2b256Hash, T)], toBuffer: T => ByteBuffer): Task[Unit] =
      Task {
        keys.map { case (k, v) => put(k.bytes.toByteBuffer, toBuffer(v)).runSyncUnsafe() }
        ()
      }
  }

  def inMemColdStore: ColdStore[Task] = new ColdStore[Task] {
    val data: TrieMap[Blake2b256Hash, PersistedData] = TrieMap.empty

    override def put(hash: Blake2b256Hash, d: PersistedData): Task[Unit] =
      Task.delay { data.put(hash, d) }

    override def get(hash: Blake2b256Hash): Task[Option[PersistedData]] =
      Task.delay { data.get(hash) }

    override def close(): Task[Unit] = Task.delay(())

    override def put(list: List[(Blake2b256Hash, PersistedData)]): Task[Unit] =
      list.traverse_(Function.tupled(put))
  }

  def emptyExporter[F[_]: Sync]: RSpaceExporter[F] = new RSpaceExporter[F] {
    override def getRoot: F[Blake2b256Hash] = ???

    override def getNodes(
        startPath: NodePath,
        skip: Int,
        take: Int
    ): F[Seq[TrieNode[Blake2b256Hash]]] = ???

    override def getHistoryItems[Value](
        keys: Seq[Blake2b256Hash],
        fromBuffer: ByteBuffer => Value
    ): F[Seq[(Blake2b256Hash, Value)]] = ???

    override def getDataItems[Value](
        keys: Seq[Blake2b256Hash],
        fromBuffer: ByteBuffer => Value
    ): F[Seq[(Blake2b256Hash, Value)]] = ???
  }

  def emptyImporter[F[_]: Sync]: RSpaceImporter[F] = new RSpaceImporter[F] {
    override def setHistoryItems[Value](
        data: Seq[(Blake2b256Hash, Value)],
        toBuffer: Value => ByteBuffer
    ): F[Unit] = ???

    override def setDataItems[Value](
        data: Seq[(Blake2b256Hash, Value)],
        toBuffer: Value => ByteBuffer
    ): F[Unit] = ???

    override def setRoot(key: Blake2b256Hash): F[Unit] = ???

    override def getHistoryItem(hash: Blake2b256Hash): F[Option[ByteVector]] = ???
  }
}
