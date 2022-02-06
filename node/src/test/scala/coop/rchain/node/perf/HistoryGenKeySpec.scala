package coop.rchain.node.perf

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryMergingInstances.{CachingHistoryStore, MergingHistory}
import coop.rchain.rspace.history.RadixTree.ExportData
import coop.rchain.rspace.history._
import coop.rchain.rspace.history.instances._
import coop.rchain.shared.{Base16, Log}
import coop.rchain.shared.syntax._
import coop.rchain.store.{InMemoryKeyValueStore, KeyValueStore, LmdbStoreManager}
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.ByteVector

import java.io.File
import java.math.BigInteger
import java.nio.file.Paths
import scala.concurrent.ExecutionContext

class HistoryGenKeySpec extends FlatSpec with Matchers {

  object Settings {
//    val typeHistory: String = "MergingHistory"
    val typeHistory: String = "RadixHistory"

    val typeStore: String = "lmdb"
//    val typeStore: String = "inMemo"

//    val calcSize: Boolean = false
    val calcSize: Boolean = true

//    val deleteTest: Boolean = false
    val deleteTest: Boolean = true

    val random: Boolean = false
//    val random: Boolean = true

//    val averageStatistic: Boolean = true
    val averageStatistic: Boolean = false

    val averageNum: Int    = 5
    val averageWarmUp: Int = 3

    val flagSize: Boolean = calcSize && (typeStore == "inMemo")

    val taskCur: List[ExpT] = tasksList
  }

  case class ExpT(initNum: Int, insReadDelNum: Int)
  val tasksList: List[ExpT] = List(
    ExpT(1, 300),
    ExpT(1, 1000),
    ExpT(1, 5000),
    ExpT(1, 10000),
    ExpT(1, 30000),
    ExpT(1, 100000)
  )

  def deleteFile(path: String): Boolean = new File(path).delete()

  sealed trait CreateHistory[F[_]] {
    def create(root: Blake2b256Hash, lmdbPath: String): F[HistoryType[F]]
  }

  def storeLMDB[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span](
      path: String
  ): F[KeyValueStore[F]] =
    for {
      lmdbHistoryManager <- LmdbStoreManager(Paths.get(path), 8L * 1024 * 1024 * 1024)
      lmdbHistoryStore   <- lmdbHistoryManager.store("db")
      _                  <- Sync[F].delay(deleteFile(path + "/data.mdb"))
      _                  <- Sync[F].delay(deleteFile(path + "/lock.mdb"))
    } yield lmdbHistoryStore

  sealed trait HistoryType[F[_]] {
    val history: History[F]
    def getNodeDataFromStore: ByteVector => F[Option[ByteVector]]
  }
  case class HistoryWithoutFunc[F[_]](
      history: History[F],
      getNodeDataFromStore: ByteVector => F[Option[ByteVector]]
  ) extends HistoryType[F]
  case class HistoryWithFunc[F[_]](
      history: History[F],
      getNodeDataFromStore: ByteVector => F[Option[ByteVector]],
      sizeBytes: () => Long,
      numRecords: () => Int
  ) extends HistoryType[F]

  case class createMergingHistory[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span]()
      extends CreateHistory[F] {
    def create(root: Blake2b256Hash, lmdbPath: String): F[HistoryType[F]] =
      Settings.typeStore match {
        case "inMemo" =>
          val inMemoStore = InMemoryKeyValueStore[F]
          inMemoStore.clear()
          val store = HistoryStoreInstances.historyStore(inMemoStore)
          for { history <- MergingHistory(root, CachingHistoryStore(store)).pure } yield HistoryWithFunc(
            history,
            _ => Sync[F].pure(None),
            inMemoStore.sizeBytes,
            inMemoStore.numRecords
          )
        case "lmdb" =>
          for {
            store <- storeLMDB(lmdbPath)
            history <- MergingHistory(
                        root,
                        CachingHistoryStore(HistoryStoreInstances.historyStore(store))
                      ).pure
          } yield HistoryWithoutFunc(history, _ => Sync[F].pure(None))

      }
  }

  case class createRadixHistory[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span]()
      extends CreateHistory[F] {
    def create(root: Blake2b256Hash, lmdbPath: String): F[HistoryType[F]] =
      Settings.typeStore match {
        case "inMemo" =>
          val inMemoStore = InMemoryKeyValueStore[F]
          inMemoStore.clear()
          val radixStore = RadixHistory.createStore(inMemoStore)
          for { history <- RadixHistory[F](root, radixStore) } yield HistoryWithFunc(
            history,
            radixStore.get1,
            inMemoStore.sizeBytes,
            inMemoStore.numRecords
          )
        case "lmdb" =>
          for {
            store      <- storeLMDB(lmdbPath)
            radixStore = RadixHistory.createStore(store)
            history    <- RadixHistory[F](root, radixStore)
          } yield HistoryWithoutFunc(history, radixStore.get1)
      }
  }

  class Experiment[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span](
      implicit ectx: ExecutionContext
  ) extends HistoryHelpers[F] {

    override def init: Deps =
      (ectx, Concurrent[F], ContextShift[F], Parallel[F], Log[F], Metrics[F], Span[F])

    def getHistory(root: Blake2b256Hash, path: String): F[HistoryType[F]] =
      Settings.typeHistory match {
        case "MergingHistory" => createMergingHistory[F].create(root, path)
        case "RadixHistory"   => createRadixHistory[F].create(root, path)
      }

    def fill32Bytes(s: String): Array[Byte] = {
      val prefix = (0 to 31 - (s.length / 2)).map(_ => "11").mkString
      Base16.unsafeDecode(prefix + s)
    }

    val v0: Blake2b256Hash = Blake2b256Hash.fromByteArray(fill32Bytes("00"))

    def experiment(numInit: Int, numInsReadDel: Int): F[Unit] = {

      val step = 3

      def genInitTasks = {
        val k      = step // step
        val genMax = numInit * k
        val r      = scala.util.Random
        (1 until genMax by k).iterator.map { i =>
          val value: Long = if (Settings.random) r.nextLong() else i.toLong
          val bytes       = BigInteger.valueOf(value).toByteArray
          Blake2b256Hash.create(bytes)
        }
      }

      def genTasks = {
        val k      = step // step
        val genMax = numInsReadDel * k
        val r      = scala.util.Random
        (0 until genMax by k).iterator.map { i =>
          val value: Long = if (Settings.random) r.nextLong() else i.toLong
          val bytes       = BigInteger.valueOf(value).toByteArray
          Blake2b256Hash.create(bytes)
        }
      }

      def readAndVerify(h: History[F], tasks: List[Blake2b256Hash]) =
        tasks.traverse { t =>
          h.read(t.bytes).map(readVal => assert(readVal.contains(t.bytes), "Test read not passed"))
        }

      def calcSizeBytesAndNumRecords(h: HistoryType[F]): Option[(Long, Int)] =
        if (Settings.flagSize) h match {
          case HistoryWithFunc(_, _, sizeBytes, numRecords) => (sizeBytes(), numRecords()).some
          case HistoryWithoutFunc(_, _)                     => (0L, 0).some
        } else none

      def export(
          rootHash: Blake2b256Hash,
          skipSize: Int,
          getNodeDataFromStore: ByteVector => F[Option[ByteVector]]
      ): F[ExportData] = {
        import coop.rchain.rspace.history.RadixTree._
        if (Settings.typeHistory == "RadixHistory") {
          val exportSettings = ExportDataSettings(
            expNP = false,
            expNK = true,
            expNV = true,
            expLP = false,
            expLV = true
          )
          sequentialExport[F](
            rootHash.bytes,
            None,
            skipSize,
            1000000,
            getNodeDataFromStore,
            exportSettings
          ).map(_._1)
        } else
          ExportData(Seq(), Seq(), Seq(), Seq(), Seq()).pure
      }

      def validation(
          rootHash: Blake2b256Hash,
          expData: ExportData
      ): F[Unit] = {
        import coop.rchain.rspace.history.RadixTree._

        val ExportData(_, nodeKVDBKeys, nodeKVDBValues, _, _) = expData

        val KeysForValid = nodeKVDBValues.map { bytes =>
          import coop.rchain.crypto.hash.Blake2b256
          ByteVector(Blake2b256.hash(bytes))
        }
        assert(KeysForValid == nodeKVDBKeys, "Error 1 of validation")

        val localStorage = (nodeKVDBKeys zip nodeKVDBValues).toMap
        for {
          expDataForValid <- export(rootHash, 0, localStorage.get(_).pure)
          _               <- assert(expDataForValid == expData, "Error 2 of validation").pure
        } yield ()
      }

      val result =
        (0 until (Settings.averageNum + Settings.averageWarmUp)).toList
          .foldM((0L, 0L, 0L, 0L, 0L, 0, (0L, 0), (0L, 0), (0L, 0))) {
            case (initData, i) =>
              def statistic(
                  timeI: Long,
                  timeR: Long,
                  timeD: Long,
                  timeExp: Long,
                  timeValid: Long,
                  numNode: Int,
                  sizeInit: (Long, Int),
                  sizeI: (Long, Int),
                  sizeD: (Long, Int)
              ): Unit = {
                def iI(v: Int)   = "%10d) ".format(v)
                def num(v: Int)  = "%10d ".format(v)
                def sec(v: Long) = "%10.3f ".format(v.toDouble / 1000)
                def MB(v: (Long, Int)) =
                  "[%10.3f  %10d]".format(v._1.toDouble / (1024 * 1024), v._2)

                val strSize = if (Settings.flagSize) MB(sizeInit) + MB(sizeI) + MB(sizeD) else ""

                val str = iI(i) + num(numInit) + num(numInsReadDel) + sec(timeI) + sec(timeR) +
                  sec(timeD) + sec(timeExp) + sec(timeValid) + num(numNode) + strSize
                println(str)
              }

              val initHashes        = genInitTasks.toList
              val initInsertActions = initHashes.map(x => InsertAction(x.bytes.toArray.toList, x))

              val insReadDelHashes = genTasks.toList
              val insertActions    = insReadDelHashes.map(x => InsertAction(x.bytes.toArray.toList, x))
              val deleteActions    = insReadDelHashes.map(x => DeleteAction(x.bytes.toArray.toList))

              for {
                historyInitW <- getHistory(v0, "/git/temp2")
                historyInit  <- getHistory(v0, "/git/temp")

                history1W <- historyInitW.history.process(initInsertActions)
                history1  <- historyInit.history.process(initInsertActions)

                size0 <- Sync[F].delay(calcSizeBytesAndNumRecords(historyInit))

                history2W             <- history1W.process(insertActions)
                temp                  <- nsTime(history1.process(insertActions))
                (history2, timeITemp) = temp

                size1 <- Sync[F].delay(calcSizeBytesAndNumRecords(historyInit))

                _              <- readAndVerify(history2W, insReadDelHashes)
                temp           <- nsTime(readAndVerify(history2, insReadDelHashes))
                (_, timeRTemp) = temp

                expDataW               <- export(history2W.root, 0, historyInitW.getNodeDataFromStore)
                temp                   <- nsTime(export(history2.root, 0, historyInit.getNodeDataFromStore))
                (expData, timeExpTemp) = temp

                numNodeTemp = expData.nK.size

                _                  <- validation(history2W.root, expDataW)
                temp               <- nsTime(validation(history2.root, expData))
                (_, timeValidTemp) = temp

                _                    <- history2W.process(deleteActions)
                temp                 <- nsTime(history2.process(deleteActions))
                (history3, timeDExp) = temp

                size2 <- Sync[F].delay(calcSizeBytesAndNumRecords(historyInit))

                _ <- Sync[F]
                      .delay(assert(history3.root == history1.root, "Test delete not passed"))
                      .whenA(Settings.deleteTest)

                _ = if (Settings.averageStatistic)
                  statistic(
                    timeITemp,
                    timeRTemp,
                    timeDExp,
                    timeExpTemp,
                    timeValidTemp,
                    numNodeTemp,
                    size0.getOrElse((0L, 0)),
                    size1.getOrElse((0L, 0)),
                    size2.getOrElse((0L, 0))
                  )
                (timeI, timeR, timeD, timeExp, timeValid, numNode, sizeInit, sizeI, sizeD) = initData
              } yield
                if (i < Settings.averageWarmUp)
                  (timeI, timeR, timeD, timeExp, timeValid, numNode, sizeInit, sizeI, sizeD)
                else
                  (
                    timeI + timeITemp,
                    timeR + timeRTemp,
                    timeD + timeDExp,
                    timeExp + timeExpTemp,
                    timeValid + timeValidTemp,
                    numNode + numNodeTemp,
                    (
                      sizeInit._1 + size0.getOrElse(0L, 0)._1,
                      sizeInit._2 + size0.getOrElse(0L, 0)._2
                    ),
                    (sizeI._1 + size1.getOrElse(0L, 0)._1, sizeI._2 + size1.getOrElse(0L, 0)._2),
                    (sizeD._1 + size2.getOrElse(0L, 0)._1, sizeD._2 + size2.getOrElse(0L, 0)._2)
                  )
          }

      result.map { x =>
        def num(v: Int)        = "%10d, ".format(v)
        def numAverage(v: Int) = "%10d, ".format(v / Settings.averageNum)
        def sec(v: Long)       = "%10.3f, ".format(v.toDouble / (1000 * Settings.averageNum))
        def MB(v: (Long, Int)) =
          "%10.3f,  %10.0f,".format(
            v._1.toDouble / (1024 * 1024 * Settings.averageNum),
            v._2.toDouble / Settings.averageNum
          )

        val (timeI, timeR, timeD, timeExp, timeValid, numNode, sizeInit, sizeI, sizeD) = x
        val strSize                                                                    = if (Settings.flagSize) MB(sizeInit) + MB(sizeI) + MB(sizeD) else ""
        val str = num(numInit) + num(numInsReadDel) + sec(timeI) + sec(timeR) + sec(timeD) + sec(
          timeExp
        ) + sec(timeValid) + numAverage(numNode) + strSize
        println(str)
      }
    }

    def fS(v: String): String = "%10s, ".format(v)
    val test: F[Unit] = {
      for {
        _ <- println(Settings.typeHistory).pure

        strSize = if (Settings.flagSize)
          fS("[sizeInit(MB)") + fS("numInit]") + fS("[sizeI(MB)") +
            fS("numI]") + fS("[sizeD(MB)") + fS("numD]")
        else ""
        str = fS("numInit") + fS("numIRD") + fS("timeI(sec)") + fS("timeR(sec)") + fS("timeD(sec)") + fS(
          "timeExp(sec)"
        ) + fS("timeValid(sec)") + fS("numNode") + strSize
        _ <- println(str).pure
        _ <- Settings.taskCur.traverse(x => experiment(x.initNum, x.insReadDelNum).map(x => x))
      } yield ()
    }

  }

  it should "execute with monix" ignore {
    import monix.eval.Task
    import monix.execution.Scheduler.Implicits.global

    implicit val log: Log.NOPLog[Task]         = new Log.NOPLog[Task]()
    implicit val met: Metrics.MetricsNOP[Task] = new Metrics.MetricsNOP[Task]()
    implicit val spn: NoopSpan[Task]           = new NoopSpan[Task]()

    val t = new Experiment[Task]
    t.test.runSyncUnsafe()
  }
}
