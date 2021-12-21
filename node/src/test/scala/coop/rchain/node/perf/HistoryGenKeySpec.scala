package coop.rchain.node.perf
import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._

import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.HistoryInstances.{CachingHistoryStore, MergingHistory}
import coop.rchain.rspace.history.RadixTree.ExportData
import coop.rchain.rspace.history._
import coop.rchain.rspace.history.instances._
import coop.rchain.rspace.state.RSpaceExporter.traverseTrie
import coop.rchain.shared.{Base16, Log}
import coop.rchain.state.TrieNode
import coop.rchain.store.{InMemoryKeyValueStore, KeyValueStore, LmdbStoreManager}
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.ByteVector

import java.io.File
import java.math.BigInteger
import java.nio.file.Paths
import scala.concurrent.ExecutionContext
import scala.language.higherKinds
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

    val taskCur: List[ExpT] = tasksMedium0
  }

  case class ExpT(initNum: Int, insReadDelNum: Int)

  val tasksSmall: List[ExpT] = List(ExpT(1, 20000))

  val tasksMedium0: List[ExpT] = List(
    ExpT(1, 300),
    ExpT(1, 1000),
    ExpT(1, 5000),
    ExpT(1, 10000),
    ExpT(1, 30000),
    ExpT(1, 100000),
    ExpT(1, 200000),
    ExpT(1, 300000),
    ExpT(1, 400000)
  )

  val tasksMedium1: List[ExpT] = List(
    ExpT(1, 5000),
    ExpT(300, 5000),
    ExpT(1000, 5000),
    ExpT(5000, 5000),
    ExpT(10000, 5000),
    ExpT(30000, 5000)
  )

  val tasksLarge0: List[ExpT] = List(
    ExpT(1, 1000),
    ExpT(1, 10000),
    ExpT(1, 50000),
    ExpT(1, 100000),
    ExpT(1, 200000)
//    ExpT(1, 300000),
//    ExpT(1, 400000),
//    ExpT(1, 500000)
//    ExpT(1, 600000)
//    ExpT(1, 700000),
//    ExpT(1, 800000),
//    ExpT(1, 900000),
//    ExpT(1, 1000000)
  )

  val tasksLarge1: List[ExpT] = List(
//    ExpT(100000, 100000),
//    ExpT(200000, 100000),
//    ExpT(300000, 100000),
//    ExpT(400000, 100000),
//    ExpT(500000, 100000),
//    ExpT(600000, 100000),
//    ExpT(700000, 100000),
//    ExpT(800000, 100000),
//    ExpT(900000, 100000),
    ExpT(1000000, 100000)
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
          val radixStore = new RadixStore(inMemoStore)
          for { history <- RadixHistory[F](root, radixStore) } yield HistoryWithFunc(
            history,
            radixStore.get1,
            inMemoStore.sizeBytes,
            inMemoStore.numRecords
          )
        case "lmdb" =>
          for {
            store      <- storeLMDB(lmdbPath)
            radixStore = new RadixStore(store)
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
    val v1: Blake2b256Hash = Blake2b256Hash.fromByteArray(fill32Bytes("01"))
    val v2: Blake2b256Hash = Blake2b256Hash.fromByteArray(fill32Bytes("02"))
    val v3: Blake2b256Hash = Blake2b256Hash.fromByteArray(fill32Bytes("03"))
    val v4: Blake2b256Hash = Blake2b256Hash.fromByteArray(fill32Bytes("04"))
    val v5: Blake2b256Hash = Blake2b256Hash.fromByteArray(fill32Bytes("05"))

    def simpleExperiment(): F[Unit] =
      for {
        historyInit <- getHistory(v0, "/git/temp")
        history1    <- historyInit.history.process(InsertAction(v1.bytes.toArray, v1) :: Nil)
        v1NewOpt    <- history1.read(v1.bytes)
        _           <- assert(v1NewOpt.get == v1.bytes, "error").pure
        history2    <- history1.process(InsertAction(v2.bytes.toArray, v2) :: Nil)
        v2NewOpt    <- history2.read(v2.bytes)
        _           <- assert(v2NewOpt.get == v2.bytes, "error").pure
        history3    <- history2.process(DeleteAction(v2.bytes.toArray) :: Nil)
        _           <- assert(history3.root == history1.root, "error").pure

      } yield ()

    def strToSeq(s: String): Seq[Byte] =
      Base16.unsafeDecode(s)

    def simpleExperimentWithSubTree(): F[Unit] =
      for {
        historyInit          <- getHistory(v0, "/git/temp")
        prefix0: Array[Byte] = Array(0x01.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
        prefix1: Array[Byte] = Array(0x01.toByte, 0x00.toByte, 0x01.toByte, 0x00.toByte)
        prefix2: Array[Byte] = Array(0x01.toByte, 0x00.toByte, 0x01.toByte)
        prefix3: Array[Byte] = Array(0x02.toByte)
        prefix4: Array[Byte] = Array(0x00.toByte, 0x00.toByte, 0x04.toByte, 0x04.toByte)
        prefix5: Array[Byte] = Array(0x00.toByte, 0x00.toByte, 0x05.toByte)
//        prefix5: Array[Byte] = Array(0x00.toByte, 0x00.toByte, 0x04.toByte) //error case

        history1 <- historyInit.history.process(
                     InsertAction(prefix0, v0)
                       :: InsertAction(prefix1, v1)
                       :: InsertAction(prefix2, v2)
                       :: InsertAction(prefix3, v3)
                       :: Nil
                   )

        v0NewOpt <- history1.read(ByteVector(prefix0))
        v1NewOpt <- history1.read(ByteVector(prefix1))
        v2NewOpt <- history1.read(ByteVector(prefix2))
        v3NewOpt <- history1.read(ByteVector(prefix3))
        _ <- assert(
              (v0NewOpt.get == v0.bytes) && (v1NewOpt.get == v1.bytes) && (v2NewOpt.get == v2.bytes) && (v3NewOpt.get == v3.bytes),
              "error1"
            ).pure

        history2 <- history1.process(
                     InsertAction(prefix4, v4)
                       :: InsertAction(prefix5, v5)
                       :: Nil
                   )
        v4NewOpt <- history2.read(ByteVector(prefix4))
        v5NewOpt <- history2.read(ByteVector(prefix5))
        _ <- assert(
              (v4NewOpt.get == v4.bytes) && (v5NewOpt.get == v5.bytes),
              "error2"
            ).pure

        history3 <- history2.process(
                     DeleteAction(prefix4)
                       :: DeleteAction(prefix5)
                       :: Nil
                   )
        _ <- assert(history3.root == history1.root, "error3").pure

      } yield ()

    def exportExperiment(): F[Unit] = {
      import coop.rchain.rspace.history.RadixTree._
      val lmdbPath: String     = "/git/temp"
      val root: Blake2b256Hash = v0

      for {
        store      <- storeLMDB(lmdbPath)
        radixStore = new RadixStore(store)
        history    <- RadixHistory[F](root, radixStore)

        prefix0: Array[Byte] = Array(0x00.toByte, 0x00.toByte, 0x00.toByte, 0x00.toByte)
        prefix1: Array[Byte] = Array(0x01.toByte, 0x01.toByte, 0x00.toByte, 0x00.toByte)
        prefix2: Array[Byte] = Array(0x01.toByte, 0x01.toByte, 0x01.toByte, 0x00.toByte)
        prefix3: Array[Byte] = Array(0x01.toByte, 0x02.toByte, 0x00.toByte, 0x00.toByte)
        prefix4: Array[Byte] = Array(0x02.toByte, 0x01.toByte, 0x00.toByte, 0x00.toByte)
        prefix5: Array[Byte] = Array(0x02.toByte, 0x02.toByte, 0x00.toByte, 0x00.toByte)

        history1 <- history.process(
                     InsertAction(prefix0, v0)
                       :: InsertAction(prefix1, v1)
                       :: InsertAction(prefix2, v2)
                       :: InsertAction(prefix3, v3)
                       :: InsertAction(prefix4, v4)
                       :: InsertAction(prefix5, v5)
                       :: Nil
                   )

        v0NewOpt <- history1.read(ByteVector(prefix0))
        v1NewOpt <- history1.read(ByteVector(prefix1))
        v2NewOpt <- history1.read(ByteVector(prefix2))
        v3NewOpt <- history1.read(ByteVector(prefix3))
        v4NewOpt <- history1.read(ByteVector(prefix4))
        v5NewOpt <- history1.read(ByteVector(prefix5))
        _ <- assert(
              (v0NewOpt.get, v1NewOpt.get, v2NewOpt.get, v3NewOpt.get, v4NewOpt.get, v5NewOpt.get) ==
                (v0.bytes, v1.bytes, v2.bytes, v3.bytes, v4.bytes, v5.bytes),
              "error1"
            ).pure

        rootHash: ByteVector               = history1.root.bytes
        initLastPrefix: Option[ByteVector] = None
//        lastPrefix: Option[ByteVector] = Some(ByteVector(0x02.toByte))
        settings = ExportDataSettings(
          expNP = true,
          expNK = true,
          expNV = true,
          expLP = true,
          expLV = true
        )

        dataAll <- sequentialExport(
                    rootHash,
                    initLastPrefix,
                    0,
                    1000,
                    radixStore.get1,
                    settings
                  )

        data1 <- sequentialExport(
                  rootHash,
                  initLastPrefix,
                  0,
                  2,
                  radixStore.get1,
                  settings
                )
        (d1, pr1) = data1

        data2 <- sequentialExport(
                  rootHash,
                  pr1,
                  0,
                  1000,
                  radixStore.get1,
                  settings
                )
        (d2, pr2) = data2

        prefabData = (
          ExportData(
            d1.nP ++ d2.nP,
            d1.nK ++ d2.nK,
            d1.nV ++ d2.nV,
            d1.lP ++ d2.lP,
            d1.lV ++ d2.lV
          ),
          pr2
        )

        r = {
          assert(prefabData == dataAll, "Error prefabrication")
          ()
        }

        (
          ExportData(_, nodeKVDBKeys, nodeKVDBValues, _, _),
          lastPrefix
        )            = prefabData
        localStorage = (nodeKVDBKeys zip nodeKVDBValues).toMap

        newData <- {
          def newGet(key: ByteVector) = localStorage.get(key).pure
          sequentialExport(
            rootHash,
            initLastPrefix,
            0,
            1000000,
            newGet,
            settings
          )
        }
        _ = {
          assert(newData == dataAll, "Error of validation")
          ()
        }

        importHash = history1.root
        startPath1 = Vector((importHash, None))
        nodesAll   <- traverseTrie(startPath1, 0, 100, radixStore.get1)
        nodes1     <- traverseTrie(startPath1, 0, 2, radixStore.get1)
        startPath2 = nodes1.last.path
        nodes2     <- traverseTrie(startPath2, 0, 100, radixStore.get1)
//        nodes2 <- traverseTrie(startPath1, 2, 100, radixStore.get1)

        (leafsAll, nonLeafsAll) = nodesAll.partition(_.isLeaf)
        (leafs1, nonLeafs1)     = nodes1.partition(_.isLeaf)
        (leafs2, nonLeafs2)     = nodes2.partition(_.isLeaf)

        leafsFabrication = leafs1 ++ leafs2

        last = nonLeafs1.last
        nonLeafsVabrication = nonLeafs1.dropRight(1) ++
          Vector(TrieNode(last.hash, last.isLeaf, Vector())) ++ nonLeafs2
        _ = {
          assert(
            leafsFabrication == leafsAll && nonLeafsVabrication == nonLeafsAll,
            "Error of validation 2"
          )
          ()
        }
        historyItems = nonLeafsAll.map(_.hash) zip nodeKVDBValues

        //required correct dataItems
//        def getBlake(x: Blake2b256Hash, store: RadixStore[F]) = store.get1(x.bytes)
//        dataItems    = Vector()
//        _ <- RSpaceImporter.validateStateItems(
//              historyItems,
//              dataItems,
//              startPath1,
//              20000,
//              0,
//              getBlake(_, radixStore)
//            )
      } yield ()
    }

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

//        _ <- simpleExperiment()
//        _ <- simpleExperimentWithSubTree()
//        _ <- exportExperiment()
        _ <- Settings.taskCur.traverse(x => experiment(x.initNum, x.insReadDelNum).map(x => x))
      } yield ()
    }

  }

  it should "execute with monix" in {
    import monix.eval.Task
    import monix.execution.Scheduler.Implicits.global

    implicit val log: Log.NOPLog[Task]         = new Log.NOPLog[Task]()
    implicit val met: Metrics.MetricsNOP[Task] = new Metrics.MetricsNOP[Task]()
    implicit val spn: NoopSpan[Task]           = new NoopSpan[Task]()

    val t = new Experiment[Task]
    t.test.runSyncUnsafe()
  }
}
