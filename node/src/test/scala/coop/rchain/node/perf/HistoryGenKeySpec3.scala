package coop.rchain.node.perf

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.instances.{RadixHistory, RadixHistory3}
import coop.rchain.rspace.history.{DeleteAction, History, InsertAction}
import coop.rchain.shared.Log
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.ByteVector

import java.math.BigInteger
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext

class HistoryGenKeySpec3 extends FlatSpec with Matchers {

  class TestCase[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span](
      implicit ectx: ExecutionContext
  ) extends HistoryHelpers[F] {
    override def init: Deps =
      (ectx, Concurrent[F], ContextShift[F], Parallel[F], Log[F], Metrics[F], Span[F])

    case class S(name: String, genCount: Int)
//options
//    val typeHistory: String = "tgrospic"
    val typeHistory: String           = "dharsh"
    val readTest: Boolean             = true
    val deleteCorrectnesTest: Boolean = true

    def setup1 =
      List(
        // Generate pairs
//        S("gen-10", 10),
//        S("gen-10", 20)
        S("gen-50k", 50000),
        S("gen-100k", 100000),
        S("gen-150k", 150000),
        S("gen-200k", 200000),
        S("gen-250k", 250000),
        S("gen-300k", 300000),
        S("gen-350k", 350000),
        S("gen-400k", 400000),
        S("gen-450k", 450000),
        S("gen-500k", 500000)
      ).traverse {
        case S(_, genCount) =>
          def k(s: String) = {
            val prefix = (0 to 31 - (s.size / 2)).map(_ => "11").mkString
            Base16.unsafeDecode(prefix + s)
          }

          val v = Blake2b256Hash.fromByteArray(k("99"))

          def genTasks = {
            val k      = 2 // step
            val genMax = genCount * k
            (0 until genMax by k).iterator.map { i =>
              val bytes = BigInteger.valueOf(i.toLong).toByteArray
              Blake2b256Hash.create(bytes)
            }
          }

          def genInsertTasks(tasks: List[Blake2b256Hash]) =
            tasks.map { t =>
              InsertAction(t.bytes.toArray.toList, t)
            }

          def genReadTasks(tasks: List[Blake2b256Hash]) =
            tasks.map { t =>
              t.bytes
            }

          def readAndVerify(tasks: List[ByteVector], h: History[F]) =
            tasks.traverse { t =>
              h.read(t).map { leafValue =>
                {
                  assert(leafValue.contains(t), "Test read not passed")
                  leafValue
                }
              }
            }

          def genDeleteTasks(tasks: List[Blake2b256Hash]) =
            tasks.map { t =>
              DeleteAction(t.bytes.toArray.toList)
            }

          def saveGroups(tasks: List[List[Blake2b256Hash]], h: History[F]): F[History[F]] =
            tasks match {
              case List() =>
                h.pure[F]
              case next +: tail =>
                for {
                  insertTasks <- Sync[F].delay(genInsertTasks(next))
                  hh          <- h.process(insertTasks)
                  r           <- saveGroups(tail, hh)
                } yield r
            }

          def readGroups(tasks: List[List[Blake2b256Hash]], h: History[F]): F[History[F]] =
            tasks match {
              case List() =>
                h.pure[F]
              case next +: tail =>
                for {
                  readTasks <- Sync[F].delay(genReadTasks(next))
                  _         <- readAndVerify(readTasks, h)
                  r         <- readGroups(tail, h)
                } yield r
            }

          def deleteGroups(tasks: List[List[Blake2b256Hash]], h: History[F]): F[History[F]] =
            tasks match {
              case List() =>
                h.pure[F]
              case next +: tail =>
                for {
                  deleteTasks <- Sync[F].delay(genDeleteTasks(next))
                  hh          <- h.process(deleteTasks)
                  r           <- deleteGroups(tail, hh)
                } yield r
            }

          val saveHistory = for {
            tasks <- Sync[F].delay(genTasks.toList)

            // Save all with history process
            radixStore = TrieMap[ByteVector, ByteVector]()

            historySave = typeHistory match {
              case "tgrospic" => RadixHistory(v, radixStore)
              case "dharsh"   => RadixHistory3(v, radixStore)
            }
            initilalHistory <- historySave.process(InsertAction(v.bytes.toArray.toList, v) :: Nil)

            _ <- shortLog(s"${tasks.size} ")

            sizeInGroup = 100000
            saveAll     = saveGroups(tasks.grouped(sizeInGroup).toList, initilalHistory)
            _           <- saveAll //warm up
            saveHistory <- msTime(saveAll)

            _ <- shortLog(" ")

            readAll = readGroups(tasks.grouped(sizeInGroup).toList, saveHistory)
            _ <- {
              for {
                _ <- readAll //warm up
                _ <- msTime(readAll)
                _ <- shortLog(" ")
              } yield ()
            }.whenA(readTest)

            deleteAll     = deleteGroups(tasks.grouped(sizeInGroup).toList, saveHistory)
            _             <- deleteAll //warm up
            deleteHistory <- msTime(deleteAll)

            _ <- shortLog("\n")

            _ <- Sync[F]
                  .delay(
                    assert(initilalHistory.root == deleteHistory.root, "Test delete not passed")
                  )
                  .whenA(deleteCorrectnesTest)
            // Results
            result = (tasks, deleteHistory.root)

          } yield result

          for {
            res          <- saveHistory
            (_, newRoot) = res
          } yield ()
      }

    val setup =
      for {
        _ <- log(s"Type History: $typeHistory")
        _ <- log(s"")
        _ <- log(s"[Num record] [insert(ms)] [read(ms)] [delete(ms)]")
//        _ <- log(s"[Num record] [insert(ms)] [delete(ms)]")
        _ <- setup1
      } yield ()
  }

  it should "execute with monix" in {
    import monix.eval.Task
    import monix.execution.Scheduler.Implicits.global

    implicit val log = new Log.NOPLog[Task]()
    implicit val met = new Metrics.MetricsNOP[Task]()
    implicit val spn = new NoopSpan[Task]()

    val t = new TestCase[Task]
    t.setup.runSyncUnsafe()
  }
}
