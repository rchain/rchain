package coop.rchain.node.perf

import cats.Parallel
import cats.effect.{Concurrent, ContextShift, Sync}
import cats.syntax.all._
import coop.rchain.crypto.codec.Base16
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.history.instances.RadixHistory
import coop.rchain.rspace.history.{History, InsertAction}
import coop.rchain.shared.Log
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.ByteVector

import java.math.BigInteger
import scala.collection.concurrent.TrieMap
import scala.concurrent.ExecutionContext

class HistoryGenKeySpec extends FlatSpec with Matchers {

  class TestCase[F[_]: Concurrent: ContextShift: Parallel: Log: Metrics: Span](
      implicit ectx: ExecutionContext
  ) extends HistoryHelpers[F] {
    override def init: Deps =
      (ectx, Concurrent[F], ContextShift[F], Parallel[F], Log[F], Metrics[F], Span[F])

    case class S(name: String, genCount: Int)

    def setup1 =
      List(
        // Generate pairs
        S("gen-10k", 10000),
        S("gen-100k", 100000),
        S("gen-200k", 200000),
        S("gen-500k", 500000),
        S("gen-1M", 1000000),
        S("gen-2M", 2000000)
      ).traverse {
        case S(name, genCount) =>
          def k(s: String) = {
            val prefix = (0 to 31 - (s.size / 2)).map(_ => "11").mkString
            Base16.unsafeDecode(prefix + s)
          }

          val v = Blake2b256Hash.fromByteArray(k("99"))

          def genPairs = {
            val k      = 2 // step
            val genMax = genCount * k
            (0 until genMax by k).iterator.map { i =>
              val bytes = BigInteger.valueOf(i.toLong).toByteArray
              val h     = Blake2b256Hash.create(bytes)
              InsertAction(h.bytes.toArray.toList, v)
            }
          }

          def saveGroups(groups: List[List[InsertAction]], h: History[F]): F[History[F]] =
            groups match {
              case List() =>
                h.pure[F]
              case next +: tail =>
                for {
                  hh <- time(s"Saved chunk (${next.size}), left ${tail.size}")(h.process(next))
                  _  <- log(s"    root: ${hh.root}")
                  r  <- saveGroups(tail, hh)
                } yield r
            }

          val saveHistory = for {
            // Warmup
            _ <- time(s"Generate $genCount pairs")(Sync[F].delay(genPairs.toList))

            pairs <- time(s"Generate $genCount pairs")(Sync[F].delay(genPairs.toList))

            // Save all with history process
            radixStore  = TrieMap[ByteVector, ByteVector]()
            historySave = RadixHistory(v, radixStore)

            saveAll = saveGroups(pairs.grouped(3250000).toList, historySave)

            newHistory <- time(s"Saved ${pairs.size} generated pairs with history")(saveAll)

            // Results
            result = (pairs, newHistory.root)

          } yield result

          for {
            res          <- time(s"Save done, DB: $name")(saveHistory)
            (_, newRoot) = res
            _            <- log(s"  new root: $newRoot")
          } yield ()
      }

    val setup2 =
      for {
        _ <- log("")
        _ <- setup1
      } yield ()

    val setup = time("Total")(setup2)
  }

  it should "execute with monix" in {
    import monix.eval.Task
    import monix.execution.Scheduler.Implicits.global

    implicit val log = new Log.NOPLog[Task]() {
//      override def info(msg: => String)(implicit ev: LogSource): Task[Unit] =
//        Sync[Task].delay(println(msg))
    }
    implicit val met = new Metrics.MetricsNOP[Task]()
    implicit val spn = new NoopSpan[Task]()

    val t = new TestCase[Task]
    t.setup.runSyncUnsafe()
  }
}
