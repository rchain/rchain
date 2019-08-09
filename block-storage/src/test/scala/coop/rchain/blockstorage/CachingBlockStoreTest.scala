package coop.rchain.blockstorage

import cats.effect.concurrent.Ref
import cats.implicits._
import coop.rchain.metrics.Metrics
import coop.rchain.models.blockImplicits._
import monix.eval.Task
import org.scalacheck.Gen
import org.scalatest._
import org.scalatest.Matchers
import org.scalatest.prop.GeneratorDrivenPropertyChecks

class CachingBlockStoreTest
    extends WordSpecLike
    with Matchers
    with OptionValues
    with GeneratorDrivenPropertyChecks {

  class CountingMetrics extends Metrics.MetricsNOP[Task] {
    val counterRef = Ref.unsafe[Task, Map[String, Long]](Map.empty)

    override def incrementCounter(name: String, delta: Long = 1)(
        implicit ev: Metrics.Source
    ): Task[Unit] =
      counterRef.update {
        _ |+| Map(name -> delta)
      }
  }

  case class TestEnvironment(
      blockStore: BlockStore[Task],
      cache: BlockStore[Task],
      metrics: CountingMetrics
  )

  def withCache(maxSize: Long = 256L * 1024L * 1024L)(f: TestEnvironment => Task[Unit]): Unit = {
    implicit val metrics = new CountingMetrics()

    FileLMDBIndexBlockStoreTest.withStore { blockStore =>
      for {
        cache <- CachingBlockStore(blockStore, maxSize)
        _     <- f(TestEnvironment(blockStore, cache, metrics))
      } yield ()
    }
  }

  "CachingBlockStore" when {
    "block is not cached" should {
      "get it from the underlying storage and cache it" in {
        forAll(blockElementGen) { block =>
          withCache() { e =>
            for {
              _              <- e.blockStore.put(block)
              b1             <- e.cache.get(block.blockHash)
              _              = b1.value shouldBe block
              b2             <- e.cache.get(block.blockHash)
              _              = b2.value shouldBe block
              metricsCounter <- e.metrics.counterRef.get
              _              = metricsCounter.getOrElse("get-miss", 0) shouldBe 1
              _              = metricsCounter.getOrElse("get-hit", 0) shouldBe 1
            } yield ()
          }
        }
      }
    }

    "block is being added" should {
      "cache it" in {
        forAll(blockElementGen) { block =>
          withCache() { e =>
            for {
              _              <- e.cache.put(block)
              b              <- e.cache.get(block.blockHash)
              _              = b.value shouldBe block
              metricsCounter <- e.metrics.counterRef.get
              _              = metricsCounter.getOrElse("get-miss", 0) shouldBe 0
              _              = metricsCounter.getOrElse("get-hit", 0) shouldBe 1
            } yield ()
          }
        }
      }

      "evict least recent used blocks" in {
        forAll(blockElementsGen, minSize(20), sizeRange(80)) { blocks =>
          whenever(blocks.size >= 20) {
            withCache(blocks.take(10).map(_.serializedSize).sum.toLong) { e =>
              val first :: others = blocks
              for {
                _              <- e.cache.put(first)
                b1             <- e.cache.get(first.blockHash) // Hit
                _              <- others.traverse_(e.cache.put)
                _              <- e.cache.get(others.last.blockHash) // Hit
                b2             <- e.cache.get(first.blockHash) // Miss
                _              = b1 shouldBe b2
                metricsCounter <- e.metrics.counterRef.get
                _              = metricsCounter.getOrElse("get-miss", 0) shouldBe 1
                _              = metricsCounter.getOrElse("get-hit", 0) shouldBe (1 + 1)
              } yield ()
            }
          }
        }
      }
    }
  }
}
