package coop.rchain.rspace

import java.nio.file.{Files, Path, Paths}

import cats._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.Ref
import cats.Parallel
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.{Metrics, Span}
import coop.rchain.rspace._
import coop.rchain.rspace.examples.StringExamples
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history._
import coop.rchain.rspace.history.{HistoryRepository, HistoryRepositoryInstances}
import coop.rchain.shared.PathOps._
import coop.rchain.shared.{Log, Serialize}
import org.scalatest._

import scodec.Codec

import scala.concurrent.ExecutionContext.Implicits.global
import monix.eval._
import monix.execution.atomic.AtomicAny
import coop.rchain.metrics.NoopSpan
import coop.rchain.store.InMemoryStoreManager

trait StorageTestsBase[F[_], C, P, A, K] extends FlatSpec with Matchers with OptionValues {
  type T    = ISpace[F, C, P, A, K]
  type ST   = HotStore[F, C, P, A, K]
  type HR   = HistoryRepository[F, C, P, A, K]
  type AtST = AtomicAny[ST]

  implicit def concurrentF: Concurrent[F]
  implicit def parF: Parallel[F]
  implicit def logF: Log[F]
  implicit def metricsF: Metrics[F]
  implicit def spanF: Span[F]
  implicit def monadF: Monad[F]
  implicit def contextShiftF: ContextShift[F]

  val logger: Logger = Logger(this.getClass.getName.stripSuffix("$"))

  override def withFixture(test: NoArgTest): Outcome = {
    logger.debug(s"Test: ${test.name}")
    super.withFixture(test)
  }

  /** A fixture for creating and running a test with a fresh instance of the test store.
    */
  def fixture[R](f: (ST, AtST, T) => F[R]): R

  def fixtureNonF[R](f: (ST, AtST, T) => R): R =
    fixture((st: ST, ast: AtST, t: T) => concurrentF.delay(f(st, ast, t)))

  def run[S](f: F[S]): S

  protected def setupTestingSpace[S, STORE](
      createISpace: (HR, ST) => F[(ST, AtST, T)],
      f: (ST, AtST, T) => F[S]
  )(
      implicit codecC: Codec[C],
      codecP: Codec[P],
      codecA: Codec[A],
      codecK: Codec[K],
      serializedC: Serialize[C]
  ): S = {

    val kvm = InMemoryStoreManager[F]

    run(for {
      roots    <- kvm.store("roots")
      cold     <- kvm.store("cold")
      history  <- kvm.store("history")
      channels <- kvm.store("channels")
      historyRepository <- HistoryRepositoryInstances
                            .lmdbRepository[F, C, P, A, K](roots, cold, history, channels)
      cache                <- Ref.of[F, Cache[C, P, A, K]](Cache[C, P, A, K]())
      testStore            = HotStore.inMem[F, C, P, A, K](Sync[F], cache, historyRepository, codecK)
      spaceAndStore        <- createISpace(historyRepository, testStore)
      (store, atom, space) = spaceAndStore
      res                  <- f(store, atom, space)
    } yield {
      res
    })
  }
}

import cats._
import cats.effect._
import coop.rchain.metrics.Metrics
import coop.rchain.shared.Log

trait TaskTests[C, P, A, R, K] extends StorageTestsBase[Task, C, P, R, K] {
  import coop.rchain.catscontrib.TaskContrib._
  import scala.concurrent.ExecutionContext

  implicit override val concurrentF: Concurrent[Task] =
    new monix.eval.instances.CatsConcurrentEffectForTask()(
      monix.execution.Scheduler.Implicits.global,
      Task.defaultOptions
    )
  implicit val logF: Log[Task]         = Log.log[Task]
  implicit val metricsF: Metrics[Task] = new Metrics.MetricsNOP[Task]()
  implicit val spanF: Span[Task]       = NoopSpan[Task]()

  implicit override val monadF: Monad[Task] = concurrentF
  implicit override val contextShiftF: ContextShift[Task] = new ContextShift[Task] {
    override def shift: Task[Unit] =
      Task.shift
    override def evalOn[B](ec: ExecutionContext)(fa: Task[B]): Task[B] =
      Task.shift(ec).bracket(_ => fa)(_ => Task.shift)
  }

  override def run[RES](f: Task[RES]): RES =
    f.unsafeRunSync(monix.execution.Scheduler.Implicits.global)
}

abstract class InMemoryHotStoreTestsBase[F[_]]
    extends StorageTestsBase[F, String, Pattern, String, StringsCaptor]
    with BeforeAndAfterAll {

  implicit val patternCodec: Codec[Pattern] =
    StringExamples.implicits.patternSerialize.toSizeHeadCodec
  implicit val stringCodec: Codec[String] = StringExamples.implicits.stringSerialize.toSizeHeadCodec
  implicit val stringCaptorCodec: Codec[StringsCaptor] =
    StringExamples.implicits.stringClosureSerialize.toSizeHeadCodec

  override def fixture[S](f: (ST, AtST, T) => F[S]): S = {
    val creator: (HR, ST) => F[(ST, AtST, T)] =
      (hr, ts) => {
        val atomicStore = AtomicAny(ts)
        val space =
          new RSpace[F, String, Pattern, String, StringsCaptor](hr, atomicStore)
        Applicative[F].pure((ts, atomicStore, space))
      }
    setupTestingSpace(creator, f)
  }

  override def afterAll(): Unit =
    super.afterAll()
}
