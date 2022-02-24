package coop.rchain.rspace

import cats.effect._
import cats.effect.concurrent.Ref
import cats.syntax.all._
import cats.{Parallel, _}
import com.typesafe.scalalogging.Logger
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.rspace.RSpace.RSpaceStore
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.history.{HistoryRepository, HistoryRepositoryInstances}
import coop.rchain.rspace.syntax._
import coop.rchain.shared.{Log, Serialize}
import coop.rchain.store.InMemoryStoreManager
import monix.eval._
import monix.execution.atomic.AtomicAny
import org.scalatest._

import scala.concurrent.ExecutionContext.Implicits.global

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
      implicit
      sc: Serialize[C],
      sp: Serialize[P],
      sa: Serialize[A],
      sk: Serialize[K]
  ): S = {

    val kvm = InMemoryStoreManager[F]

    run(for {
      stores                            <- kvm.rSpaceStores
      RSpaceStore(history, roots, cold) = stores
      historyRepository <- HistoryRepositoryInstances
                            .lmdbRepository[F, C, P, A, K](
                              history,
                              roots,
                              cold
                            )
      cache         <- Ref[F].of(HotStoreState[C, P, A, K]())
      historyReader <- historyRepository.getHistoryReader(historyRepository.root)
      testStore <- {
        val hr = historyReader.base
        HotStore[F, C, P, A, K](cache, hr)
      }
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
