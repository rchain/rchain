package coop.rchain.rspace

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
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.{Async, IO, Ref}
import cats.effect.unsafe.implicits.global

trait StorageTestsBase[F[_], C, P, A, K] extends AnyFlatSpec with Matchers with OptionValues {
  type T    = ISpace[F, C, P, A, K]
  type ST   = HotStore[F, C, P, A, K]
  type HR   = HistoryRepository[F, C, P, A, K]
  type AtST = AtomicAny[ST]

  implicit def concurrentF: Async[F]
  implicit def parF: Parallel[F]
  implicit def logF: Log[F]
  implicit def metricsF: Metrics[F]
  implicit def spanF: Span[F]
  implicit def monadF: Monad[F]

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

trait IOTests[C, P, A, R, K] extends StorageTestsBase[IO, C, P, R, K] {
  implicit val concurrentF: Async[IO]    = IO.asyncForIO
  implicit val monadF: Monad[IO]         = Monad[IO]
  implicit val logF: Log[IO]             = Log.log[IO]
  implicit val metricsF: Metrics[IO]     = new Metrics.MetricsNOP[IO]()
  implicit val spanF: Span[IO]           = NoopSpan[IO]()
  override def run[RES](f: IO[RES]): RES = f.unsafeRunSync
}

abstract class InMemoryHotStoreTestsBase[F[_]]
    extends StorageTestsBase[F, String, Pattern, String, StringsCaptor]
    with BeforeAndAfterAll {

  override def fixture[S](f: (ST, AtST, T) => F[S]): S = {
    val creator: (HR, ST) => F[(ST, AtST, T)] =
      (hr, ts) => {
        val atomicStore = AtomicAny(ts)
        val space =
          new RSpace[F, String, Pattern, String, StringsCaptor](
            hr,
            atomicStore
          )
        Applicative[F].pure((ts, atomicStore, space))
      }
    setupTestingSpace(creator, f)
  }

  override def afterAll(): Unit =
    super.afterAll()
}
