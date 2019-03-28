package coop.rchain.rspace

import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import cats.effect.concurrent.{MVar, Ref}
import cats.effect.implicits._
import cats.mtl._
import coop.rchain.rspace.examples.StringExamples._
import coop.rchain.rspace.examples.StringExamples.implicits._
import coop.rchain.rspace.internal._
import org.scalatest._

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

trait HotStoreSpec[F[_]] extends FlatSpec with Matchers {

  implicit def M: Monad[F]

  def fixture(
      f: (
          MonadState[F, Cache[String, Pattern, StringsCaptor]],
          History[F],
          HotStore[F, String, Pattern, StringMatch, StringsCaptor]
      ) => F[Unit]
  ): Unit

  val channels = List("ch1")
  val patterns = Seq[Pattern](StringMatch("val"))
  val continuations =
    List(WaitingContinuation.create(channels, patterns, new StringsCaptor(), false))

  "getContinuations when cache is empty" should "read data from history and put into the cache" in fixture {
    (state, history, hotStore) =>
      {
        for {
          _                 <- history.putContinuations(channels, continuations)
          cache             <- state.inspect(identity)
          _                 = cache.continuations shouldBe empty
          readContinuations <- hotStore.getContinuations(channels)
          cache             <- state.inspect(identity)
          _                 = cache.continuations(channels) shouldEqual continuations
        } yield (readContinuations shouldEqual continuations)
      }
  }

  "getContinuations when cache contains data" should "read data from cache ignoring history" in fixture {
    (state, history, hotStore) =>
      {
        val cachedContinuations =
          List(WaitingContinuation.create(List("ch2"), patterns, new StringsCaptor(), false))
        for {
          _ <- history.putContinuations(channels, continuations)
          _ <- state.modify(
                _ =>
                  Cache(
                    continuations = Map(
                      channels -> List(
                        WaitingContinuation
                          .create(List("ch2"), patterns, new StringsCaptor(), false)
                      )
                    )
                  )
              )
          readContinuations <- hotStore.getContinuations(channels)
          cache             <- state.get
          _                 = cache.continuations(channels) shouldEqual cachedContinuations
        } yield (readContinuations shouldEqual cachedContinuations)
      }
  }
}

import coop.rchain.rspace.internal.{Datum, WaitingContinuation}

final case class HistoryState(
    continuations: Map[List[String], List[WaitingContinuation[Pattern, StringsCaptor]]] = Map.empty
)

class History[F[_]: Monad](implicit R: Ref[F, HistoryState])
    extends HistoryReader[F, String, Pattern, StringMatch, StringsCaptor] {

  def getJoins(channel: String): F[List[List[String]]]      = ???
  def getData(channel: String): F[List[Datum[StringMatch]]] = ???

  def getContinuations(
      channels: List[String]
  ): F[List[WaitingContinuation[Pattern, StringsCaptor]]] = R.get.map(_.continuations(channels))

  def putContinuations(
      channels: List[String],
      continuations: List[WaitingContinuation[Pattern, StringsCaptor]]
  ): F[Unit] = R.modify { prev =>
    (prev.copy(continuations = prev.continuations.+(channels -> continuations)), ())
  }
}

class InMemHotStoreSpec extends HotStoreSpec[Task] {

  type F[A] = Task[A]
  override implicit val M: Monad[F] = implicitly[Concurrent[Task]]


  private[rspace] implicit def liftToMonadState[V](
      state: MVar[F, V]
  ): MonadState[F, V] =
    new MonadState[F, V] {
      val monad: cats.Monad[F] = implicitly[Monad[F]]
      def get: F[V]            = state.take
      def set(s: V): F[Unit]   = state.put(s)

      override def inspect[A](f: V => A): F[A] = state.read.map(f)
      override def modify(f: V => V): F[Unit] =
        for {
          current <- state.take
          _       <- state.put(f(current))
        } yield ()
    }

  override def fixture(
      f: (
          MonadState[F, Cache[String, Pattern, StringsCaptor]],
          History[F],
          HotStore[F, String, Pattern, StringMatch, StringsCaptor]
      ) => F[Unit]
  ) =
    (for {
      historyState <- Ref.of[F, HistoryState](HistoryState())
      history = {
        implicit val hs = historyState
        new History[F]
      }
      mvarCache <- MVar.of[Task, Cache[String, Pattern, StringsCaptor]](
                    Cache(
                      Map.empty[List[String], List[WaitingContinuation[Pattern, StringsCaptor]]]
                    )
                  )
      hotStore = {
        implicit val hr                                                          = history
        implicit val cache: MonadState[F, Cache[String, Pattern, StringsCaptor]] = mvarCache
        HotStore.inMem[Task, String, Pattern, StringMatch, StringsCaptor]
      }
      res <- f(mvarCache, history, hotStore)
    } yield res).runSyncUnsafe(1.second)

}
