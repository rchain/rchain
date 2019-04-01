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
import coop.rchain.rspace.test.ArbitraryInstances._
import org.scalatest._
import org.scalatest.prop._

import scala.collection.immutable.Seq
import scala.concurrent.duration._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global

trait HotStoreSpec[F[_]] extends FlatSpec with Matchers with GeneratorDrivenPropertyChecks {

  implicit def M: Monad[F]

  type Channel      = String
  type Data         = Datum[String]
  type Continuation = WaitingContinuation[Pattern, StringsCaptor]

  def fixture(
      f: (
          MonadState[F, Cache[String, Pattern, String, StringsCaptor]],
          History[F],
          HotStore[F, String, Pattern, String, StringsCaptor]
      ) => F[Unit]
  ): Unit

  "getContinuations when cache is empty" should "read from history and put into the cache" in forAll {
    (channels: List[Channel], historyContinuations: List[Continuation]) =>
      fixture { (state, history, hotStore) =>
        for {
          _                 <- history.putContinuations(channels, historyContinuations)
          cache             <- state.inspect(identity)
          _                 = cache.continuations shouldBe empty
          readContinuations <- hotStore.getContinuations(channels)
          cache             <- state.inspect(identity)
          _                 = cache.continuations(channels) shouldEqual historyContinuations
        } yield (readContinuations shouldEqual historyContinuations)
      }
  }

  "getContinuations when cache contains data" should "read from cache ignoring history" in forAll {
    (
        channels: List[Channel],
        historyContinuations: List[Continuation],
        cachedContinuations: List[Continuation]
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putContinuations(channels, historyContinuations)
            _ <- state.modify(
                  _ =>
                    Cache(
                      continuations = Map(
                        channels -> cachedContinuations
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

  "putContinuation when cache is empty" should "read from history and add the continuation to it" in forAll {
    (
        channels: List[Channel],
        historyContinuations: List[Continuation],
        insertedContinuation: Continuation
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _     <- history.putContinuations(channels, historyContinuations)
            _     <- hotStore.putContinuation(channels, insertedContinuation)
            cache <- state.inspect(identity)
          } yield
            (cache.continuations(channels) shouldEqual insertedContinuation :: historyContinuations)
        }
      }
  }

  "putContinuation when cache contains data" should "read from the cache and add the continuation to it" in forAll {
    (
        channels: List[Channel],
        historyContinuations: List[Continuation],
        cachedContinuations: List[Continuation],
        insertedContinuation: Continuation
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putContinuations(channels, historyContinuations)
            _ <- state.modify(
                  _ =>
                    Cache(
                      continuations = Map(
                        channels -> cachedContinuations
                      )
                    )
                )
            _     <- hotStore.putContinuation(channels, insertedContinuation)
            cache <- state.inspect(identity)
          } yield
            (cache.continuations(channels) shouldEqual insertedContinuation :: cachedContinuations)
        }
      }
  }

  "getData when cache is empty" should "read from history and put into the cache" in forAll {
    (channel: Channel, historyData: List[Datum[String]]) =>
      fixture { (state, history, hotStore) =>
        for {
          _        <- history.putData(channel, historyData)
          cache    <- state.inspect(identity)
          _        = cache.data shouldBe empty
          readData <- hotStore.getData(channel)
          cache    <- state.inspect(identity)
          _        = cache.data(channel) shouldEqual historyData
        } yield (readData shouldEqual historyData)
      }
  }

  "getData when cache contains data" should "read from cache ignoring history" in forAll {
    (
        channel: Channel,
        historyData: List[Data],
        cachedData: List[Data]
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putData(channel, historyData)
            _ <- state.modify(
                  _ =>
                    Cache(
                      data = Map(
                        channel -> cachedData
                      )
                    )
                )
            readData <- hotStore.getData(channel)
            cache    <- state.get
            _        = cache.data(channel) shouldEqual cachedData
          } yield (readData shouldEqual cachedData)
        }
      }
  }

  "putData when cache is empty" should "read from history and add the continuation to it" in forAll {
    (
        channel: Channel,
        historyData: List[Data],
        insertedData: Data
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _     <- history.putData(channel, historyData)
            _     <- hotStore.putDatum(channel, insertedData)
            cache <- state.inspect(identity)
          } yield (cache.data(channel) shouldEqual insertedData :: historyData)
        }
      }
  }

  "putData when cache contains data" should "read from the cache and add the continuation to it" in forAll {
    (
        channel: Channel,
        historyData: List[Data],
        cachedData: List[Data],
        insertedData: Data
    ) =>
      fixture { (state, history, hotStore) =>
        {
          for {
            _ <- history.putData(channel, historyData)
            _ <- state.modify(
                  _ =>
                    Cache(
                      data = Map(
                        channel -> cachedData
                      )
                    )
                )
            _     <- hotStore.putDatum(channel, insertedData)
            cache <- state.inspect(identity)
          } yield (cache.data(channel) shouldEqual insertedData :: cachedData)
        }
      }
  }
}

class History[F[_]: Monad](implicit R: Ref[F, Cache[String, Pattern, String, StringsCaptor]])
    extends HistoryReader[F, String, Pattern, String, StringsCaptor] {

  def getJoins(channel: String): F[List[List[String]]] = ???

  def getData(channel: String): F[List[Datum[String]]] = R.get.map(_.data(channel))
  def putData(channel: String, data: List[Datum[String]]): F[Unit] = R.modify { prev =>
    (prev.copy(data = prev.data.+(channel -> data)), ())
  }

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
          MonadState[F, Cache[String, Pattern, String, StringsCaptor]],
          History[F],
          HotStore[F, String, Pattern, String, StringsCaptor]
      ) => F[Unit]
  ) =
    (for {
      historyState <- Ref.of[F, Cache[String, Pattern, String, StringsCaptor]](
                       Cache[String, Pattern, String, StringsCaptor]()
                     )
      history = {
        implicit val hs = historyState
        new History[F]
      }
      mvarCache <- MVar.of[Task, Cache[String, Pattern, String, StringsCaptor]](
                    Cache(
                      Map.empty[List[String], List[WaitingContinuation[Pattern, StringsCaptor]]]
                    )
                  )
      hotStore = {
        implicit val hr = history
        implicit val cache: MonadState[F, Cache[String, Pattern, String, StringsCaptor]] =
          mvarCache
        HotStore.inMem[Task, String, Pattern, String, StringsCaptor]
      }
      res <- f(mvarCache, history, hotStore)
    } yield res).runSyncUnsafe(1.second)

}
