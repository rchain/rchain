package coop.rchain.casper.util.comm

import scala.collection.immutable.Queue

import cats.effect.concurrent.Ref
import cats.effect.SyncIO

import org.scalatest.{Matchers, WordSpecLike}

class FairRoundRobinDispatcherSpec extends WordSpecLike with Matchers {

  "FairRoundRobinDispatcher" when {

    "ensureSourceExists" should {
      "add a new Source" in
        new TestEnv(1, 0, 0) {
          validate(
            dispatcher.ensureSourceExists("A")
          )(
            expectedQueue = Queue("A"),
            expectedMessages = Map("A" -> Queue.empty[Int]),
            expectedRetries = Map("A"  -> 0)
          )
        }

      "not add a Source if it exists" in
        new TestEnv(1, 0, 0) {
          validate(
            for {
              _ <- queue.update(_.enqueue("A"))
              _ <- messages.update(_ + ("A" -> Queue(1)))
              _ <- retries.update(_ + ("A" -> 2))
              _ <- dispatcher.ensureSourceExists("A")
            } yield ()
          )(
            expectedQueue = Queue("A"),
            expectedMessages = Map("A" -> Queue(1)),
            expectedRetries = Map("A"  -> 2)
          )
        }
    }

    "enqueueMessage" should {
      "enqueue the message" in
        new TestEnv(10, 0, 0) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- retries.update(_ + ("A" -> 2))
              _ <- dispatcher.enqueueMessage("A", 1)
            } yield ()
          )(
            expectedQueue = Queue("A"),
            expectedMessages = Map("A" -> Queue(1)),
            expectedRetries = Map("A"  -> 0)
          )
        }

      "drop the message if the queue reached max size" in
        new TestEnv(1, 0, 0) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.enqueueMessage("A", 1)
              _ <- retries.update(_ + ("A" -> 2))
              _ <- dispatcher.enqueueMessage("A", 2)
            } yield ()
          )(
            expectedQueue = Queue("A"),
            expectedMessages = Map("A" -> Queue(1)),
            expectedRetries = Map("A"  -> 2)
          )
        }
    }

    "handleMessage" should {
      "handle the message" in
        new TestEnv(10, 0, 0) {
          val result: Boolean =
            validate(
              for {
                _      <- dispatcher.ensureSourceExists("A")
                _      <- dispatcher.enqueueMessage("A", 1)
                result <- dispatcher.handleMessage("A")
              } yield result
            )(
              expectedQueue = Queue("A"),
              expectedMessages = Map("A"  -> Queue(1)),
              expectedRetries = Map("A"   -> 0),
              expectedHandled = Queue("A" -> 1)
            )

          result shouldBe true
        }

      "not handle any messages if the current source queue is empty" in
        new TestEnv(10, 0, 0) {
          val result: Boolean =
            validate(
              for {
                _      <- dispatcher.ensureSourceExists("B")
                _      <- dispatcher.ensureSourceExists("A")
                _      <- dispatcher.enqueueMessage("A", 1)
                result <- dispatcher.handleMessage("B")
              } yield result
            )(
              expectedQueue = Queue("B", "A"),
              expectedMessages = Map(
                "A" -> Queue(1),
                "B" -> Queue.empty[Int]
              ),
              expectedRetries = Map(
                "A" -> 0,
                "B" -> 0
              )
            )

          result shouldBe false
        }
    }

    "rotate" should {
      "rotate the sources" in
        new TestEnv(1, 0, 0) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.rotate
            } yield ()
          )(
            expectedQueue = Queue("B", "C", "A"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue.empty[Int],
              "C" -> Queue.empty[Int]
            ),
            expectedRetries = Map(
              "A" -> 0,
              "B" -> 0,
              "C" -> 0
            )
          )
        }

      "rotate single source queue" in
        new TestEnv(1, 0, 0) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.rotate
            } yield ()
          )(
            expectedQueue = Queue("A"),
            expectedMessages = Map("A" -> Queue.empty[Int]),
            expectedRetries = Map("A"  -> 0)
          )
        }
    }

    "dropSource" should {
      "drop the source" in
        new TestEnv(1, 0, 0) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.dropSource("A")
            } yield ()
          )(
            expectedQueue = Queue("B"),
            expectedMessages = Map("B" -> Queue.empty[Int]),
            expectedRetries = Map("B"  -> 0)
          )
        }
    }

    "giveUp" should {
      "reset skipped, increment retries and rotate" in
        new TestEnv(1, 0, 1) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- skipped.set(2)
              _ <- dispatcher.giveUp("A")
            } yield ()
          )(
            expectedQueue = Queue("B", "C", "A"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue.empty[Int],
              "C" -> Queue.empty[Int]
            ),
            expectedRetries = Map(
              "A" -> 1,
              "B" -> 0,
              "C" -> 0
            )
          )
        }

      "reset skipped and drop source" in
        new TestEnv(1, 0, 1) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- retries.update(r => r.updated("A", 2))
              _ <- skipped.set(2)
              _ <- dispatcher.giveUp("A")
            } yield ()
          )(
            expectedQueue = Queue("B", "C"),
            expectedMessages = Map(
              "B" -> Queue.empty[Int],
              "C" -> Queue.empty[Int]
            ),
            expectedRetries = Map(
              "B" -> 0,
              "C" -> 0
            )
          )
        }
    }

    "success" should {
      "remove message from queue, reset skipped and rotate" in
        new TestEnv(1, 0, 0) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.enqueueMessage("A", 1)
              _ <- dispatcher.enqueueMessage("B", 2)
              _ <- dispatcher.enqueueMessage("C", 3)
              _ <- skipped.set(2)
              _ <- dispatcher.success("A")
            } yield ()
          )(
            expectedQueue = Queue("B", "C", "A"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue(2),
              "C" -> Queue(3)
            ),
            expectedRetries = Map(
              "A" -> 0,
              "B" -> 0,
              "C" -> 0
            )
          )
        }
    }

    "failure" should {
      "increment skipped (if skipped < giveUpAfterSkipped)" in
        new TestEnv(1, 2, 0) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.enqueueMessage("B", 2)
              _ <- dispatcher.enqueueMessage("C", 3)
              _ <- dispatcher.failure("A")
            } yield ()
          )(
            expectedQueue = Queue("A", "B", "C"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue(2),
              "C" -> Queue(3)
            ),
            expectedRetries = Map(
              "A" -> 0,
              "B" -> 0,
              "C" -> 0
            ),
            expectedSkipped = 1
          )
        }

      "give up (if skipped >= giveUpAfterSkipped)" in
        new TestEnv(1, 1, 1) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.enqueueMessage("B", 2)
              _ <- dispatcher.enqueueMessage("C", 3)
              _ <- skipped.set(1)
              _ <- dispatcher.failure("A")
            } yield ()
          )(
            expectedQueue = Queue("B", "C", "A"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue(2),
              "C" -> Queue(3)
            ),
            expectedRetries = Map(
              "A" -> 1,
              "B" -> 0,
              "C" -> 0
            ),
            expectedSkipped = 0
          )
        }
    }

    "dispatch (integration)" should {
      "discovers new source" in
        new TestEnv(10, 10, 10) {
          validate(
            for {
              _ <- dispatcher.dispatch("A", 1)
            } yield ()
          )(
            expectedQueue = Queue("A"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int]
            ),
            expectedRetries = Map(
              "A" -> 0
            ),
            expectedHandled = Queue("A" -> 1)
          )
        }

      "dispatch round robin" in
        new TestEnv(10, 10, 10) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.dispatch("A", 1)
              _ <- dispatcher.dispatch("B", 2)
              _ <- dispatcher.dispatch("C", 3)
            } yield ()
          )(
            expectedQueue = Queue("A", "B", "C"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue.empty[Int],
              "C" -> Queue.empty[Int]
            ),
            expectedRetries = Map(
              "A" -> 0,
              "B" -> 0,
              "C" -> 0
            ),
            expectedHandled = Queue("A" -> 1, "B" -> 2, "C" -> 3)
          )
        }

      "enqueue when waiting for message from other sources" in
        new TestEnv(10, 10, 10) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.dispatch("B", 1)
              _ <- dispatcher.dispatch("B", 2)
              _ <- dispatcher.dispatch("C", 3)
            } yield ()
          )(
            expectedQueue = Queue("A", "B", "C"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue(1, 2),
              "C" -> Queue(3)
            ),
            expectedRetries = Map(
              "A" -> 0,
              "B" -> 0,
              "C" -> 0
            ),
            expectedSkipped = 3
          )
        }

      "enqueue when waiting for message from other sources and drop if full" in
        new TestEnv(2, 10, 10) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.dispatch("B", 1)
              _ <- dispatcher.dispatch("B", 2)
              _ <- dispatcher.dispatch("C", 3)
              _ <- dispatcher.dispatch("B", 4)
            } yield ()
          )(
            expectedQueue = Queue("A", "B", "C"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue(1, 2),
              "C" -> Queue(3)
            ),
            expectedRetries = Map(
              "A" -> 0,
              "B" -> 0,
              "C" -> 0
            ),
            expectedSkipped = 4
          )
        }

      "dispatch as many messages from buffer as possible" in
        new TestEnv(10, 10, 10) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.dispatch("B", 1)
              _ <- dispatcher.dispatch("B", 2)
              _ <- dispatcher.dispatch("C", 3)
              _ <- dispatcher.dispatch("A", 4)
            } yield ()
          )(
            expectedQueue = Queue("A", "B", "C"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue(2),
              "C" -> Queue.empty[Int]
            ),
            expectedRetries = Map(
              "A" -> 0,
              "B" -> 0,
              "C" -> 0
            ),
            expectedHandled = Queue("A" -> 4, "B" -> 1, "C" -> 3)
          )
        }

      "give up after skipped messages and continue with other sources" in
        new TestEnv(10, 3, 10) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.dispatch("B", 1)
              _ <- dispatcher.dispatch("B", 2)
              _ <- dispatcher.dispatch("C", 3)
            } yield ()
          )(
            expectedQueue = Queue("A", "B", "C"),
            expectedMessages = Map(
              "A" -> Queue.empty[Int],
              "B" -> Queue(2),
              "C" -> Queue.empty[Int]
            ),
            expectedRetries = Map(
              "A" -> 1,
              "B" -> 0,
              "C" -> 0
            ),
            expectedHandled = Queue("B" -> 1, "C" -> 3)
          )
        }

      "drop source after retries and continue with other sources" in
        new TestEnv(10, 3, 0) {
          validate(
            for {
              _ <- dispatcher.ensureSourceExists("A")
              _ <- dispatcher.ensureSourceExists("B")
              _ <- dispatcher.ensureSourceExists("C")
              _ <- dispatcher.dispatch("B", 1)
              _ <- dispatcher.dispatch("B", 2)
              _ <- dispatcher.dispatch("C", 3)
            } yield ()
          )(
            expectedQueue = Queue("C", "B"),
            expectedMessages = Map(
              "B" -> Queue.empty[Int],
              "C" -> Queue.empty[Int]
            ),
            expectedRetries = Map(
              "B" -> 0,
              "C" -> 0
            ),
            expectedHandled = Queue("B" -> 1, "C" -> 3, "B" -> 2)
          )
        }

      "be effectively disabled with giveUpAfterSkipped = 0 and dropSourceAfterRetries = 0" in
        new TestEnv(10, 0, 0) {
          validate(
            for {
              _ <- dispatcher.dispatch("A", 1)
              _ <- dispatcher.dispatch("B", 2)
              _ <- dispatcher.dispatch("C", 3)
              _ <- dispatcher.dispatch("D", 4)
              _ <- dispatcher.dispatch("C", 5)
              _ <- dispatcher.dispatch("B", 6)
              _ <- dispatcher.dispatch("A", 7)
              _ <- dispatcher.dispatch("D", 8)
              _ <- dispatcher.dispatch("C", 9)
            } yield ()
          )(
            expectedQueue = Queue("C"),
            expectedMessages = Map(
              "C" -> Queue.empty[Int]
            ),
            expectedRetries = Map(
              "C" -> 0
            ),
            expectedHandled = Queue(
              "A" -> 1,
              "B" -> 2,
              "C" -> 3,
              "D" -> 4,
              "C" -> 5,
              "B" -> 6,
              "A" -> 7,
              "D" -> 8,
              "C" -> 9
            )
          )
        }

    }
  }

  abstract class TestEnv(
      maxSourceQueueSize: Int,
      giveUpAfterSkipped: Int,
      dropSourceAfterRetries: Int
  ) {
    val (queue, messages, retries, skipped, handled) =
      (for {
        queue    <- Ref[SyncIO].of(Queue.empty[String])
        messages <- Ref[SyncIO].of(Map.empty[String, Queue[Int]])
        retries  <- Ref[SyncIO].of(Map.empty[String, Int])
        skipped  <- Ref[SyncIO].of(0)
        handled  <- Ref[SyncIO].of(Queue.empty[(String, Int)])
      } yield (queue, messages, retries, skipped, handled)).unsafeRunSync()

    val dispatcher: FairRoundRobinDispatcher[SyncIO, String, Int] =
      new FairRoundRobinDispatcher[SyncIO, String, Int](
        (s, m) => handled.update(_.enqueue((s, m))),
        queue,
        messages,
        retries,
        skipped,
        maxSourceQueueSize,
        giveUpAfterSkipped,
        dropSourceAfterRetries
      )

    private val state =
      for {
        q <- queue.get
        m <- messages.get
        r <- retries.get
        s <- skipped.get
        h <- handled.get
      } yield (q, m, r, s, h)

    def validate[A](block: => SyncIO[A])(
        expectedQueue: Queue[String] = Queue.empty[String],
        expectedMessages: Map[String, Queue[Int]] = Map.empty[String, Queue[Int]],
        expectedRetries: Map[String, Int] = Map.empty[String, Int],
        expectedSkipped: Int = 0,
        expectedHandled: Queue[(String, Int)] = Queue.empty[(String, Int)]
    ): A = {
      val (postState, result) = block.flatMap(r => state.map((_, r))).unsafeRunSync()
      val (q, m, r, s, h)     = postState
      q shouldEqual expectedQueue
      m shouldEqual expectedMessages
      r shouldEqual expectedRetries
      s shouldEqual expectedSkipped
      h shouldEqual expectedHandled
      result
    }
  }
}
