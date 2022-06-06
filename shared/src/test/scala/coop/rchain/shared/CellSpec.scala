package coop.rchain.shared

import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

import cats._, cats.data._, cats.syntax.all._
import coop.rchain.catscontrib._, Catscontrib._, TaskContrib._
import monix.eval.{MVar, Task}
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import scala.collection.concurrent.TrieMap

class CellSpec extends AnyFunSpec with Matchers with BeforeAndAfterEach {

  implicit val io: SchedulerService = Scheduler.io("test")

  val increment: Int => Task[Int] = (i: Int) => Task.delay(i + 1)

  def justIncrement(cell: Cell[Task, Int]): Task[Unit] =
    cell.flatModify(increment)

  def incrementAndStore(
      name: String,
      cell: Cell[Task, Int],
      external: TrieMap[String, Int]
  ): Task[Unit] =
    cell.flatModify(
      i =>
        for {
          newI <- increment(i)
          _    <- Task.delay(external += (name -> newI))
        } yield newI
    )

  describe("MVar Cell") {

    def run(test: Cell[Task, Int] => Task[Unit]): Unit =
      (for {
        cell <- Cell.mvarCell[Task, Int](0)
        _    <- test(cell)
      } yield ()).unsafeRunSync

    describe("flatModify") {
      it("should allow thread-safe concurrent state modification") {
        run(
          cell =>
            for {
              f1  <- justIncrement(cell).start
              f2  <- justIncrement(cell).start
              f3  <- justIncrement(cell).start
              _   <- f1.join
              _   <- f2.join
              _   <- f3.join
              res <- cell.read
              _   <- Task.delay(res shouldBe 3)
            } yield ()
        )
      }

      /**
        * While modifing the state, other effects can take place. In this example
        * external datasource (TrieMap) is being modified
        */
      it("should allow effectful state modification") {

        val external: TrieMap[String, Int] = TrieMap.empty[String, Int]

        run(
          cell =>
            for {
              f1  <- incrementAndStore("worker1", cell, external).start
              f2  <- incrementAndStore("worker2", cell, external).start
              f3  <- incrementAndStore("worker3", cell, external).start
              _   <- f1.join
              _   <- f2.join
              _   <- f3.join
              res <- cell.read
              _   <- Task.delay(res shouldBe 3)
              _   <- Task.delay(external.keys should contain allOf ("worker1", "worker2", "worker3"))
              _   <- Task.delay(external.values should contain allOf (1, 2, 3))
            } yield ()
        )
      }

      it("should restore state after a failed modify") {
        def failWithError(cell: Cell[Task, Int]): Task[Unit] =
          cell.flatModify(_ => Task.raiseError(new RuntimeException))

        run(
          cell =>
            for {
              _   <- justIncrement(cell)
              _   <- failWithError(cell).attempt
              _   <- justIncrement(cell)
              res <- cell.read
              _   <- Task.delay(res shouldBe 2)
            } yield ()
        )
      }

    }

    describe("modify") {
      def justIncrement(cell: Cell[Task, Int]): Task[Unit] =
        cell.modify(_ + 1)

      it("should allow thread-safe concurrent state modification") {
        run(
          cell =>
            for {
              f1  <- justIncrement(cell).start
              f2  <- justIncrement(cell).start
              f3  <- justIncrement(cell).start
              _   <- f1.join
              _   <- f2.join
              _   <- f3.join
              res <- cell.read
              _   <- Task.delay(res shouldBe 3)
            } yield ()
        )
      }
    }

  }
  describe("Ref Cell") {

    def run(test: Cell[Task, Int] => Task[Unit]): Unit =
      (for {
        cell <- Cell.refCell[Task, Int](0)
        _    <- test(cell)
      } yield ()).unsafeRunSync

    describe("flatModify") {
      it("should allow thread-safe concurrent state modification") {
        run(
          cell =>
            for {
              f1  <- justIncrement(cell).start
              f2  <- justIncrement(cell).start
              f3  <- justIncrement(cell).start
              _   <- f1.join
              _   <- f2.join
              _   <- f3.join
              res <- cell.read
              _   <- Task.delay(res should (be >= 1 and be <= 3))
            } yield ()
        )
      }

      /**
        * While modifing the state, other effects can take place. In this example
        * external datasource (TrieMap) is being modified
        */
      it("should allow effectful state modification") {

        val external: TrieMap[String, Int] = TrieMap.empty[String, Int]

        run(
          cell =>
            for {
              f1  <- incrementAndStore("worker1", cell, external).start
              f2  <- incrementAndStore("worker2", cell, external).start
              f3  <- incrementAndStore("worker3", cell, external).start
              _   <- f1.join
              _   <- f2.join
              _   <- f3.join
              res <- cell.read
              _   <- Task.delay(res should (be >= 1 and be <= 3))
              _   <- Task.delay(external.keys should contain allOf ("worker1", "worker2", "worker3"))
            } yield ()
        )
      }

      it("should restore state after a failed modify") {
        def failWithError(cell: Cell[Task, Int]): Task[Unit] =
          cell.flatModify(_ => Task.raiseError(new RuntimeException))

        run(
          cell =>
            for {
              _   <- justIncrement(cell)
              _   <- failWithError(cell).attempt
              _   <- justIncrement(cell)
              res <- cell.read
              _   <- Task.delay(res shouldBe 2)
            } yield ()
        )
      }

    }
  }
}
