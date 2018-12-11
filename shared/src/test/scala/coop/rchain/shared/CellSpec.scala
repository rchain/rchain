package coop.rchain.shared

import org.scalatest._

import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, TaskContrib._
import monix.eval.{MVar, Task}
import monix.execution.Scheduler
import monix.execution.schedulers.SchedulerService
import scala.collection.concurrent.TrieMap

class CellSpec extends FunSpec with Matchers with BeforeAndAfterEach {

  implicit val io: SchedulerService = Scheduler.io("test")

  describe("Cell") {
    it("should allow thread-safe concurrent state modification") {
      run(
        cell =>
          for {
            f1  <- justIncrement(cell).fork
            f2  <- justIncrement(cell).fork
            f3  <- justIncrement(cell).fork
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
            f1  <- incrementAndStore("worker1", cell, external).fork
            f2  <- incrementAndStore("worker2", cell, external).fork
            f3  <- incrementAndStore("worker3", cell, external).fork
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

  private def run(test: Cell[Task, Int] => Task[Unit]): Unit =
    (for {
      cell <- Cell.mvarCell[Task, Int](0)
      _    <- test(cell)
    } yield ()).unsafeRunSync

  private def justIncrement(cell: Cell[Task, Int]): Task[Unit] =
    cell.modify(increment)

  private def incrementAndStore(
      name: String,
      cell: Cell[Task, Int],
      external: TrieMap[String, Int]
  ): Task[Unit] =
    cell.modify(
      i =>
        for {
          newI <- increment(i)
          _    <- Task.delay(external += (name -> newI))
        } yield newI
    )

  private val increment: Int => Task[Int] = (i: Int) => Task.delay(i + 1)

  private def failWithError(cell: Cell[Task, Int]): Task[Unit] =
    cell.modify(_ => Task.raiseError(new RuntimeException))

}
