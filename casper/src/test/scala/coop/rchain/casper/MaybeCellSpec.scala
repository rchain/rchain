package coop.rchain.casper
import coop.rchain.casper.protocol.ApprovedBlock
import coop.rchain.shared.MaybeCell
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import org.scalatest.{FunSpec, Matchers}

import scala.util.Success

class MaybeCellSpec extends FunSpec with Matchers {

  describe("MaybeCell") {
    it("get should return ApprovedBlock on get when available") {
      implicit val testScheduler = TestScheduler()
      val approvedBlock          = ApprovedBlock()
      val test = for {
        lab   <- MaybeCell.of[Task, ApprovedBlock]
        _     <- lab.set(approvedBlock)
        block <- lab.get
      } yield block

      val result = test.runToFuture
      testScheduler.tick()
      assert(result.value == Some(Success(Some(approvedBlock))))
    }

    it("get should return None no ApprovedBlock has been seen") {
      implicit val testScheduler = TestScheduler()
      val test = for {
        lab   <- LastApprovedBlock.of[Task]
        block <- lab.get
      } yield block

      val result = test.runToFuture
      testScheduler.tick()
      assert(result.value == Some(Success(None)))
    }
  }
}
