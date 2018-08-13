package coop.rchain.casper
import coop.rchain.casper.protocol.ApprovedBlock
import monix.eval.Task
import monix.execution.schedulers.TestScheduler
import org.scalatest.{FunSpec, Matchers}

import scala.concurrent.duration._
import scala.util.Success

class LastApprovedBlockSpec extends FunSpec with Matchers {

  describe("LastApprovedBlock") {
    it("get should return ApprovedBlock on get when available") {
      implicit val testScheduler = TestScheduler()
      val approvedBlock          = ApprovedBlock()
      val test = for {
        lab   <- LastApprovedBlock.of[Task]
        _     <- lab.set(approvedBlock)
        block <- lab.get
      } yield block

      val result = test.runAsync
      testScheduler.tick()
      assert(result.value == Some(Success(Some(approvedBlock))))
    }

    it("get should return None no ApprovedBlock has been seen") {
      implicit val testScheduler = TestScheduler()
      val test = for {
        lab   <- LastApprovedBlock.of[Task]
        block <- lab.get
      } yield block

      val result = test.runAsync
      testScheduler.tick()
      assert(result.value == Some(Success(None)))
    }
  }
}
