package coop.rchain.finalization

import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

class FinalizationSpec extends FlatSpec with Matchers {
  import monix.execution.Scheduler.Implicits.global

  val sut = new NetworkRunner[Task]()

  it should "run network with complete dag" ignore {
    val r        = sut.runDagComplete.runSyncUnsafe()
    val (end, _) = r
    val a = end.senders.toList.map(
      _.realFringes
        .map(_.toList.sortBy { case (k, _) => k.id }.map(_._2.id).toString())
    )
    println(a.mkString("\n"))

  }

  it should "run random network" ignore {
    val r        = sut.runRandom.runSyncUnsafe()
    val (end, _) = r.last
    val a = end.senders.toList.map(
      _.realFringes
        .map(_.toList.sortBy { case (k, _) => k.id }.map(_._2.id).toString())
    )
    println(a.mkString("\n"))

  }

  // This test is ignored by default to provide finite tests time execution
  // It makes sense to turn on this test only on the local machine for long-time finalization testing
  it should "run infinite test" ignore {
    sut.runInfinite(enableOutput = false, generateCode = true).runSyncUnsafe()
  }
}
