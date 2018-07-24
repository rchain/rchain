package coop.rchain.casper.util.rholang

import coop.rchain.casper.genesis.contracts.TestSetUtil
import coop.rchain.rspace.trace.Log
import coop.rchain.rspace.Checkpoint
import coop.rchain.casper.protocol.Deploy
import coop.rchain.casper.util.ProtoUtil.termDeploy
import coop.rchain.casper.util.rholang.InterpreterUtil.mkTerm
import com.google.protobuf.ByteString

import monix.execution.Scheduler.Implicits.global

import org.scalatest.{FlatSpec, Matchers}

import scala.annotation.tailrec

class RepeatedReplayRSpaceTest extends FlatSpec with Matchers {
  import RepeatedReplayRSpaceTest._

  "ReplayRSpace" should "not have untraced comm events" in {
    //TODO: get this down to a minimal rholang code example
    val code = scala.io.Source.fromFile("./rholang/examples/tut-philosophers.rho").mkString
    val rm1  = mkManager()
    val rm2  = mkManager()
    val n    = 100

    val chs = repeat(n, code, rm1, rm2)

    chs.size should be(n)
  }
}

object RepeatedReplayRSpaceTest {
  def mkManager(): RuntimeManager = {
    val active = TestSetUtil.runtime("test")
    RuntimeManager.fromRuntime(active)
  }

  def eval(code: String, start: ByteString, rm: RuntimeManager): (Deploy, Checkpoint) = {
    val term   = mkTerm(code).right.get
    val deploy = termDeploy(term)

    deploy -> rm.computeState(start, Seq(deploy)).right.get
  }

  def replay(d: Deploy, start: ByteString, log: Log, rm: RuntimeManager): Checkpoint = {
    val f = rm.replayComputeState(log)
    f(start, Seq(d)).right.get
  }

  private def useHash(hash: ByteString)(code: String,
                                        rm1: RuntimeManager,
                                        rm2: RuntimeManager): (ByteString, Checkpoint) = {
    val (d, ch) = eval(code, hash, rm1)
    val chRep   = replay(d, hash, ch.log, rm2)
    //TODO: handle this better...
    if (ch.root != chRep.root) throw new Exception("Holy Shit!")
    else
      ByteString.copyFrom(ch.root.bytes.toArray) -> ch
  }

  @tailrec
  def repeat(n: Int,
             code: String,
             rm1: RuntimeManager,
             rm2: RuntimeManager,
             start: Option[ByteString] = None,
             acc: List[Checkpoint] = Nil): List[Checkpoint] =
    if (n == 0) acc
    else
      start match {
        case None =>
          val (newStart, ch) = useHash(rm1.emptyStateHash)(code, rm1, rm2)
          repeat(n - 1, code, rm1, rm2, Some(newStart), ch :: acc)

        case Some(hash) =>
          val (newStart, ch) = useHash(hash)(code, rm1, rm2)
          repeat(n - 1, code, rm1, rm2, Some(newStart), ch :: acc)
      }

}
