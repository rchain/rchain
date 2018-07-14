package coop.rchain.roscala

import coop.rchain.roscala.pools.{ParallelStrandPool, SimpleStrandPool, StrandPoolExecutor}
import org.scalactic.source
import org.scalatest.words.ResultOfStringPassedToVerb
import org.scalatest.{FlatSpec, Matchers}

trait VmSpecUtils extends FlatSpec with Matchers with App {

  def strandPoolName[E: StrandPoolExecutor] = StrandPoolExecutor.instance[E].getClass.getSimpleName

  def runBehaviour[E: StrandPoolExecutor]: Unit

  private def registerVmTest[E: StrandPoolExecutor](verb: String, rest: String, testFun: => Any)(
      implicit pos: source.Position) =
    registerTest(s"${verb.trim} ${rest.trim} using $strandPoolName")(testFun)

  implicit class ResultOfStringPassedToVerbOps(v: ResultOfStringPassedToVerb) {
    def inBlock[E: StrandPoolExecutor](testFun: => Any)(implicit pos: source.Position): Unit =
      registerVmTest(v.verb, v.rest, testFun)
  }

  it should behave like runBehaviour[ParallelStrandPool]
  it should behave like runBehaviour[SimpleStrandPool]
}
