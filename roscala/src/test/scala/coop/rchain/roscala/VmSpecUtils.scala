package coop.rchain.roscala

import coop.rchain.roscala.pools.{ParallelStrandPool, SimpleStrandPool, StrandPoolExecutor}
import org.scalactic.source
import org.scalatest.words.ResultOfStringPassedToVerb
import org.scalatest.{FlatSpec, Matchers}

trait VmSpecUtils extends FlatSpec with Matchers {
  def strandPoolName[E: StrandPoolExecutor] = StrandPoolExecutor.instance[E].getClass.getSimpleName

  def runBehaviour[E: StrandPoolExecutor]: Unit

  private def registerVmTest[E: StrandPoolExecutor](v: ResultOfStringPassedToVerb, testFun: => Any)(
      implicit pos: source.Position
  ) =
    registerTest(s"${v.verb.trim} ${v.rest.trim} using $strandPoolName")(testFun)

  implicit class ResultOfStringPassedToVerbOps(v: ResultOfStringPassedToVerb) {
    def inMultimode[E: StrandPoolExecutor](testFun: => Any)(implicit pos: source.Position): Unit =
      registerVmTest(v, testFun)
  }

  runBehaviour[ParallelStrandPool]
  runBehaviour[SimpleStrandPool]
}
