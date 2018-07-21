package coop.rchain.roscala

import coop.rchain.roscala.pools.{ParallelStrandPool, SimpleStrandPool, StrandPoolExecutor}
import org.scalactic.source
import org.scalatest.words.ResultOfStringPassedToVerb
import org.scalatest.{FlatSpec, Matchers}

trait VmSpecUtils extends FlatSpec with Matchers with App {
  final class CurryStrandPoolType[E1](v: ResultOfStringPassedToVerb) {
    def apply[E2](testFun: => Any)(implicit
                                   pos: source.Position,
                                   e1: StrandPoolExecutor[E1],
                                   e2: StrandPoolExecutor[E2]): Unit =
      if (e1 == e2) {
        registerVmTest[E1](v, testFun)
      }
  }

  def strandPoolName[E: StrandPoolExecutor] = StrandPoolExecutor.instance[E].getClass.getSimpleName

  def runBehaviour[E: StrandPoolExecutor]: Unit

  private def registerVmTest[E: StrandPoolExecutor](v: ResultOfStringPassedToVerb, testFun: => Any)(
      implicit pos: source.Position) =
    registerTest(s"${v.verb.trim} ${v.rest.trim} using $strandPoolName")(testFun)

  implicit class ResultOfStringPassedToVerbOps(v: ResultOfStringPassedToVerb) {
    def inMultimode[E: StrandPoolExecutor](testFun: => Any)(implicit pos: source.Position): Unit =
      registerVmTest(v, testFun)

    def inMode[E] = new CurryStrandPoolType[E](v)
  }

  it should behave like runBehaviour[ParallelStrandPool]
  it should behave like runBehaviour[SimpleStrandPool]
}
