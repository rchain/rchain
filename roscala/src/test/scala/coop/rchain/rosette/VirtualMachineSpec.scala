package coop.rchain.rosette

import coop.rchain.rosette.Ob.NIV
import coop.rchain.rosette.utils.opcodes.Dsl._
import coop.rchain.rosette.utils.opcodes._
import org.scalatest._

class VirtualMachineSpec extends WordSpec with Matchers {
  val someObsInd = someObs.size - 1
  val regInd = 9
  val bool = true
  val n = 4
  val m = 5

  def formatSuffix(suffix: String)(
      f: (String, String) => String)(name: String, path: String): String =
    s"${f(name, path)} $suffix"

  def formatPrefix(prefix: String)(
      f: (String, String) => String)(name: String, path: String): String =
    s"$prefix ${f(name, path)}"

  def rsltSuf(isEq: Boolean) = {
    val eqSym = if (isEq) "==" else "!="
    formatSuffix(s"if state.ctxt.rslt $eqSym Ob.RBLFALSE")(defaultFormatter) _
  }

  "The virtual machine's state" should {

    implicit val wordSpec: WordSpec = this

    (theState >> 'ctxt >> 'argvec on OpAlloc(n)) {
      _ shouldBe Tuple(n, NIV)
    }

    (theState >> 'strandPool on OpFork(n)) {
      val newCtxt = testState.ctxt.copy(pc = PC(n))
      _ shouldBe newCtxt +: testState.strandPool
    }

    (theState >> 'exitFlag on OpHalt()) {
      _ shouldBe true
    }

    (theState >> 'exitCode on OpHalt()) {
      _ shouldBe 0
    }

    (theState >> 'ctxt >> 'argvec >> 'elem on OpImmediateLitToArg(
      value = m,
      arg = someObsInd)) {
      val updatedElem =
        testState.ctxt.argvec.elem
          .updated(someObsInd, VirtualMachine.vmLiterals(m))
      _ shouldBe updatedElem
    }

    (theState >> 'pc >> 'relative on (
      OpJmpFalse(n),
      testState.update(_ >> 'ctxt >> 'rslt)(_ => Ob.RBLFALSE),
      rsltSuf(true)
    )) {
      _ shouldBe n
    }

    (theState >> 'pc >> 'relative on (
      OpJmpFalse(n),
      testState.update(_ >> 'ctxt >> 'rslt)(_ => Ob.NIV),
      formatPrefix("not")(rsltSuf(false))
    )) {
      _ shouldBe testState.pc.relative
    }

    (theState >> 'pc >> 'relative on OpJmp(n)) {
      _ shouldBe n
    }

    (theState >> 'ctxt >> 'nargs on OpNargs(n)) {
      _ shouldBe n
    }

    (theState >> 'ctxt >> 'pc on OpOutstanding(m, n)) {
      _ shouldBe PC(m)
    }

    (theState >> 'ctxt >> 'outstanding on OpOutstanding(m, n)) {
      _ shouldBe n
    }

    (theState >> 'ctxt on OpPop()) {
      _ shouldBe testState.ctxt.ctxt
    }

    (theState >> 'ctxt on OpPushAlloc(n)) {
      val pushed = Ctxt(Some(Tuple(n, None)), testState.ctxt)
      _ shouldBe pushed
    }

    (theState >> 'ctxt on OpPush()) {
      val pushed = Ctxt(None, testState.ctxt)
      _ shouldBe pushed
    }

    (theState >> 'doRtnData on OpRtn(bool)) {
      _ shouldBe bool
    }

    (theState >> 'doRtnFlag on OpRtn(bool)) {
      _ shouldBe true
    }

    (theState >> 'ctxt >> 'ctxt on OpSend(bool, !bool, m)) {
      _ shouldBe Ctxt.NIV
    }

    (theState >> 'ctxt >> 'nargs on OpSend(bool, !bool, m)) {
      _ shouldBe m
    }

    (theState >> 'xmitData on OpSend(bool, !bool, m)) {
      _ shouldBe (bool, !bool)
    }

    (theState >> 'exitFlag on OpUnknown()) {
      _ shouldBe true
    }

    (theState >> 'exitCode on OpUnknown()) {
      _ shouldBe 1
    }

    (theState >> 'ctxt >> 'argvec >> 'elem on OpXferArgToArg(
      dest = someObsInd,
      src = someObsInd)) {
      val elem = testState.ctxt.argvec.elem(someObsInd)
      val updated = testState.ctxt.argvec.elem.updated(someObsInd, elem)
      _ shouldBe updated
    }

    (theState >> 'ctxt >> 'rslt on OpXferArgToRslt(someObsInd)) {
      val elem = testState.ctxt.argvec.elem(someObsInd)
      _ shouldBe elem
    }

    (theState >> 'ctxt >> 'argvec >> 'elem on OpXferGlobalToArg(
      arg = someObsInd,
      global = someObsInd)) {
      val elem = testState.globalEnv.entry(someObsInd)
      val updated = testState.ctxt.argvec.elem.updated(someObsInd, elem)
      _ shouldBe updated
    }

    (theState >> 'ctxt >> 'rslt on OpXferRegToRslt(reg = someObsInd)) {
      val elem = testState.ctxt.getReg(someObsInd)
      Some(_) shouldBe elem
    }

    (theState >> 'ctxt >> 'argvec >> 'elem on OpXferRsltToArg(someObsInd)) {
      val elem = testState.ctxt.rslt
      val updated = testState.ctxt.argvec.elem.updated(someObsInd, elem)
      _ shouldBe updated
    }

    (theState >> 'loc on OpXferSrcToRslt(someObsInd)) {
      val atom = LocationAtom(testState.code.lit((someObsInd)))
      _ shouldBe atom
    }

    (theState >> 'ctxt >> 'rslt on OpXferSrcToRslt(someObsInd)) {
      val ob =
        Location.fetch(testState.loc, testState.ctxt, testState.globalEnv)
      _ shouldBe ob
    }

    (theState >> 'ctxt >> 'nargs on OpXmit(bool, bool, m)) {
      _ shouldBe m
    }

    (theState >> 'xmitData on OpXmit(bool, !bool, m)) {
      _ shouldBe (bool, !bool)
    }

    (theState >> 'doXmitFlag on OpXmit(bool, !bool, m)) {
      _ shouldBe true
    }
  }
}
