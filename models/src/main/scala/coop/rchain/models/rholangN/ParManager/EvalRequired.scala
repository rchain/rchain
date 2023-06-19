package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

import scala.annotation.unused

private[ParManager] object EvalRequired {
  private def eRequiredParSeq(ps: Seq[ParN]) = ps.exists(_.evalRequired)

  def evalRequiredFn(p: RhoTypeN): Boolean = p match {

    /** Main types */
    case pproc: ParProcN => eRequiredParSeq(pproc.ps)
    case send: SendN     => eRequiredParSeq(send.data)

    /** Ground types */
    case _: GNilN => false
    case _: GIntN => false

    /** Collections */
    case list: EListN => eRequiredParSeq(list.ps)

    /** Vars */
    case _: BoundVarN => true
    case _: FreeVarN  => true
    case _: WildcardN => true

    /** Expr */
    /** Bundle */
    /** Connective */
    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
