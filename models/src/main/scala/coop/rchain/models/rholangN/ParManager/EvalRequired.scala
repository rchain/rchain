package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object EvalRequired {
  private def eReq(p: RhoTypeN): Boolean       = p.evalRequired
  private def eReq(ps: Seq[RhoTypeN]): Boolean = ps.exists(eReq)

  def evalRequiredFn(p: RhoTypeN): Boolean = p match {

    /** Main types */
    case pProc: ParProcN => eReq(pProc.ps)
    case _: SendN        => true
    case _: ReceiveN     => true
    case _: MatchN       => true
    case _: NewN         => true

    /** Ground types */
    case _: GNilN  => false
    case _: GBoolN => false
    case _: GIntN  => false

    /** Collections */
    case list: EListN => eReq(list.ps)

    /** Vars */
    case _: BoundVarN => true
    case _: FreeVarN  => true
    case _: WildcardN => true

    /** Expr */
    /** Bundle */
    /** Connective */
    /** Auxiliary types */
    case _: ReceiveBindN => true
    case _: MatchCaseN   => true

    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
