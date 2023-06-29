package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object ConnectiveUsed {
  private def cUsed(p: RhoTypeN): Boolean       = p.connectiveUsed
  private def cUsed(ps: Seq[RhoTypeN]): Boolean = ps.exists(cUsed)
  private def cUsed(pOpt: Option[RhoTypeN]): Boolean =
    if (pOpt.isDefined) cUsed(pOpt.get) else false

  def connectiveUsedFn(p: RhoTypeN): Boolean = p match {

    /** Basic types */
    case _: NilN           => false
    case pProc: ParProcN   => cUsed(pProc.ps)
    case send: SendN       => cUsed(send.chan) || cUsed(send.data)
    case receive: ReceiveN => cUsed(receive.binds) || cUsed(receive.body)
    case m: MatchN         => cUsed(m.target) || cUsed(m.cases)
    case _: NewN           => false // There are no situations when New gets into the matcher

    /** Ground types */
    case _: GroundN => false

    /** Collections */
    case eList: EListN   => cUsed(eList.ps) || cUsed(eList.remainder)
    case eTuple: ETupleN => cUsed(eTuple.ps)

    /** Vars */
    case _: BoundVarN => false
    case _: FreeVarN  => true
    case _: WildcardN => true

    /** Unforgeable names */
    case _: UnforgeableN => false

    /** Operations */
    case op: Operation1ParN  => cUsed(op.p)
    case op: Operation2ParN  => cUsed(op.p1) || cUsed(op.p2)
    case eMethod: EMethodN   => cUsed(eMethod.target) || cUsed(eMethod.arguments)
    case eMatches: EMatchesN => cUsed(eMatches.target)

    /** Bundle */
    /** Connective */
    /** Auxiliary types */
    case bind: ReceiveBindN => cUsed(bind.source)
    case mCase: MatchCaseN  => cUsed(mCase.source)

    /** Other types */
    case _: BundleN       => false // There are no situations when New gets into the matcher
    case _: SysAuthTokenN => false

    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
