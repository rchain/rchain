package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object SubstituteRequired {
  private def sReq(p: RhoTypeN): Boolean       = p.substituteRequired
  private def sReq(ps: Seq[RhoTypeN]): Boolean = ps.exists(sReq)

  def substituteRequiredFn(p: RhoTypeN): Boolean = p match {

    /** Basic types */
    case _: NilN           => false
    case pProc: ParProcN   => sReq(pProc.ps)
    case send: SendN       => sReq(send.chan) || sReq(send.data)
    case receive: ReceiveN => sReq(receive.binds) || sReq(receive.body)
    case m: MatchN         => sReq(m.target) || sReq(m.cases)
    case n: NewN           => sReq(n.p)

    /** Ground types */
    case _: GroundN => false

    /** Collections */
    case eList: EListN   => sReq(eList.ps)
    case eTuple: ETupleN => sReq(eTuple.ps)
    case eSet: ESetN     => sReq(eSet.sortedPs)

    /** Vars */
    case _: BoundVarN => true
    case _: FreeVarN  => false
    case _: WildcardN => false

    /** Unforgeable names */
    case _: UnforgeableN => false

    /** Operations */
    case op: Operation1ParN  => sReq(op.p)
    case op: Operation2ParN  => sReq(op.p1) || sReq(op.p2)
    case eMethod: EMethodN   => sReq(eMethod.target) || sReq(eMethod.arguments)
    case eMatches: EMatchesN => sReq(eMatches.target) || sReq(eMatches.pattern)

    /** Connective */
    case _: ConnectiveSTypeN => false
    case connNot: ConnNotN   => sReq(connNot.p)
    case connAnd: ConnAndN   => sReq(connAnd.ps)
    case connOr: ConnOrN     => sReq(connOr.ps)
    case _: ConnVarRefN      => true

    /** Auxiliary types */
    case bind: ReceiveBindN => sReq(bind.patterns) || sReq(bind.source)
    case mCase: MatchCaseN  => sReq(mCase.pattern) || sReq(mCase.source)

    /** Other types */
    case bundle: BundleN  => sReq(bundle.body)
    case _: SysAuthTokenN => false

    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
