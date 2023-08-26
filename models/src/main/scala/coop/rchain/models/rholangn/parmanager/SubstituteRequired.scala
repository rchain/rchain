package coop.rchain.models.rholangn.parmanager

import coop.rchain.models.rholangn._

private[parmanager] object SubstituteRequired {
  private def sReq(p: RhoTypeN): Boolean = p.substituteRequired
  private def sReq(kv: (RhoTypeN, RhoTypeN)): Boolean =
    kv._1.substituteRequired || kv._2.substituteRequired
  private def sReq(ps: Seq[RhoTypeN]): Boolean                         = ps.exists(sReq)
  private def sReqKVPairs(kVPairs: Seq[(RhoTypeN, RhoTypeN)]): Boolean = kVPairs.exists(sReq)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def substituteRequiredFn(p: RhoTypeN): Boolean = p match {

    /** Basic types */
    case _: NilN.type => false
    case p: ParProcN  => sReq(p.ps)
    case p: SendN     => sReq(p.chan) || sReq(p.args)
    case p: ReceiveN  => sReq(p.binds) || sReq(p.body)
    case p: MatchN    => sReq(p.target) || sReq(p.cases)
    case p: NewN      => sReq(p.p)

    /** Ground types */
    case _: GroundN => false

    /** Collections */
    case eList: EListN   => sReq(eList.ps)
    case eTuple: ETupleN => sReq(eTuple.ps)
    case eSet: ESetN     => sReq(eSet.ps.toSeq)
    case eMap: EMapN     => sReqKVPairs(eMap.ps.toSeq)

    /** Vars */
    case _: BoundVarN      => true
    case _: FreeVarN       => false
    case _: WildcardN.type => false

    /** Operations */
    case p: Operation1ParN => sReq(p.p)
    case p: Operation2ParN => sReq(p.p1) || sReq(p.p2)
    case p: EMethodN       => sReq(p.target) || sReq(p.args)
    case p: EMatchesN      => sReq(p.target) || sReq(p.pattern)

    /** Unforgeable names */
    case _: UnforgeableN => false

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
    case bundle: BundleN => sReq(bundle.body)

    case x => throw new Exception(s"Undefined type $x")
  }
}
