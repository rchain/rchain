package coop.rchain.models.rholangn.parmanager

import coop.rchain.models.rholangn._

object SubstituteRequired {
  def sReq(p: RhoTypeN): Boolean                               = p.substituteRequired
  def sReq(kv: (RhoTypeN, RhoTypeN)): Boolean                  = kv._1.substituteRequired || kv._2.substituteRequired
  def sReq(ps: Seq[RhoTypeN]): Boolean                         = ps.exists(sReq)
  def sReqKVPairs(kVPairs: Seq[(RhoTypeN, RhoTypeN)]): Boolean = kVPairs.exists(sReq)
  def sReqReceiveBind(p: ReceiveBindN): Boolean                = sReq(p.patterns) || sReq(p.source)
  def sReqMatchCase(p: MatchCaseN): Boolean                    = sReq(p.pattern) || sReq(p.source)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def substituteRequiredFn(input: RhoTypeN): Boolean = input match {

    /** Basic types */
    case _: NilN.type => false
    case p: ParProcN  => sReq(p.ps)
    case p: SendN     => sReq(p.chan) || sReq(p.args)
    case p: ReceiveN  => p.binds.exists(sReqReceiveBind) || sReq(p.body)
    case p: MatchN    => sReq(p.target) || p.cases.exists(sReqMatchCase)
    case p: NewN      => sReq(p.p)

    /** Ground types */
    case _: GroundN => false

    /** Collections */
    case p: EListN  => sReq(p.ps)
    case p: ETupleN => sReq(p.ps)
    case p: ESetN   => sReq(p.ps.toSeq)
    case p: EMapN   => sReqKVPairs(p.ps.toSeq)

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
    case p: ConnNotN         => sReq(p.p)
    case p: ConnAndN         => sReq(p.ps)
    case p: ConnOrN          => sReq(p.ps)
    case _: ConnVarRefN      => true

    /** Other types */
    case p: BundleN => sReq(p.body)

    case p => throw new Exception(s"Undefined type $p")
  }
}
