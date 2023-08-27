package coop.rchain.models.rholangn.parmanager

import coop.rchain.models.rholangn._

object EvalRequired {
  def eReq(p: RhoTypeN): Boolean                               = p.evalRequired
  def eReq(kv: (RhoTypeN, RhoTypeN)): Boolean                  = eReq(kv._1) || eReq(kv._2)
  def eReq(ps: Seq[RhoTypeN]): Boolean                         = ps.exists(eReq)
  def eReqKVPairs(kVPairs: Seq[(RhoTypeN, RhoTypeN)]): Boolean = kVPairs.exists(eReq)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def evalRequiredFn(input: RhoTypeN): Boolean = input match {

    /** Basic types */
    case p: BasicN =>
      p match {
        case _: NilN.type    => false
        case pProc: ParProcN => eReq(pProc.ps)
        case _               => true
      }

    /** Ground types */
    case _: GroundN => false

    /** Collections */
    case p: EListN  => eReq(p.ps)
    case p: ETupleN => eReq(p.ps)
    case p: ESetN   => eReq(p.ps.toSeq)
    case p: EMapN   => eReqKVPairs(p.ps.toSeq)

    /** Vars */
    case _: VarN => true

    /** Operations */
    case _: OperationN => true

    /** Unforgeable names */
    case _: UnforgeableN => false

    /** Connective */
    case _: ConnectiveN => false

    /** Other types */
    case p: BundleN => eReq(p.body)

    case p => throw new Exception(s"Undefined type $p")
  }
}
