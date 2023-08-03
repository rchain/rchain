package coop.rchain.models.rholangn.parmanager

import coop.rchain.models.rholangn._

private[parmanager] object EvalRequired {
  private def eReq(p: RhoTypeN): Boolean                               = p.evalRequired
  private def eReq(kv: (RhoTypeN, RhoTypeN)): Boolean                  = eReq(kv._1) || eReq(kv._2)
  private def eReq(ps: Seq[RhoTypeN]): Boolean                         = ps.exists(eReq)
  private def eReqKVPairs(kVPairs: Seq[(RhoTypeN, RhoTypeN)]): Boolean = kVPairs.exists(eReq)

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def evalRequiredFn(p: RhoTypeN): Boolean = p match {

    /** Basic types */
    case p: BasicN =>
      p match {
        case _: NilN         => false
        case pProc: ParProcN => eReq(pProc.ps)
        case _               => true
      }

    /** Ground types */
    case _: GroundN => false

    /** Collections */
    case eList: EListN   => eReq(eList.ps)
    case eTuple: ETupleN => eReq(eTuple.ps)
    case eSet: ESetN     => eReq(eSet.sortedPs)
    case eMap: EMapN     => eReqKVPairs(eMap.sortedPs)

    /** Vars */
    case _: VarN => true

    /** Operations */
    case _: OperationN => true

    /** Unforgeable names */
    case _: UnforgeableN => false

    /** Connective */
    case _: ConnectiveN => false

    /** Auxiliary types */
    case _: ReceiveBindN => true
    case _: MatchCaseN   => true

    /** Other types */
    case bundle: BundleN => eReq(bundle.body)

    case _ => throw new Exception("Not defined type")
  }
}
