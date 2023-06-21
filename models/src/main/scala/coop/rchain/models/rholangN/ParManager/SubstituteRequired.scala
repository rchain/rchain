package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object SubstituteRequired {
  private def sReq(p: RhoTypeN): Boolean       = p.substituteRequired
  private def sReq(ps: Seq[RhoTypeN]): Boolean = ps.exists(sReq)

  def substituteRequiredFn(p: RhoTypeN): Boolean = p match {

    /** Main types */
    case pproc: ParProcN   => sReq(pproc.ps)
    case send: SendN       => sReq(send.chan) || sReq(send.data)
    case receive: ReceiveN => sReq(receive.binds) || sReq(receive.body)

    /** Ground types */
    case _: GNilN => false
    case _: GIntN => false

    /** Collections */
    case list: EListN => sReq(list.ps)

    /** Vars */
    case _: BoundVarN => true
    case _: FreeVarN  => false
    case _: WildcardN => false

    /** Expr */
    /** Bundle */
    /** Connective */
    /** Auxiliary types */
    case bind: ReceiveBindN => sReq(bind.patterns) || sReq(bind.source)

    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
