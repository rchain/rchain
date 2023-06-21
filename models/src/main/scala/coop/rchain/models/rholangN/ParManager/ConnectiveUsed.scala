package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object ConnectiveUsed {
  private def cUsed(p: RhoTypeN): Boolean       = p.connectiveUsed
  private def cUsed(ps: Seq[RhoTypeN]): Boolean = ps.exists(cUsed)
  private def cUsed(pOpt: Option[RhoTypeN]): Boolean =
    if (pOpt.isDefined) cUsed(pOpt.get) else false

  def connectiveUsedFn(p: RhoTypeN): Boolean = p match {

    /** Main types */
    case pproc: ParProcN   => cUsed(pproc.ps)
    case send: SendN       => cUsed(send.chan) || cUsed(send.data)
    case receive: ReceiveN => cUsed(receive.binds) || cUsed(receive.body)

    /** Ground types */
    case _: GNilN => false
    case _: GIntN => false

    /** Collections */
    case list: EListN => cUsed(list.ps) || cUsed(list.remainder)

    /** Vars */
    case _: BoundVarN => false
    case _: FreeVarN  => true
    case _: WildcardN => true

    /** Expr */
    /** Bundle */
    /** Connective */
    /** Auxiliary types */
    case bind: ReceiveBindN => cUsed(bind.source)

    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
