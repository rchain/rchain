package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object SubstituteRequired {
  private def srPar(p: RhoTypeN)        = p.substituteRequired
  private def srPars(ps: Seq[RhoTypeN]) = ps.exists(srPar)

  def substituteRequiredFn(p: RhoTypeN): Boolean = p match {

    /** Main types */
    case pproc: ParProcN   => srPars(pproc.ps)
    case send: SendN       => srPar(send.chan) || srPars(send.data)
    case receive: ReceiveN => srPars(receive.binds) || srPar(receive.body)

    /** Ground types */
    case _: GNilN => false
    case _: GIntN => false

    /** Collections */
    case list: EListN => srPars(list.ps)

    /** Vars */
    case _: BoundVarN => true
    case _: FreeVarN  => false
    case _: WildcardN => false

    /** Expr */
    /** Bundle */
    /** Connective */
    /** Auxiliary types */
    case bind: ReceiveBindN => srPars(bind.patterns) || srPar(bind.source)

    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
