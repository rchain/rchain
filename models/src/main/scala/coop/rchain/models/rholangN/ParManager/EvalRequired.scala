package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

private[ParManager] object EvalRequired {
  private def erPar(p: RhoTypeN)        = p.evalRequired
  private def erPars(ps: Seq[RhoTypeN]) = ps.exists(erPar)

  def evalRequiredFn(p: RhoTypeN): Boolean = p match {

    /** Main types */
    case pproc: ParProcN   => erPars(pproc.ps)
    case send: SendN       => erPar(send.chan) || erPars(send.data)
    case receive: ReceiveN => erPars(receive.binds)

    /** Ground types */
    case _: GNilN => false
    case _: GIntN => false

    /** Collections */
    case list: EListN => erPars(list.ps)

    /** Vars */
    case _: BoundVarN => true
    case _: FreeVarN  => true
    case _: WildcardN => true

    /** Expr */
    /** Bundle */
    /** Connective */
    /** Auxiliary types */
    case bind: ReceiveBindN => erPar(bind.source)

    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
