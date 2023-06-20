package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

import scala.annotation.unused

private[ParManager] object ConnectiveUsed {
  private def cuPar(p: RhoTypeN)               = p.connectiveUsed
  private def cuPars(ps: Seq[RhoTypeN])        = ps.exists(cuPar)
  private def cuParOpt(pOpt: Option[RhoTypeN]) = if (pOpt.isDefined) cuPar(pOpt.get) else false

  def connectiveUsedFn(p: RhoTypeN): Boolean = p match {

    /** Main types */
    case pproc: ParProcN   => cuPars(pproc.ps)
    case send: SendN       => cuPar(send.chan) || cuPars(send.data)
    case receive: ReceiveN => cuPars(receive.binds) || cuPar(receive.body)

    /** Ground types */
    case _: GNilN => false
    case _: GIntN => false

    /** Collections */
    case list: EListN => cuPars(list.ps) || cuParOpt(list.remainder)

    /** Vars */
    case _: BoundVarN => false
    case _: FreeVarN  => true
    case _: WildcardN => true

    /** Expr */
    /** Bundle */
    /** Connective */
    /** Auxiliary types */
    case bind: ReceiveBindN => cuPar(bind.source)

    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
