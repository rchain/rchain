package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

import scala.annotation.unused

private[ParManager] object ConnectiveUsed {
  private def cUsedParSeq(ps: Seq[ParN]) = ps.exists(_.connectiveUsed)

  def connectiveUsedFn(p: RhoTypeN): Boolean = p match {

    /** Main types */
    case pproc: ParProcN => cUsedParSeq(pproc.ps)
    case send: SendN     => send.chan.connectiveUsed || cUsedParSeq(send.data)

    /** Ground types */
    case _: GNilN => false
    case _: GIntN => false

    /** Collections */
    case list: EListN => cUsedParSeq(list.ps) || list.remainder.isDefined

    /** Vars */
    case _: BoundVarN => false
    case _: FreeVarN  => true
    case _: WildcardN => true

    /** Expr */
    /** Bundle */
    /** Connective */
    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
