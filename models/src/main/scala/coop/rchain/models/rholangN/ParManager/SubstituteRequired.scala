package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

import scala.annotation.unused

private[ParManager] object SubstituteRequired {
  private def sRequiredParSeq(ps: Seq[ParN]) = ps.exists(_.substituteRequired)

  def substituteRequiredFn(p: RhoTypeN): Boolean = p match {

    /** Main types */
    case pproc: ParProcN => sRequiredParSeq(pproc.ps)
    case send: SendN     => sRequiredParSeq(send.data)

    /** Ground types */
    case _: GNilN => false
    case _: GIntN => false

    /** Collections */
    case list: EListN => sRequiredParSeq(list.ps)

    /** Vars */
    case _: BoundVar => true
    case _: FreeVar  => false
    case _: Wildcard => false

    /** Expr */
    /** Bundle */
    /** Connective */
    case _ =>
      assert(assertion = false, "Not defined type")
      false
  }
}
