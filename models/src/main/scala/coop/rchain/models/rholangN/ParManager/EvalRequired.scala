package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

import scala.annotation.unused

private[ParManager] object EvalRequired {
  private def eRequiredParSeq(ps: Seq[ParN])      = ps.exists(_.evalRequired)

  /** Main types */
  def evalRequiredParProc(ps: Seq[ParN]): Boolean = eRequiredParSeq(ps)
  def evalRequiredSend(
                        @unused chan: ParN,
                        data: Seq[ParN],
                        @unused persistent: Boolean
                      ): Boolean =
    eRequiredParSeq(data)

  /** Ground types */
  def evalRequiredGNil(): Boolean                 = false
  def evalRequiredGInt(@unused v: Long): Boolean  = false

  /** Collections */
  def evalRequiredEList(ps: Seq[ParN]): Boolean   = eRequiredParSeq(ps)

  /** Vars */
  def evalRequiredBoundVar(@unused value: Int): Boolean = true
  def evalRequiredFreeVar(@unused value: Int): Boolean  = true
  def evalRequiredWildcard(): Boolean                   = true

  /** Expr */

  /** Bundle */

  /** Connective */
}
