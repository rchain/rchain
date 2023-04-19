package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

import scala.annotation.unused

private[ParManager] object SubstituteRequired {
  private def sRequiredParSeq(ps: Seq[ParN]) = ps.exists(_.substituteRequired)

  /** Main types */
  def substituteRequiredParProc(ps: Seq[ParN]): Boolean = sRequiredParSeq(ps)

  def substituteRequiredSend(
                              @unused chan: ParN,
                              data: Seq[ParN],
                              @unused persistent: Boolean
                            ): Boolean =
    sRequiredParSeq(data)

  /** Ground types */
  def substituteRequiredGNil(): Boolean = false

  def substituteRequiredGInt(@unused v: Long): Boolean = false

  /** Collections */
  def substituteRequiredEList(ps: Seq[ParN]): Boolean = sRequiredParSeq(ps)

  /** Vars */
  def substituteRequiredBoundVar(@unused value: Int): Boolean = true

  def substituteRequiredFreeVar(@unused value: Int): Boolean = false

  def substituteRequiredWildcard(): Boolean = false

  /** Expr */

  /** Bundle */

  /** Connective */
}

