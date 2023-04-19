package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

import scala.annotation.unused

private[ParManager] object ConnectiveUsed {
  private def cUsedParSeq(ps: Seq[ParN])            = ps.exists(_.connectiveUsed)

  /** Main types */
  def connectiveUsedParProc(ps: Seq[ParN]): Boolean = cUsedParSeq(ps)
  def connectiveUsedSend(chan: ParN, data: Seq[ParN], @unused persistent: Boolean): Boolean =
    chan.connectiveUsed || cUsedParSeq(data)

  /** Ground types */
  def connectiveUsedGNil(): Boolean                 = false
  def connectiveUsedGInt(@unused v: Long): Boolean  = false

  /** Collections */
  def connectiveUsedEList(ps: Seq[ParN]): Boolean   = cUsedParSeq(ps)

  /** Vars */
  def connectiveUsedBoundVar(@unused value: Int): Boolean = false
  def connectiveUsedFreeVar(@unused value: Int): Boolean  = true
  def connectiveUsedWildcard(): Boolean                   = true

  /** Expr */

  /** Bundle */

  /** Connective */
}
