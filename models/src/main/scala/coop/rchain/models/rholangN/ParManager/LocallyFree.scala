package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

import scala.annotation.unused
import scala.collection.BitSet

private[ParManager] object LocallyFree {
  private def locallyFreeParSeq(ps: Seq[ParN]) =
    ps.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)

  /** Main types */
  def locallyFreeParProc(ps: Seq[ParN]): BitSet = locallyFreeParSeq(ps)
  def locallyFreeSend(chan: ParN, data: Seq[ParN], @unused persistent: Boolean): BitSet =
    chan.locallyFree | locallyFreeParSeq(data)

  /** Ground types */
  def locallyFreeGNil(): BitSet                 = BitSet()
  def locallyFreeGInt(@unused v: Long): BitSet  = BitSet()

  /** Collections */
  def locallyFreeEList(ps: Seq[ParN]): BitSet   = locallyFreeParSeq(ps)

  /** Vars */
  def locallyFreeBoundVar(value: Int): BitSet        = BitSet(value)
  def locallyFreeFreeVar(@unused value: Int): BitSet = BitSet()
  def locallyFreeWildcard(): BitSet                  = BitSet()

  /** Expr */

  /** Bundle */

  /** Connective */
}