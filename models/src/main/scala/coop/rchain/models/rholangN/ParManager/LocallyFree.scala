package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._

import scala.annotation.unused
import scala.collection.BitSet

private[ParManager] object LocallyFree {
  private def locallyFreeParSeq(ps: Seq[ParN]) =
    ps.foldLeft(BitSet())((acc, p) => acc | p.locallyFree)

  def locallyFreeFn(p: RhoTypeN): BitSet = p match {

    /** Main types */
    case pproc: ParProcN => locallyFreeParSeq(pproc.ps)
    case send: SendN     => send.chan.locallyFree | locallyFreeParSeq(send.data)

    /** Ground types */
    case _: GNilN => BitSet()
    case _: GIntN => BitSet()

    /** Collections */
    case list: EListN => locallyFreeParSeq(list.ps)

    /** Vars */
    case bv: BoundVarN => BitSet(bv.value)
    case _: FreeVarN   => BitSet()
    case _: WildcardN  => BitSet()

    /** Expr */
    /** Bundle */
    /** Connective */
    case _ =>
      assert(assertion = false, "Not defined type")
      BitSet()

  }
}
