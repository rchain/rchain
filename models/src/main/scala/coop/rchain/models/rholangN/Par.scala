package coop.rchain.models.rholangN

import scodec.bits.ByteVector
import coop.rchain.rspace.hashing.Blake2b256Hash

import scala.collection.BitSet
import coop.rchain.models.rholangN.ParManager.Manager._

sealed trait RhoTypeN {
  protected def meta: ParMetaData

  override def equals(x: Any): Boolean = ParManager.Manager.equals(this, x)

  lazy val serializedSize: Int         = meta.serializedSizeFn()
  lazy val rhoHash: Blake2b256Hash     = meta.rhoHashFn()
  lazy val locallyFree: BitSet         = meta.locallyFreeFn()
  lazy val connectiveUsed: Boolean     = meta.connectiveUsedFn()
  lazy val evalRequired: Boolean       = meta.evalRequiredFn()
  lazy val substituteRequired: Boolean = meta.substituteRequiredFn()

  def toBytes: ByteVector = parToBytes(this)
}
object RhoTypeN {
  def fromBytes(bytes: ByteVector): RhoTypeN = parFromBytes(bytes)
}
sealed trait ParN    extends RhoTypeN
sealed trait AuxParN extends RhoTypeN
sealed trait ExprN   extends ParN
sealed trait VarN    extends ParN

final class ParMetaData(
    val rhoHashFn: () => Blake2b256Hash,
    val serializedSizeFn: () => Int,
    val locallyFreeFn: () => BitSet,
    val connectiveUsedFn: () => Boolean,
    val evalRequiredFn: () => Boolean,
    val substituteRequiredFn: () => Boolean
)

/** Main types */
class ParProcN(val ps: Seq[ParN], protected val meta: ParMetaData) extends ParN {
  def add(p: ParN): ParProcN = ParProcN(ps :+ p)
}
object ParProcN {
  def apply(): ParProcN              = createParProc(Seq())
  def apply(p: ParN): ParProcN       = createParProc(Seq(p))
  def apply(ps: Seq[ParN]): ParProcN = createParProc(ps)
}

final class SendN(
                   val chan: ParN,
                   val data: Seq[ParN],
                   val persistent: Boolean,
                   protected val meta: ParMetaData
                 ) extends ParN
object SendN {
  def apply(chan: ParN, data: ParN, persistent: Boolean): SendN =
    createSend(chan, Seq(data), persistent)
  def apply(chan: ParN, data: Seq[ParN], persistent: Boolean): SendN =
    createSend(chan, data, persistent)
}

//final class ReceiveN(
//    val binds: Seq[ReceiveBindN],
//    val body: ParN,
//    val persistent: Boolean,
//    val peek: Boolean,
//    val bindCount: Int,
//    protected val meta: ParMetaData
//) extends ParN
//
//final class ReceiveBindN(
//    val patterns: Seq[ParN],
//    val source: ParN,
//    val remainder: Option[VarN],
//    val freeCount: Int
//    protected val meta: ParMetaData
//) extends AuxParN

//final class MatchN(val target: ParN, val cases: Seq[MatchCase])
//final class MatchCase(val pattern: ParN, val source: ParN, val freeCount: Int = 0)

/** Ground types */
final class GNilN(protected val meta: ParMetaData) extends ParN
object GNilN { def apply(): GNilN = createGNil }

final class GIntN(val v: Long, protected val meta: ParMetaData) extends ExprN
object GIntN { def apply(v: Long): GIntN = createGInt(v) }

/** Collections */
final class EListN(val ps: Seq[ParN], val remainder: Option[VarN], protected val meta: ParMetaData)
    extends ExprN
object EListN {
  def apply(ps: Seq[ParN] = Seq(), r: Option[VarN] = None): EListN = createEList(ps, r)
  def apply(p: ParN): EListN                                       = apply(Seq(p), None)
}

/** Vars */
final class BoundVar(val value: Int, protected val meta: ParMetaData) extends VarN
object BoundVar { def apply(value: Int): BoundVar = createBoundVar(value) }

final class FreeVar(val value: Int, protected val meta: ParMetaData) extends VarN
object FreeVar { def apply(value: Int): FreeVar = createFreeVar(value) }

final class Wildcard(protected val meta: ParMetaData) extends VarN
object Wildcard { def apply(): Wildcard = createWildcard }

/** Expr */

/** Bundle */

/** Connective */
//final class VarRefN(index: Int = 0, depth: Int = 0)
