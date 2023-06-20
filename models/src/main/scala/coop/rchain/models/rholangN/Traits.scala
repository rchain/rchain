package coop.rchain.models.rholangN

import scodec.bits.ByteVector
import coop.rchain.rspace.hashing.Blake2b256Hash

import coop.rchain.models.rholangN.ParManager.Manager._

sealed trait RhoTypeN {
  override def equals(x: Any): Boolean = ParManager.Manager.equals(this, x)

  lazy val rhoHash: Blake2b256Hash     = rhoHashFn(this)
  lazy val serializedSize: Int         = serializedSizeFn(this)
  lazy val connectiveUsed: Boolean     = connectiveUsedFn(this)
  lazy val evalRequired: Boolean       = evalRequiredFn(this)
  lazy val substituteRequired: Boolean = substituteRequiredFn(this)
}

trait AuxParN extends RhoTypeN

trait ParN extends RhoTypeN {
  def toBytes: ByteVector = parToBytes(this)
}
object ParN {
  def fromBytes(bytes: ByteVector): ParN = parFromBytes(bytes)
}

/** Any process may be an operand to an expression.
  * Only processes equivalent to a ground process of compatible type will reduce.
  */
trait ExprN extends ParN

/** A variable used as a var should be bound in a process context, not a name
  * context. For example:
  * for (@x <- c1; @y <- c2) { z!(x + y) } is fine, but
  * for (x <- c1; y <- c2) { z!(x + y) } should raise an error.
  */
trait VarN extends ParN

//final class MatchN(val target: ParN, val cases: Seq[MatchCase])
//final class MatchCase(val pattern: ParN, val source: ParN, val freeCount: Int = 0)

//final class VarRefN(index: Int = 0, depth: Int = 0)
