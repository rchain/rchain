package coop.rchain.models.rholangN

import coop.rchain.models.rholangN.ParManager.Manager._
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.bits.ByteVector

/** Base trait for Rholang elements in the Reducer */
sealed trait RhoTypeN {

  /** Cryptographic hash code of the element */
  lazy val rhoHash: Blake2b256Hash = rhoHashFn(this)

  /** Element size after serialization (in bytes) */
  lazy val serializedSize: Int = serializedSizeFn(this)

  /** True if the element or at least one of the nested elements non-concrete.
    * Such element cannot be viewed as if it were a term.*/
  // TODO: Rename connectiveUsed for more clarity
  lazy val connectiveUsed: Boolean = connectiveUsedFn(this)

  /** True if the element or at least one of the nested elements can be evaluate in Reducer */
  lazy val evalRequired: Boolean = evalRequiredFn(this)

  /** True if the element or at least one of the nested elements can be substitute in Reducer */
  lazy val substituteRequired: Boolean = substituteRequiredFn(this)

  override def equals(x: Any): Boolean = ParManager.Manager.equals(this, x)
}

/* TODO: In the future, it is necessary to append the classification.
         Add main types and ground types.
         Ground types must be part of expressions, and expressions are part of the main types.
 */
/** Auxiliary elements included in other pairs */
trait AuxParN extends RhoTypeN

/** Rholang element that can be processed in parallel, together with other elements */
trait ParN extends RhoTypeN {
  def toBytes: ByteVector = parToBytes(this)
}
object ParN {
  def fromBytes(bytes: ByteVector): ParN = parFromBytes(bytes)
}

/** Expressions included in Rholang elements  */
trait ExprN extends ParN

/** Variables in Rholang (can be bound, free and wildcard) */
trait VarN extends ParN
