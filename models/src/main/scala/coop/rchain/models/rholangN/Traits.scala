package coop.rchain.models.rholangN

import scodec.bits.ByteVector
import coop.rchain.rspace.hashing.Blake2b256Hash

import scala.collection.BitSet
import coop.rchain.models.rholangN.ParManager.Manager._

sealed trait RhoTypeN {
  override def equals(x: Any): Boolean = ParManager.Manager.equals(this, x)

  lazy val rhoHash: Blake2b256Hash     = rhoHashFn(this)
  lazy val serializedSize: Int         = serializedSizeFn(this)
  lazy val connectiveUsed: Boolean     = connectiveUsedFn(this)
  lazy val evalRequired: Boolean       = evalRequiredFn(this)
  lazy val substituteRequired: Boolean = substituteRequiredFn(this)
}

//trait AuxParN extends RhoTypeN

trait ParN extends RhoTypeN {
  def toBytes: ByteVector = parToBytes(this)
}
object ParN {
  def fromBytes(bytes: ByteVector): ParN = parFromBytes(bytes)
}
trait ExprN extends ParN
trait VarN  extends ParN

//final class ReceiveN(
//    val binds: Seq[ReceiveBindN],
//    val body: ParN,
//    val persistent: Boolean,
//    val peek: Boolean,
//    val bindCount: Int
//) extends ParN
//
//final class ReceiveBindN(
//    val patterns: Seq[ParN],
//    val source: ParN,
//    val remainder: Option[VarN],
//    val freeCount: Int
//) extends AuxParN

//final class MatchN(val target: ParN, val cases: Seq[MatchCase])
//final class MatchCase(val pattern: ParN, val source: ParN, val freeCount: Int = 0)

//final class VarRefN(index: Int = 0, depth: Int = 0)
