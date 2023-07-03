package coop.rchain.models.rholangN.ParManager

import coop.rchain.models.rholangN._
import coop.rchain.rspace.hashing.Blake2b256Hash
import scodec.bits.ByteVector

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

object Manager {

  def parToBytes(p: ParN): ByteVector = {
    val baos = new ByteArrayOutputStream(p.serializedSize)
    Serialization.serialize(p, baos)
    ByteVector(baos.toByteArray)
  }

  def parFromBytes(bv: ByteVector): ParN = {
    val bais = new ByteArrayInputStream(bv.toArray)
    Serialization.deserialize(bais)
  }

  def equals(self: RhoTypeN, other: Any): Boolean = other match {
    case x: RhoTypeN => x.rhoHash == self.rhoHash
    case _           => false
  }

  def sortPars(ps: Seq[ParN]): Seq[ParN]             = Sorting.sortPars(ps)
  def sortStrings(strings: Seq[String]): Seq[String] = Sorting.sortStrings(strings)
  def comparePars(p1: ParN, p2: ParN): Int           = Sorting.comparePars(p1, p2)

  /** MetaData */
  def rhoHashFn(p: RhoTypeN): Blake2b256Hash     = RhoHash.rhoHashFn(p)
  def serializedSizeFn(p: RhoTypeN): Int         = SerializedSize.serializedSizeFn(p)
  def connectiveUsedFn(p: RhoTypeN): Boolean     = ConnectiveUsed.connectiveUsedFn(p)
  def evalRequiredFn(p: RhoTypeN): Boolean       = EvalRequired.evalRequiredFn(p)
  def substituteRequiredFn(p: RhoTypeN): Boolean = SubstituteRequired.substituteRequiredFn(p)

}
