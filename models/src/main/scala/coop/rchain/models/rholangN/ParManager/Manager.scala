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

  def sortPars(ps: Seq[ParN]): Seq[ParN]                  = Sorting.sortPars(ps)
  def sortBinds(bs: Seq[ReceiveBindN]): Seq[ReceiveBindN] = Sorting.sortBinds(bs)
  def sortBindsWithT[T](bs: Seq[(ReceiveBindN, T)]): Seq[(ReceiveBindN, T)] =
    Sorting.sortBindsWithT(bs)
  def sortUris(uris: Seq[String]): Seq[String] = Sorting.sortUris(uris)
  def sortInjections(injections: Map[String, ParN]): Seq[(String, ParN)] =
    Sorting.sortInjections(injections)
  def comparePars(p1: ParN, p2: ParN): Int = Sorting.comparePars(p1, p2)

  def addPar(p1: ParN, p2: ParN): ParN = p1 match {
    case _: NilN         => p2
    case pProc: ParProcN => pProc.addPar(p2)
    case _               => ParProcN(Seq(p2, p1))
  }

  /** MetaData */
  def rhoHashFn(p: RhoTypeN): Blake2b256Hash     = RhoHash.rhoHashFn(p)
  def serializedSizeFn(p: RhoTypeN): Int         = SerializedSize.serializedSizeFn(p)
  def connectiveUsedFn(p: RhoTypeN): Boolean     = ConnectiveUsed.connectiveUsedFn(p)
  def evalRequiredFn(p: RhoTypeN): Boolean       = EvalRequired.evalRequiredFn(p)
  def substituteRequiredFn(p: RhoTypeN): Boolean = SubstituteRequired.substituteRequiredFn(p)

}
