package coop.rchain.models.rholangn.parmanager

import coop.rchain.models.rholangn._
import coop.rchain.rspace.hashing.Blake2b256Hash

import java.io.{ByteArrayInputStream, ByteArrayOutputStream}

object Manager {

  def parToBytes(p: ParN): Array[Byte] = {
    val baos = new ByteArrayOutputStream(p.serializedSize)
    Serialization.serialize(p, baos)
    baos.toByteArray
  }

  def parFromBytes(bv: Array[Byte]): ParN = {
    val bais = new ByteArrayInputStream(bv)
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

  private def flatPs(ps: Seq[ParN]): Seq[ParN] =
    ps.flatMap {
      case _: NilN.type => Seq()
      case x: ParProcN  => flatPs(x.ps)
      case p            => Seq(p)
    }

  private def makePProc(ps: Seq[ParN]): ParN = ps.length match {
    case 0 => NilN
    case 1 => ps.head
    case _ => ParProcN(ps)
  }

  /**
    * Create a flatten parallel Par (ParProc) from par sequence
    * Flatting is the process of transforming ParProc(P, Q, ...):
    * - empty data:  ParProc()  -> Nil
    * - single data: ParProc(P) -> P
    * - nil data:    ParProc(P, Q, Nil) -> ParProc(P, Q)
    * - nested data  ParProc(ParProc(P,Q), ParProc(L,K)) -> ParProc(P, Q, L, K)
    * @param ps initial par sequence to be executed in parallel
    * @return
    */
  def flattedPProc(ps: Seq[ParN]): ParN = makePProc(flatPs(ps))

  /**
    * Create a flatten parallel Par (ParProc) from two Pars.
    * See [[flattedPProc]] for more information.
    */
  def combinePars(p1: ParN, p2: ParN): ParN = flattedPProc(Seq(p1, p2))

  /** MetaData */
  def rhoHashFn(p: RhoTypeN): Blake2b256Hash     = RhoHash.rhoHashFn(p)
  def serializedSizeFn(p: RhoTypeN): Int         = SerializedSize.serializedSizeFn(p)
  def connectiveUsedFn(p: RhoTypeN): Boolean     = ConnectiveUsed.connectiveUsedFn(p)
  def evalRequiredFn(p: RhoTypeN): Boolean       = EvalRequired.evalRequiredFn(p)
  def substituteRequiredFn(p: RhoTypeN): Boolean = SubstituteRequired.substituteRequiredFn(p)

}
