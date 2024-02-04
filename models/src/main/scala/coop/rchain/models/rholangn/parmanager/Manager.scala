package coop.rchain.models.rholangn.parmanager

import cats.Eval
import com.google.protobuf.CodedOutputStream
import coop.rchain.models.rholangn._
import coop.rchain.models.rholangn.parmanager.protobuf.{
  ProtoCodec,
  ProtoPrimitiveReader,
  ProtoPrimitiveWriter
}

import java.io.InputStream

object Manager {
  private def flatPs(ps: Seq[ParN]): Seq[ParN] =
    ps.flatMap {
      case _: NilN.type => Seq()
      case x: ParProcN  => flatPs(x.ps)
      case p            => Seq(p)
    }

  private def makePProc(ps: Seq[ParN]): ParN = ps match {
    case Nil      => NilN
    case p :: Nil => p
    case _        => ParProcN(ps)
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
  def rhoHashFn(p: RhoTypeN): Eval[Array[Byte]] = RhoHash.calcHash(p)
  def serializedSizeFn(p: RhoTypeN): Eval[Int]  = SerializedSize.calcSerSize(p)
  def serializedFn(p: RhoTypeN, memoizeChildren: Boolean): Eval[Array[Byte]] = {
    val write = (out: CodedOutputStream) =>
      Serialization.serialize(p, ProtoPrimitiveWriter(out), memoizeChildren)
    p.serializedSize.flatMap(size => ProtoCodec.encode(size, write))
  }
  def connectiveUsedFn(p: RhoTypeN): Eval[Boolean] = ConnectiveUsed.connectiveUsedFn(p)
  def evalRequiredFn(p: RhoTypeN): Boolean         = EvalRequired.evalRequiredFn(p)
  def substituteRequiredFn(p: RhoTypeN): Boolean   = SubstituteRequired.substituteRequiredFn(p)

  // Deserialize with protobuf
  def protoDeserialize(bytes: Array[Byte]): ParN = {
    val decode = (in: InputStream) => Serialization.deserialize(ProtoPrimitiveReader(in))
    ProtoCodec.decode(bytes, decode).value
  }
}
