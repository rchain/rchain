package coop.rchain.models.rholangn.parmanager

import cats.Eval
import cats.syntax.all._
import com.google.protobuf.CodedOutputStream
import coop.rchain.models.rholangn._

import scala.annotation.unused

private object ProtobufSerializedSize {
  import Constants._

  // Terminal expressions
  def sSize(bytes: Array[Byte]): Eval[Int] =
    Eval.later(CodedOutputStream.computeByteArraySizeNoTag(bytes))
  def sSize(@unused v: Boolean): Eval[Int] = Eval.now(booleanSize)
  def sSize(v: Int): Eval[Int]             = Eval.later(CodedOutputStream.computeInt32SizeNoTag(v))
  def sSize(v: Long): Eval[Int]            = Eval.later(CodedOutputStream.computeInt64SizeNoTag(v))
  def sSize(v: String): Eval[Int]          = Eval.later(CodedOutputStream.computeStringSizeNoTag(v))
  def sSize(v: BigInt): Eval[Int]          = sSize(v.toByteArray)

  // Recursive traversal with memoization of serialized size on children objects
  def sSize(x: RhoTypeN): Eval[Int] = x.serializedSize

  // Recursive traversal of a sequence with memoization of serialized size on children objects
  def sSize(ps: Seq[RhoTypeN]): Eval[Int] = sSizeSeq[RhoTypeN](ps, sSize)

  def sSize(kv: (RhoTypeN, RhoTypeN)): Eval[Int] =
    kv.bimap(sSize, sSize).mapN(_ + _)

  def sSize(pOpt: Option[RhoTypeN]): Eval[Int] =
    (Eval.now(booleanSize), pOpt.traverse(sSize)).mapN(_ + _.getOrElse(0))

  def sSizeSeqTuplePar(seq: Seq[(RhoTypeN, RhoTypeN)]): Eval[Int] =
    sSizeSeq[(RhoTypeN, RhoTypeN)](seq, sSize)

  def totalSize(sizes: Int*): Int = tagSize + sizes.sum

  // Calculates serialized size of a sequence (the sum of element sizes)
  def sSizeSeq[T](seq: Seq[T], f: T => Eval[Int]): Eval[Int] =
    (sSize(seq.size), seq.traverse(f).map(_.sum)).mapN(_ + _)
}

private[parmanager] object SerializedSize {
  import ProtobufSerializedSize._

  def sSizeReceiveBind(p: ReceiveBindN): Eval[Int] =
    (sSize(p.patterns), sSize(p.source), sSize(p.remainder), sSize(p.freeCount))
      .mapN(totalSize(_, _, _, _))

  def sSizeMatchCase(p: MatchCaseN): Eval[Int] =
    (sSize(p.pattern), sSize(p.source), sSize(p.freeCount)).mapN(totalSize(_, _, _))

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def calcSerSize(input: RhoTypeN): Eval[Int] = Eval.defer {
    input match {

      /* Terminal expressions (0-arity constructors) */
      /* =========================================== */

      case _: NilN.type      => Eval.now(totalSize())
      case p: GBoolN         => sSize(p.v).map(totalSize(_))
      case p: GIntN          => sSize(p.v).map(totalSize(_))
      case p: GBigIntN       => sSize(p.v).map(totalSize(_))
      case p: GStringN       => sSize(p.v).map(totalSize(_))
      case p: GByteArrayN    => sSize(p.v).map(totalSize(_))
      case p: GUriN          => sSize(p.v).map(totalSize(_))
      case _: WildcardN.type => Eval.now(totalSize())

      /* Unforgeable names */
      case p: UnforgeableN => sSize(p.v).map(totalSize(_))

      /* Vars */
      case p: BoundVarN   => sSize(p.idx).map(totalSize(_))
      case p: FreeVarN    => sSize(p.idx).map(totalSize(_))
      case p: ConnVarRefN => (sSize(p.index), sSize(p.depth)).mapN(totalSize(_, _))

      /* Simple types */
      case _: ConnectiveSTypeN => Eval.now(totalSize())

      /* Unary expressions (1-arity constructors) */
      /* ======================================== */

      case p: Operation1ParN => sSize(p.p).map(totalSize(_))

      case p: BundleN =>
        (sSize(p.body), sSize(p.writeFlag), sSize(p.readFlag)).mapN(totalSize(_, _, _))

      /* Connective */
      case p: ConnNotN => sSize(p.p).map(totalSize(_))

      /* Binary expressions (2-arity constructors) */
      /* ========================================= */

      case p: Operation2ParN => (sSize(p.p1), sSize(p.p2)).mapN(totalSize(_, _))

      case p: EMatchesN =>
        (sSize(p.target), sSize(p.pattern)).mapN(totalSize(_, _))

      /* N-ary parameter expressions (N-arity constructors) */
      /* ================================================== */

      case p: ParProcN => sSize(p.ps).map(totalSize(_))

      case p: SendN => (sSize(p.chan), sSize(p.args), sSize(p.persistent)).mapN(totalSize(_, _, _))

      case p: ReceiveN =>
        val bindsSize = p.binds.traverse(sSizeReceiveBind).map(totalSize)
        (bindsSize, sSize(p.body), sSize(p.persistent), sSize(p.peek), sSize(p.bindCount))
          .mapN(totalSize(_, _, _, _, _))

      case p: MatchN =>
        val casesSize = p.cases.traverse(sSizeMatchCase).map(totalSize)
        (sSize(p.target), casesSize).mapN(totalSize(_, _))

      case p: NewN =>
        (sSize(p.bindCount), sSize(p.p), sSize(p.uri), sSizeSeqTuplePar(p.injections.toSeq))
          .mapN(totalSize(_, _, _, _))

      /* Collections */
      case p: EListN  => (sSize(p.ps), sSize(p.remainder)).mapN(totalSize(_, _))
      case p: ETupleN => sSize(p.ps).map(totalSize(_))
      case p: ESetN   => (sSize(p.ps.toSeq), sSize(p.remainder)).mapN(totalSize(_, _))
      case p: EMapN   => (sSizeSeqTuplePar(p.ps.toSeq), sSize(p.remainder)).mapN(totalSize(_, _))

      /* Connective */
      case p: ConnAndN => sSize(p.ps).map(totalSize(_))
      case p: ConnOrN  => sSize(p.ps).map(totalSize(_))

      case p: EMethodN =>
        (sSize(p.methodName), sSize(p.target), sSize(p.args)).mapN(totalSize(_, _, _))

      case p => throw new Exception(s"Undefined type $p")
    }
  }
}
