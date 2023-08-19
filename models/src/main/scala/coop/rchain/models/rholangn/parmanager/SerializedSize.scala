package coop.rchain.models.rholangn.parmanager

import cats.Eval
import cats.syntax.all._
import com.google.protobuf.CodedOutputStream
import coop.rchain.models.rholangn.{RhoTypeN, _}

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

  // Recursive traversal using memoized value
  def sSize(x: RhoTypeN): Eval[Int] = x.serializedSize

  // Recursive traversal of a sequence using memoized values
  def sSize(ps: Seq[RhoTypeN]): Eval[Int] = sSizeSeq[RhoTypeN](ps, sSize)

  def sSize(kv: (RhoTypeN, RhoTypeN)): Eval[Int] =
    kv.bimap(sSize, sSize).mapN(_ + _)

  def sSize(pOpt: Option[RhoTypeN]): Eval[Int] =
    (Eval.now(booleanSize), pOpt.traverse(sSize)).mapN(_ + _.getOrElse(0))

  def sSizeSeqTuplePar(seq: Seq[(RhoTypeN, RhoTypeN)]): Eval[Int] =
    sSizeSeq[(RhoTypeN, RhoTypeN)](seq, sSize)

  def sSizeTupleStringPar(kv: (String, RhoTypeN)): Eval[Int] =
    kv.bimap(sSize, sSize).mapN(_ + _)

  def sSizeSeqTupleStringPar(seq: Seq[(String, RhoTypeN)]): Eval[Int] =
    sSizeSeq[(String, RhoTypeN)](seq, sSizeTupleStringPar)

  def totalSize(sizes: Int*): Int = tagSize + sizes.sum

  // Calculates serialized size of a sequence (the sum of element sizes)
  def sSizeSeq[T](seq: Seq[T], f: T => Eval[Int]): Eval[Int] =
    (sSize(seq.size), seq.traverse(f).map(_.sum)).mapN(_ + _)
}

private[parmanager] object SerializedSize {
  import ProtobufSerializedSize._

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def calcSerSize(p: RhoTypeN): Eval[Int] = Eval.defer {
    p match {

      /* Terminal expressions (0-arity constructors) */
      /* =========================================== */

      case _: NilN.type            => Eval.now(totalSize())
      case gBool: GBoolN           => sSize(gBool.v).map(totalSize(_))
      case gInt: GIntN             => sSize(gInt.v).map(totalSize(_))
      case gBigInt: GBigIntN       => sSize(gBigInt.v).map(totalSize(_))
      case gString: GStringN       => sSize(gString.v).map(totalSize(_))
      case gByteArray: GByteArrayN => sSize(gByteArray.v).map(totalSize(_))
      case gUri: GUriN             => sSize(gUri.v).map(totalSize(_))
      case _: WildcardN.type       => Eval.now(totalSize())

      /* Unforgeable names */
      case unf: UnforgeableN => sSize(unf.v).map(totalSize(_))

      /* Vars */
      case v: BoundVarN   => sSize(v.idx).map(totalSize(_))
      case v: FreeVarN    => sSize(v.idx).map(totalSize(_))
      case v: ConnVarRefN => (sSize(v.index), sSize(v.depth)).mapN(totalSize(_, _))

      /* Simple types */
      case _: ConnectiveSTypeN => Eval.now(totalSize())

      /* Unary expressions (1-arity constructors) */
      /* ======================================== */

      case op: Operation1ParN => sSize(op.p).map(totalSize(_))

      case bundle: BundleN =>
        (sSize(bundle.body), sSize(bundle.writeFlag), sSize(bundle.readFlag))
          .mapN(totalSize(_, _, _))

      /* Connective */
      case connNot: ConnNotN => sSize(connNot.p).map(totalSize(_))

      /* Binary expressions (2-arity constructors) */
      /* ========================================= */

      case op: Operation2ParN => (sSize(op.p1), sSize(op.p2)).mapN(totalSize(_, _))

      case eMatches: EMatchesN =>
        (sSize(eMatches.target), sSize(eMatches.pattern)).mapN(totalSize(_, _))

      /* N-ary parameter expressions (N-arity constructors) */
      /* ================================================== */

      case pProc: ParProcN => sSize(pProc.ps).map(totalSize(_))

      case send: SendN =>
        (sSize(send.chan), sSize(send.data), sSize(send.persistent)).mapN(totalSize(_, _, _))

      case receive: ReceiveN =>
        val bindsSize      = sSize(receive.binds)
        val bodySize       = sSize(receive.body)
        val persistentSize = sSize(receive.persistent)
        val peekSize       = sSize(receive.peek)
        val bindCountSize  = sSize(receive.bindCount)
        (bindsSize, bodySize, persistentSize, peekSize, bindCountSize)
          .mapN(totalSize(_, _, _, _, _))

      case m: MatchN =>
        val targetSize = sSize(m.target)
        val casesSize  = sSize(m.cases)
        (targetSize, casesSize).mapN(totalSize(_, _))

      case n: NewN =>
        val bindCountSize  = sSize(n.bindCount)
        val pSize          = sSize(n.p)
        val uriSize        = sSizeSeq[String](n.uri, sSize)
        val injectionsSize = sSizeSeqTupleStringPar(n.injections.toSeq)
        (bindCountSize, pSize, uriSize, injectionsSize).mapN(totalSize(_, _, _, _))

      /* Collections */
      case list: EListN    => (sSize(list.ps), sSize(list.remainder)).mapN(totalSize(_, _))
      case eTuple: ETupleN => sSize(eTuple.ps).map(totalSize(_))
      case eSet: ESetN     => (sSize(eSet.sortedPs), sSize(eSet.remainder)).mapN(totalSize(_, _))
      case eMap: EMapN =>
        (sSizeSeqTuplePar(eMap.sortedPs), sSize(eMap.remainder)).mapN(totalSize(_, _))

      /* Connective */
      case connAnd: ConnAndN => sSize(connAnd.ps).map(totalSize(_))
      case connOr: ConnOrN   => sSize(connOr.ps).map(totalSize(_))

      case eMethod: EMethodN =>
        val methodNameSize = sSize(eMethod.methodName)
        val targetSize     = sSize(eMethod.target)
        val argumentsSize  = sSize(eMethod.arguments)
        (methodNameSize, targetSize, argumentsSize).mapN(totalSize(_, _, _))

      /* Auxiliary types */

      case bind: ReceiveBindN =>
        val patternsSize  = sSize(bind.patterns)
        val sourceSize    = sSize(bind.source)
        val reminderSize  = sSize(bind.remainder)
        val freeCountSize = sSize(bind.freeCount)
        (patternsSize, sourceSize, reminderSize, freeCountSize).mapN(totalSize(_, _, _, _))

      case mCase: MatchCaseN =>
        val patternSize   = sSize(mCase.pattern)
        val sourceSize    = sSize(mCase.source)
        val freeCountSize = sSize(mCase.freeCount)
        (patternSize, sourceSize, freeCountSize).mapN(totalSize(_, _, _))

      case x => throw new Exception(s"Undefined type $x")
    }
  }
}
