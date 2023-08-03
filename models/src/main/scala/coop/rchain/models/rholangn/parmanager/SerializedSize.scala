package coop.rchain.models.rholangn.parmanager

import com.google.protobuf.CodedOutputStream
import coop.rchain.models.rholangn._

import scala.annotation.unused

private[parmanager] object SerializedSize {

  import Constants._

  private def sSize(bytes: Array[Byte]): Int = CodedOutputStream.computeByteArraySizeNoTag(bytes)

  private def sSize(@unused v: Boolean): Int = booleanSize
  private def sSize(v: Int): Int             = CodedOutputStream.computeInt32SizeNoTag(v)
  private def sSize(v: Long): Int            = CodedOutputStream.computeInt64SizeNoTag(v)
  private def sSize(v: BigInt): Int          = sSize(v.toByteArray)
  private def sSize(v: String): Int          = CodedOutputStream.computeStringSizeNoTag(v)

  private def sSize(p: RhoTypeN): Int              = p.serializedSize
  private def sSize(kv: (RhoTypeN, RhoTypeN)): Int = kv._1.serializedSize + kv._2.serializedSize
  private def sSizeInjection(injection: (String, RhoTypeN)): Int =
    sSize(injection._1) + injection._2.serializedSize

  private def sSizeSeq[T](seq: Seq[T], f: T => Int): Int =
    sSize(seq.size) + seq.map(f).sum

  private def sSize(ps: Seq[RhoTypeN]): Int = sSizeSeq[RhoTypeN](ps, sSize)

  private def sSizeStrings(strings: Seq[String]): Int = sSizeSeq[String](strings, sSize)

  private def sSizeKVPairs(strings: Seq[(RhoTypeN, RhoTypeN)]): Int =
    sSizeSeq[(RhoTypeN, RhoTypeN)](strings, sSize)

  private def sSizeInjections(injections: Seq[(String, RhoTypeN)]): Int =
    sSizeSeq[(String, RhoTypeN)](injections, sSizeInjection)

  private def sSize(pOpt: Option[RhoTypeN]): Int =
    booleanSize + (if (pOpt.isDefined) pOpt.get.serializedSize else 0)

  private def totalSize(sizes: Int*): Int = tagSize + sizes.sum

  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def serializedSizeFn(p: RhoTypeN): Int = p match {

    /** Basic types */
    case _: NilN => totalSize()

    case pProc: ParProcN =>
      val psSize = sSize(pProc.ps)
      totalSize(psSize)

    case send: SendN =>
      totalSize(sSize(send.chan), sSize(send.data), sSize(send.persistent))

    case receive: ReceiveN =>
      val bindsSize      = sSize(receive.binds)
      val bodySize       = sSize(receive.body)
      val persistentSize = sSize(receive.persistent)
      val peekSize       = sSize(receive.peek)
      val bindCountSize  = sSize(receive.bindCount)
      totalSize(bindsSize, bodySize, persistentSize, peekSize, bindCountSize)

    case m: MatchN =>
      val targetSize = sSize(m.target)
      val casesSize  = sSize(m.cases)
      totalSize(targetSize, casesSize)

    case n: NewN =>
      val bindCountSize  = sSize(n.bindCount)
      val pSize          = sSize(n.p)
      val uriSize        = sSizeStrings(n.uri)
      val injectionsSize = sSizeInjections(n.injections.toSeq)
      totalSize(bindCountSize, pSize, uriSize, injectionsSize)

    /** Ground types */
    case gBool: GBoolN           => totalSize(sSize(gBool.v))
    case gInt: GIntN             => totalSize(sSize(gInt.v))
    case gBigInt: GBigIntN       => totalSize(sSize(gBigInt.v))
    case gString: GStringN       => totalSize(sSize(gString.v))
    case gByteArray: GByteArrayN => totalSize(sSize(gByteArray.v))
    case gUri: GUriN             => totalSize(sSize(gUri.v))

    /** Collections */
    case list: EListN    => totalSize(sSize(list.ps), sSize(list.remainder))
    case eTuple: ETupleN => totalSize(sSize(eTuple.ps))
    case eSet: ESetN     => totalSize(sSize(eSet.sortedPs), sSize(eSet.remainder))
    case eMap: EMapN     => totalSize(sSizeKVPairs(eMap.sortedPs), sSize(eMap.remainder))

    /** Vars */
    case v: BoundVarN      => totalSize(sSize(v.idx))
    case v: FreeVarN       => totalSize(sSize(v.idx))
    case _: WildcardN.type => totalSize()

    /** Operations */
    case op: Operation1ParN => totalSize(sSize(op.p))
    case op: Operation2ParN => totalSize(sSize(op.p1), sSize(op.p2))
    case eMethod: EMethodN =>
      val methodNameSize = sSize(eMethod.methodName)
      val targetSize     = sSize(eMethod.target)
      val argumentsSize  = sSize(eMethod.arguments)
      totalSize(methodNameSize, targetSize, argumentsSize)
    case eMatches: EMatchesN => totalSize(sSize(eMatches.target), sSize(eMatches.pattern))

    /** Unforgeable names */
    case unf: UnforgeableN => totalSize(sSize(unf.v))

    /** Connective */
    case _: ConnectiveSTypeN => totalSize()

    case connNot: ConnNotN => totalSize(sSize(connNot.p))
    case connAnd: ConnAndN => totalSize(sSize(connAnd.ps))
    case connOr: ConnOrN   => totalSize(sSize(connOr.ps))

    case connVarRef: ConnVarRefN => totalSize(sSize(connVarRef.index), sSize(connVarRef.depth))

    /** Auxiliary types */
    case bind: ReceiveBindN =>
      val patternsSize  = sSize(bind.patterns)
      val sourceSize    = sSize(bind.source)
      val reminderSize  = sSize(bind.remainder)
      val freeCountSize = sSize(bind.freeCount)
      totalSize(patternsSize, sourceSize, reminderSize, freeCountSize)

    case mCase: MatchCaseN =>
      val patternSize   = sSize(mCase.pattern)
      val sourceSize    = sSize(mCase.source)
      val freeCountSize = sSize(mCase.freeCount)
      totalSize(patternSize, sourceSize, freeCountSize)

    /** Other types */
    case bundle: BundleN =>
      val bodySize      = sSize(bundle.body)
      val writeFlagSize = sSize(bundle.writeFlag)
      val readFlagSize  = sSize(bundle.readFlag)
      totalSize(bodySize, writeFlagSize, readFlagSize)

    case _ => throw new Exception("Not defined type")
  }
}
