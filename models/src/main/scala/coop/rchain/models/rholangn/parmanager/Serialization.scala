package coop.rchain.models.rholangn.parmanager

import cats.Eval
import cats.syntax.all._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import coop.rchain.models.rholangn._
import coop.rchain.models.rholangn.parmanager.Constants._

import java.io.{InputStream, OutputStream}

private[parmanager] object Serialization {
  def serialize(par: ParN, output: OutputStream): Eval[Unit] = {
    val cos = CodedOutputStream.newInstance(output)

    object Serializer {
      private def write(x: Array[Byte]): Eval[Unit] = Eval.later(cos.writeByteArrayNoTag(x))

      private def write(x: Byte): Eval[Unit]    = Eval.later(cos.writeRawByte(x))
      private def write(x: Boolean): Eval[Unit] = Eval.later(cos.writeBoolNoTag(x))
      private def write(x: Int): Eval[Unit]     = Eval.later(cos.writeInt32NoTag(x))
      private def write(x: BigInt): Eval[Unit]  = Eval.defer(write(x.toByteArray))
      private def write(x: Long): Eval[Unit]    = Eval.later(cos.writeInt64NoTag(x))
      private def write(x: String): Eval[Unit]  = Eval.later(cos.writeStringNoTag(x))

      private def write(pOpt: Option[RhoTypeN]): Eval[Unit] =
        pOpt.map(write(true) *> write(_)).getOrElse(write(false))

      private def write(kv: (ParN, ParN)): Eval[Unit] =
        write(kv._1) *> write(kv._2)

      private def writeInjection(injection: (String, ParN)): Eval[Unit] =
        write(injection._1) *> write(injection._2)

      private def writeSeq[T](seq: Seq[T], f: T => Eval[Unit]): Eval[Unit] =
        write(seq.size) <* seq.traverse(f)

      private def write(ps: Seq[RhoTypeN]): Eval[Unit]           = writeSeq[RhoTypeN](ps, write)
      private def writeStrings(strings: Seq[String]): Eval[Unit] = writeSeq[String](strings, write)
      private def writeKVPairs(kVPairs: Seq[(ParN, ParN)]): Eval[Unit] =
        writeSeq[(ParN, ParN)](kVPairs, write)
      private def writeInjections(injections: Seq[(String, ParN)]): Eval[Unit] =
        writeSeq[(String, ParN)](injections, writeInjection)

      private def write1ParOp(tag: Byte, p: ParN): Eval[Unit] =
        write(tag) *> write(p)

      private def write2ParOp(tag: Byte, p1: ParN, p2: ParN): Eval[Unit] =
        write(tag) *> write(p1) *> write(p2)

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def write(p: RhoTypeN): Eval[Unit] = {
        p match {

          /** Basic types */
          case _: NilN.type => write(NIL)

          case pProc: ParProcN =>
            write(PARPROC) *> Eval.defer(write(pProc.sortedPs))

          case send: SendN =>
            write(SEND) *>
              Eval.defer(write(send.chan)) *>
              Eval.defer(write(send.data)) *>
              Eval.defer(write(send.persistent))

          case receive: ReceiveN =>
            write(RECEIVE) *>
              Eval.defer(write(receive.sortedBinds)) *>
              Eval.defer(write(receive.body)) *>
              Eval.defer(write(receive.persistent)) *>
              Eval.defer(write(receive.peek)) *>
              Eval.defer(write(receive.bindCount))

          case m: MatchN =>
            write(MATCH) *>
              Eval.defer(write(m.target)) *>
              Eval.defer(write(m.cases))

          case n: NewN =>
            write(NEW) *>
              write(n.bindCount) *>
              Eval.defer(write(n.p)) *>
              Eval.defer(writeStrings(n.sortedUri)) *>
              Eval.defer(writeInjections(n.sortedInjections))

          /** Ground types */
          case gBool: GBoolN =>
            write(GBOOL) *> Eval.defer(write(gBool.v))

          case gInt: GIntN =>
            write(GINT) *> Eval.defer(write(gInt.v))

          case gBigInt: GBigIntN =>
            write(GBIG_INT) *> Eval.defer(write(gBigInt.v))

          case gString: GStringN =>
            write(GSTRING) *> Eval.defer(write(gString.v))

          case gByteArray: GByteArrayN =>
            write(GBYTE_ARRAY) *> Eval.defer(write(gByteArray.v))

          case gUri: GUriN =>
            write(GURI) *> Eval.defer(write(gUri.v))

          /** Collections */
          case eList: EListN =>
            write(ELIST) *> Eval.defer(write(eList.ps)) *> Eval.defer(write(eList.remainder))

          case eTuple: ETupleN =>
            write(ETUPLE) *> Eval.defer(write(eTuple.ps))

          case eSet: ESetN =>
            write(ESET) *> Eval.defer(write(eSet.sortedPs)) *> Eval.defer(write(eSet.remainder))

          case eMap: EMapN =>
            write(EMAP) *>
              Eval.defer(writeKVPairs(eMap.sortedPs)) *>
              Eval.defer(write(eMap.remainder))

          /** Vars */
          case bVar: BoundVarN =>
            write(BOUND_VAR) *> Eval.defer(write(bVar.idx))

          case fVar: FreeVarN =>
            write(FREE_VAR) *> Eval.defer(write(fVar.idx))

          case _: WildcardN.type =>
            Eval.defer(write(WILDCARD))

          /** Operations */
          case op: Operation1ParN =>
            val tag = op match {
              case _: ENegN => ENEG
              case _: ENotN => ENOT
            }
            Eval.defer(write1ParOp(tag, op.p))

          case op: Operation2ParN =>
            val tag = op match {
              case _: EPlusN           => EPLUS
              case _: EMinusN          => EMINUS
              case _: EMultN           => EMULT
              case _: EDivN            => EDIV
              case _: EModN            => EMOD
              case _: ELtN             => ELT
              case _: ELteN            => ELTE
              case _: EGtN             => EGT
              case _: EGteN            => EGTE
              case _: EEqN             => EEQ
              case _: ENeqN            => ENEQ
              case _: EAndN            => EAND
              case _: EShortAndN       => ESHORTAND
              case _: EOrN             => EOR
              case _: EShortOrN        => ESHORTOR
              case _: EPlusPlusN       => EPLUSPLUS
              case _: EMinusMinusN     => EMINUSMINUS
              case _: EPercentPercentN => EPERCENT
            }
            Eval.defer(write2ParOp(tag, op.p1, op.p2))

          case eMethod: EMethodN =>
            write(EMETHOD) *>
              write(eMethod.methodName) *>
              Eval.defer(write(eMethod.target)) *>
              Eval.defer(write(eMethod.arguments))

          case eMatches: EMatchesN =>
            Eval.defer(write2ParOp(EMATCHES, eMatches.target, eMatches.pattern))

          /** Unforgeable names */
          case unf: UnforgeableN =>
            val writeUnfKind = unf match {
              case _: UPrivateN      => write(UPRIVATE)
              case _: UDeployIdN     => write(UDEPLOY_ID)
              case _: UDeployerIdN   => write(UDEPLOYER_ID)
              case _: USysAuthTokenN => write(SYS_AUTH_TOKEN)
            }
            writeUnfKind *> write(unf.v)

          /** Connective */
          case _: ConnBoolN.type      => write(CONNECTIVE_BOOL)
          case _: ConnIntN.type       => write(CONNECTIVE_INT)
          case _: ConnBigIntN.type    => write(CONNECTIVE_BIG_INT)
          case _: ConnStringN.type    => write(CONNECTIVE_STRING)
          case _: ConnUriN.type       => write(CONNECTIVE_URI)
          case _: ConnByteArrayN.type => write(CONNECTIVE_BYTEARRAY)

          case connNot: ConnNotN =>
            write(CONNECTIVE_NOT) *> Eval.defer(write(connNot.p))

          case connAnd: ConnAndN =>
            write(CONNECTIVE_AND) *> Eval.defer(write(connAnd.ps))

          case connOr: ConnOrN =>
            write(CONNECTIVE_OR) *> Eval.defer(write(connOr.ps))

          case connVarRef: ConnVarRefN =>
            write(CONNECTIVE_VARREF) *> write(connVarRef.index) *> write(connVarRef.depth)

          /** Auxiliary types */
          case bind: ReceiveBindN =>
            write(RECEIVE_BIND) *>
              Eval.defer(write(bind.patterns)) *>
              Eval.defer(write(bind.source)) *>
              Eval.defer(write(bind.remainder)) *>
              Eval.defer(write(bind.freeCount))

          case mCase: MatchCaseN =>
            write(MATCH_CASE) *>
              Eval.defer(write(mCase.pattern)) *>
              Eval.defer(write(mCase.source)) *>
              Eval.defer(write(mCase.freeCount))

          /** Other types */
          case bundle: BundleN =>
            write(BUNDLE) *>
              Eval.defer(write(bundle.body)) *>
              Eval.defer(write(bundle.writeFlag)) *>
              Eval.defer(write(bundle.readFlag))

          case _ => throw new Exception("Not defined type")
        }
      }
    }

    Serializer.write(par) <* Eval.now(cos.flush())
  }

  def deserialize(input: InputStream): ParN = {
    val cis = CodedInputStream.newInstance(input)

    def readBytes(): Array[Byte] = cis.readByteArray()

    def readTag(): Byte      = cis.readRawByte()
    def readBool(): Boolean  = cis.readBool()
    def readInt(): Int       = cis.readInt32()
    def readBigInt(): BigInt = BigInt(readBytes())
    def readLong(): Long     = cis.readInt64()
    def readString(): String = cis.readString()

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def readVar(): VarN = readPar() match {
      case v: VarN => v
      case _       => throw new Exception("Value must be Var")
    }

    def readVarOpt(): Option[VarN]      = if (readBool()) Some(readVar()) else None
    def readKVPair(): (ParN, ParN)      = (readPar(), readPar())
    def readInjection(): (String, ParN) = (readString(), readPar())

    def readLength(): Int = cis.readUInt32()
    def readSeq[T](f: () => T): Seq[T] = {
      val count = readLength()
      (1 to count).map(_ => f())
    }

    def readStrings(): Seq[String]            = readSeq(readString _)
    def readPars(): Seq[ParN]                 = readSeq(readPar _)
    def readKVPairs(): Seq[(ParN, ParN)]      = readSeq(readKVPair _)
    def readInjections(): Seq[(String, ParN)] = readSeq(readInjection _)

    /** Auxiliary types deserialization */
    def readReceiveBinds(): Seq[ReceiveBindN] = {
      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def matchReceiveBind(tag: Byte): ReceiveBindN = tag match {
        case RECEIVE_BIND =>
          val patterns  = readPars()
          val source    = readPar()
          val remainder = readVarOpt()
          val freeCount = readInt()
          ReceiveBindN(patterns, source, remainder, freeCount)
        case _ => throw new Exception("Invalid tag for ReceiveBindN deserialization")
      }
      def readReceiveBind() = readTagAndMatch(matchReceiveBind)
      readSeq(readReceiveBind _)
    }

    def readMatchCases(): Seq[MatchCaseN] = {
      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def matchMCase(tag: Byte): MatchCaseN = tag match {
        case MATCH_CASE =>
          val pattern   = readPar()
          val source    = readPar()
          val freeCount = readInt()
          MatchCaseN(pattern, source, freeCount)
        case _ => throw new Exception("Invalid tag for matchMCase deserialization")
      }
      def readMatchCase() = readTagAndMatch(matchMCase)
      readSeq(readMatchCase _)
    }

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def matchPar(tag: Byte): ParN = tag match {

      /** Basic types */
      case PARPROC =>
        val ps = readPars()
        ParProcN(ps)

      case SEND =>
        val chan       = readPar()
        val dataSeq    = readPars()
        val persistent = readBool()
        SendN(chan, dataSeq, persistent)

      case RECEIVE =>
        val binds      = readReceiveBinds()
        val body       = readPar()
        val persistent = readBool()
        val peek       = readBool()
        val bindCount  = readInt()
        ReceiveN(binds, body, persistent, peek, bindCount)

      case MATCH =>
        val target = readPar()
        val cases  = readMatchCases()
        MatchN(target, cases)

      case NEW =>
        val bindCount  = readInt()
        val p          = readPar()
        val uri        = readStrings()
        val injections = readInjections()
        NewN(bindCount, p, uri, injections)

      /** Ground types */
      case NIL => NilN

      case GBOOL =>
        val v = readBool()
        GBoolN(v)

      case GINT =>
        val v = readLong()
        GIntN(v)

      case GBIG_INT =>
        val v = readBigInt()
        GBigIntN(v)

      case GSTRING =>
        val v = readString()
        GStringN(v)

      case GBYTE_ARRAY =>
        val v = readBytes()
        GByteArrayN(v)

      case GURI =>
        val v = readString()
        GUriN(v)

      /** Collections */
      case ELIST =>
        val ps        = readPars()
        val remainder = readVarOpt()
        EListN(ps, remainder)

      case ETUPLE =>
        val ps = readPars()
        ETupleN(ps)

      case ESET =>
        val ps        = readPars()
        val remainder = readVarOpt()
        ESetN(ps, remainder)

      case EMAP =>
        val ps        = readKVPairs()
        val remainder = readVarOpt()
        EMapN(ps, remainder)

      /** Vars */
      case BOUND_VAR =>
        val v = readInt()
        BoundVarN(v)

      case FREE_VAR =>
        val v = readInt()
        FreeVarN(v)

      case WILDCARD => WildcardN

      /** Unforgeable names */
      case UPRIVATE =>
        val v = readBytes()
        UPrivateN(v)

      case UDEPLOY_ID =>
        val v = readBytes()
        UDeployIdN(v)

      case UDEPLOYER_ID =>
        val v = readBytes()
        UDeployerIdN(v)

      case SYS_AUTH_TOKEN =>
        val _ = readBytes() // TODO: Temporary solution for easier conversion from old types - change type in the future
        USysAuthTokenN()

      /** Operations */
      case ENEG =>
        val p = readPar()
        ENegN(p)

      case ENOT =>
        val p = readPar()
        ENotN(p)

      case EPLUS =>
        val p1 = readPar()
        val p2 = readPar()
        EPlusN(p1, p2)

      case EMINUS =>
        val p1 = readPar()
        val p2 = readPar()
        EMinusN(p1, p2)

      case EMULT =>
        val p1 = readPar()
        val p2 = readPar()
        EMultN(p1, p2)

      case EDIV =>
        val p1 = readPar()
        val p2 = readPar()
        EDivN(p1, p2)

      case EMOD =>
        val p1 = readPar()
        val p2 = readPar()
        EModN(p1, p2)

      case ELT =>
        val p1 = readPar()
        val p2 = readPar()
        ELtN(p1, p2)

      case ELTE =>
        val p1 = readPar()
        val p2 = readPar()
        ELteN(p1, p2)

      case EGT =>
        val p1 = readPar()
        val p2 = readPar()
        EGtN(p1, p2)

      case EGTE =>
        val p1 = readPar()
        val p2 = readPar()
        EGteN(p1, p2)

      case EEQ =>
        val p1 = readPar()
        val p2 = readPar()
        EEqN(p1, p2)

      case ENEQ =>
        val p1 = readPar()
        val p2 = readPar()
        ENeqN(p1, p2)

      case EAND =>
        val p1 = readPar()
        val p2 = readPar()
        EAndN(p1, p2)

      case ESHORTAND =>
        val p1 = readPar()
        val p2 = readPar()
        EShortAndN(p1, p2)

      case EOR =>
        val p1 = readPar()
        val p2 = readPar()
        EOrN(p1, p2)

      case ESHORTOR =>
        val p1 = readPar()
        val p2 = readPar()
        EShortOrN(p1, p2)

      case EPLUSPLUS =>
        val p1 = readPar()
        val p2 = readPar()
        EPlusPlusN(p1, p2)

      case EMINUSMINUS =>
        val p1 = readPar()
        val p2 = readPar()
        EMinusMinusN(p1, p2)

      case EPERCENT =>
        val p1 = readPar()
        val p2 = readPar()
        EPercentPercentN(p1, p2)

      case EMETHOD =>
        val methodName = readString()
        val target     = readPar()
        val arguments  = readPars()
        EMethodN(methodName, target, arguments)

      case EMATCHES =>
        val target  = readPar()
        val pattern = readPar()
        EMatchesN(target, pattern)

      /** Connective */
      case CONNECTIVE_BOOL      => ConnBoolN
      case CONNECTIVE_INT       => ConnIntN
      case CONNECTIVE_BIG_INT   => ConnBigIntN
      case CONNECTIVE_STRING    => ConnStringN
      case CONNECTIVE_URI       => ConnUriN
      case CONNECTIVE_BYTEARRAY => ConnByteArrayN

      case CONNECTIVE_NOT =>
        val p = readPar()
        ConnNotN(p)

      case CONNECTIVE_AND =>
        val ps = readPars()
        ConnAndN(ps)

      case CONNECTIVE_OR =>
        val ps = readPars()
        ConnOrN(ps)

      case CONNECTIVE_VARREF =>
        val index = readInt()
        val depth = readInt()
        ConnVarRefN(index, depth)

      /** Other types */
      case BUNDLE =>
        val body      = readPar()
        val writeFlag = readBool()
        val readFlag  = readBool()
        BundleN(body, writeFlag, readFlag)

      case _ => throw new Exception("Invalid tag for ParN deserialization")
    }

    def readTagAndMatch[T](f: Byte => T): T = f(readTag())
    def readPar(): ParN                     = readTagAndMatch(matchPar)

    readPar()
  }
}
