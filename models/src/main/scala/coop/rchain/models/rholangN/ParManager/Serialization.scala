package coop.rchain.models.rholangN.ParManager

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import coop.rchain.models.rholangN.ParManager.Constants._
import coop.rchain.models.rholangN.ParManager.Sorting._
import coop.rchain.models.rholangN._
import scodec.bits.ByteVector

import java.io.{InputStream, OutputStream}

private[ParManager] object Serialization {
  def serialize(par: ParN, output: OutputStream): Unit = {
    val cos = CodedOutputStream.newInstance(output)

    object Serializer {
      private def write(x: Array[Byte]): Unit = cos.writeByteArrayNoTag(x)

      private def write(x: Byte): Unit       = cos.writeRawByte(x)
      private def write(x: Boolean): Unit    = cos.writeBoolNoTag(x)
      private def write(x: Int): Unit        = cos.writeInt32NoTag(x)
      private def write(x: BigInt): Unit     = write(x.toByteArray)
      private def write(x: Long): Unit       = cos.writeInt64NoTag(x)
      private def write(x: String): Unit     = cos.writeStringNoTag(x)
      private def write(x: ByteVector): Unit = write(x.toArray)

      private def write(pOpt: Option[RhoTypeN]): Unit =
        if (pOpt.isDefined) {
          write(true)
          write(pOpt.get)
        } else write(false)

      private def writeSeq[T](seq: Seq[T], f: T => Unit): Unit = {
        write(seq.size)
        seq.foreach(f)
      }

      private def write(ps: Seq[RhoTypeN]): Unit           = writeSeq[RhoTypeN](ps, write)
      private def writeStrings(strings: Seq[String]): Unit = writeSeq[String](strings, write)

      private def write1ParOp(tag: Byte, p: ParN): Unit = {
        write(tag)
        write(p)
      }

      private def write2ParOp(tag: Byte, p1: ParN, p2: ParN): Unit = {
        write(tag)
        write(p1)
        write(p2)
      }

      def write(p: RhoTypeN): Unit = p match {

        /** Basic types */
        case _: NilN => write(NIL)

        case pProc: ParProcN =>
          write(PARPROC)
          write(sortPars(pProc.ps))

        case send: SendN =>
          write(SEND)
          write(send.chan)
          write(send.data)
          write(send.persistent)

        case receive: ReceiveN =>
          write(RECEIVE)
          write(receive.binds)
          write(receive.body)
          write(receive.persistent)
          write(receive.peek)
          write(receive.bindCount)

        case m: MatchN =>
          write(MATCH)
          write(m.target)
          write(m.cases)

        case n: NewN =>
          write(NEW)
          write(n.bindCount)
          write(n.p)
          writeStrings(sortStrings(n.uri))

        /** Ground types */
        case gBool: GBoolN =>
          write(GBOOL)
          write(gBool.v)

        case gInt: GIntN =>
          write(GINT)
          write(gInt.v)

        case gBigInt: GBigIntN =>
          write(GBIG_INT)
          write(gBigInt.v)

        case gString: GStringN =>
          write(GSTRING)
          write(gString.v)

        case gByteArray: GByteArrayN =>
          write(GBYTE_ARRAY)
          write(gByteArray.v)

        case gUri: GUriN =>
          write(GURI)
          write(gUri.v)

        /** Collections */
        case eList: EListN =>
          write(ELIST)
          write(eList.ps)
          write(eList.remainder)

        case eTuple: ETupleN =>
          write(ETUPLE)
          write(eTuple.ps)

        /** Vars */
        case bVar: BoundVarN =>
          write(BOUND_VAR)
          write(bVar.idx)

        case fVar: FreeVarN =>
          write(FREE_VAR)
          write(fVar.idx)

        case _: WildcardN =>
          write(WILDCARD)

        /** Unforgeable names */
        case unf: UnforgeableN =>
          unf match {
            case _: UPrivateN    => write(UPRIVATE)
            case _: UDeployIdN   => write(UDEPLOY_ID)
            case _: UDeployerIdN => write(UDEPLOYER_ID)
          }
          write(unf.v)

        /** Operations */
        case op: Operation1ParN =>
          val tag = op match {
            case _: ENegN => ENEG
            case _: ENotN => ENOT
          }
          write1ParOp(tag, op.p)

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
          write2ParOp(tag, op.p1, op.p2)

        case eMethod: EMethodN =>
          write(EMETHOD)
          write(eMethod.methodName)
          write(eMethod.target)
          write(eMethod.arguments)

        case eMatches: EMatchesN =>
          write2ParOp(EMATCHES, eMatches.target, eMatches.pattern)

        /** Connective */
        case _: ConnBoolN      => write(CONNECTIVE_BOOL)
        case _: ConnIntN       => write(CONNECTIVE_INT)
        case _: ConnBigIntN    => write(CONNECTIVE_BIG_INT)
        case _: ConnStringN    => write(CONNECTIVE_STRING)
        case _: ConnUriN       => write(CONNECTIVE_URI)
        case _: ConnByteArrayN => write(CONNECTIVE_BYTEARRAY)

        case connNot: ConnNotN =>
          write(CONNECTIVE_NOT)
          write(connNot.p)

        case connAnd: ConnAndN =>
          write(CONNECTIVE_AND)
          write(connAnd.ps)

        case connOr: ConnOrN =>
          write(CONNECTIVE_OR)
          write(connOr.ps)

        case connVarRef: ConnVarRefN =>
          write(CONNECTIVE_VARREF)
          write(connVarRef.index)
          write(connVarRef.depth)

        /** Auxiliary types */
        case bind: ReceiveBindN =>
          write(RECEIVE_BIND)
          write(bind.patterns)
          write(bind.source)
          write(bind.remainder)
          write(bind.freeCount)

        case mCase: MatchCaseN =>
          write(MATCH_CASE)
          write(mCase.pattern)
          write(mCase.source)
          write(mCase.freeCount)

        /** Other types */
        case bundle: BundleN =>
          write(BUNDLE)
          write(bundle.body)
          write(bundle.writeFlag)
          write(bundle.readFlag)

        case _: SysAuthTokenN =>
          write(SYS_AUTH_TOKEN)

        case _ => assert(assertion = false, "Not defined type")
      }
    }

    Serializer.write(par)
    cos.flush()
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

    def readVar(): VarN = readPar() match {
      case v: VarN => v
      case _ =>
        assert(assertion = false, "Value must be Var")
        WildcardN()
    }

    def readVarOpt(): Option[VarN] = if (readBool()) Some(readVar()) else None

    def readLength(): Int = cis.readUInt32()
    def readSeq[T](f: () => T): Seq[T] = {
      val count = readLength()
      (1 to count).map(_ => f())
    }

    def readStrings(): Seq[String] = readSeq(readString _)
    def readPars(): Seq[ParN]      = readSeq(readPar _)

    /** Auxiliary types deserialization */
    def readReceiveBinds(): Seq[ReceiveBindN] = {
      def matchReceiveBind(tag: Byte): ReceiveBindN = tag match {
        case RECEIVE_BIND =>
          val patterns  = readPars()
          val source    = readPar()
          val remainder = readVarOpt()
          val freeCount = readInt()
          ReceiveBindN(patterns, source, remainder, freeCount)
        case _ =>
          assert(assertion = false, "Invalid tag for ReceiveBindN deserialization")
          ReceiveBindN(Seq(), NilN(), None, 0)
      }
      def readReceiveBind() = readTagAndMatch(matchReceiveBind)
      readSeq(readReceiveBind _)
    }

    def readMatchCases(): Seq[MatchCaseN] = {
      def matchMCase(tag: Byte): MatchCaseN = tag match {
        case MATCH_CASE =>
          val pattern   = readPar()
          val source    = readPar()
          val freeCount = readInt()
          MatchCaseN(pattern, source, freeCount)
        case _ =>
          assert(assertion = false, "Invalid tag for ReceiveBindN deserialization")
          MatchCaseN(NilN(), NilN(), 0)
      }
      def readMatchCase() = readTagAndMatch(matchMCase)
      readSeq(readMatchCase _)
    }

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
        val bindCount = readInt()
        val p         = readPar()
        val uri       = readStrings()
        NewN(bindCount, p, uri)

      /** Ground types */
      case NIL =>
        NilN()

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

      /** Vars */
      case BOUND_VAR =>
        val v = readInt()
        BoundVarN(v)

      case FREE_VAR =>
        val v = readInt()
        FreeVarN(v)

      case WILDCARD =>
        WildcardN()

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
      case CONNECTIVE_BOOL =>
        ConnBoolN()

      case CONNECTIVE_INT =>
        ConnIntN()

      case CONNECTIVE_BIG_INT =>
        ConnBigIntN()

      case CONNECTIVE_STRING =>
        ConnStringN()

      case CONNECTIVE_URI =>
        ConnUriN()

      case CONNECTIVE_BYTEARRAY =>
        ConnByteArrayN()

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

      case SYS_AUTH_TOKEN =>
        SysAuthTokenN()

      case _ =>
        assert(assertion = false, "Invalid tag for ParN deserialization")
        NilN()
    }

    def readTagAndMatch[T](f: Byte => T): T = f(readTag())
    def readPar(): ParN                     = readTagAndMatch(matchPar)

    readPar()
  }
}
