package coop.rchain.models.rholangN.ParManager

import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import coop.rchain.models.rholangN.ParManager.Constants._
import coop.rchain.models.rholangN.ParManager.Sorting._
import coop.rchain.models.rholangN._

import java.io.{InputStream, OutputStream}

private[ParManager] object Codecs {
  def serialize(par: ParN, output: OutputStream): Unit = {
    val cos = CodedOutputStream.newInstance(output)

    object Serializer {
      private def write(x: Byte): Unit    = cos.writeRawByte(x)
      private def write(x: Boolean): Unit = cos.writeBoolNoTag(x)
      private def write(x: Int): Unit     = cos.writeInt32NoTag(x)
      private def write(x: Long): Unit    = cos.writeInt64NoTag(x)
      private def write(x: String): Unit  = cos.writeStringNoTag(x)

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

      def write(p: RhoTypeN): Unit = p match {

        /** Main types */
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
        case _: GNilN =>
          write(GNIL)

        case gInt: GIntN =>
          write(GINT)
          write(gInt.v)

        /** Collections */
        case eList: EListN =>
          write(ELIST)
          write(eList.ps)
          write(eList.remainder)

        /** Vars */
        case bVar: BoundVarN =>
          write(BOUND_VAR)
          write(bVar.value)

        case fVar: FreeVarN =>
          write(FREE_VAR)
          write(fVar.value)

        case _: WildcardN =>
          write(WILDCARD)

        /** Expr */
        /** Bundle */
        /** Connective */
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

        case _ => assert(assertion = false, "Not defined type")
      }
    }

    Serializer.write(par)
    cos.flush()
  }

  def deserialize(input: InputStream): ParN = {
    val cis = CodedInputStream.newInstance(input)

    def readTag(): Byte = cis.readRawByte()

    def readBool(): Boolean = cis.readBool()

    def readInt(): Int = cis.readInt32()

    def readLength(): Int = cis.readUInt32()

    def readLong(): Long = cis.readInt64()

    def readString(): String = cis.readString()

    def readVar(): VarN = readPar() match {
      case v: VarN => v
      case _ =>
        assert(assertion = false, "Value must be Var")
        WildcardN()
    }

    def readVarOpt(): Option[VarN] = if (readBool()) Some(readVar()) else None

    def readSeq[T](f: () => T): Seq[T] = {
      val count = readLength()
      (1 to count).map(_ => f())
    }

    def readStrings(): Seq[String] = readSeq(readString _)

    def readPars(): Seq[ParN] = readSeq(readPar _)

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
          ReceiveBindN(Seq(), GNilN(), None, 0)
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
          MatchCaseN(GNilN(), GNilN(), 0)
      }
      def readMatchCase() = readTagAndMatch(matchMCase)
      readSeq(readMatchCase _)
    }

    def matchPar(tag: Byte): ParN = tag match {

      /** Main types */
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
      case GNIL =>
        GNilN()

      case GINT =>
        val v = readLong()
        GIntN(v)

      /** Collections */
      case ELIST =>
        val ps        = readPars()
        val remainder = readVarOpt()
        EListN(ps, remainder)

      /** Vars */
      case BOUND_VAR =>
        val v = readInt()
        BoundVarN(v)

      case FREE_VAR =>
        val v = readInt()
        FreeVarN(v)

      case WILDCARD =>
        WildcardN()

      /** Expr */
      /** Bundle */
      /** Connective */
      case _ =>
        assert(assertion = false, "Invalid tag for ParN deserialization")
        GNilN()
    }

    def readTagAndMatch[T](f: Byte => T): T = f(readTag())
    def readPar(): ParN                     = readTagAndMatch(matchPar)

    readPar()
  }
}
