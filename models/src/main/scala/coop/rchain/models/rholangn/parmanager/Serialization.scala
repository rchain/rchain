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
      // Terminal expressions
      private def write(x: Array[Byte]): Eval[Unit] = Eval.now(cos.writeByteArrayNoTag(x))
      private def write(x: Byte): Eval[Unit]        = Eval.now(cos.writeRawByte(x))
      private def write(x: Boolean): Eval[Unit]     = Eval.now(cos.writeBoolNoTag(x))
      private def write(x: Int): Eval[Unit]         = Eval.now(cos.writeInt32NoTag(x))
      private def write(x: Long): Eval[Unit]        = Eval.now(cos.writeInt64NoTag(x))
      private def write(x: String): Eval[Unit]      = Eval.now(cos.writeStringNoTag(x))
      private def write(x: BigInt): Eval[Unit]      = write(x.toByteArray)

      // Recursive traversal of children elements, defer to prevent stackoverflow (force heap objects)
      private def writeSeq[T](seq: Seq[T], f: T => Eval[Unit]): Eval[Unit] =
        write(seq.size) *> Eval.defer(seq.traverse_(f))

      private def write(pOpt: Option[RhoTypeN]): Eval[Unit] =
        pOpt.map(write(true) *> write(_)).getOrElse(write(false))

      private def write(kv: (ParN, ParN)): Eval[Unit] =
        write(kv._1) *> write(kv._2)

      private def writeInjection(injection: (String, ParN)): Eval[Unit] =
        write(injection._1) *> write(injection._2)

      private def write(ps: Seq[RhoTypeN]): Eval[Unit]           = writeSeq[RhoTypeN](ps, write)
      private def writeStrings(strings: Seq[String]): Eval[Unit] = writeSeq[String](strings, write)
      private def writeKVPairs(kVPairs: Seq[(ParN, ParN)]): Eval[Unit] =
        writeSeq[(ParN, ParN)](kVPairs, write)
      private def writeInjections(injections: Seq[(String, ParN)]): Eval[Unit] =
        writeSeq(injections, writeInjection)

      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def write(p: RhoTypeN): Eval[Unit] = {
        p match {

          /** Basic types */
          case _: NilN.type => write(NIL)

          case pProc: ParProcN =>
            write(PARPROC) *> write(pProc.sortedPs)

          case send: SendN =>
            write(SEND) *>
              write(send.chan) *>
              write(send.data) *>
              write(send.persistent)

          case receive: ReceiveN =>
            write(RECEIVE) *>
              write(receive.sortedBinds) *>
              write(receive.body) *>
              write(receive.persistent) *>
              write(receive.peek) *>
              write(receive.bindCount)

          case m: MatchN =>
            write(MATCH) *>
              write(m.target) *>
              write(m.cases)

          case n: NewN =>
            write(NEW) *>
              write(n.bindCount) *>
              write(n.p) *>
              writeStrings(n.sortedUri) *>
              writeInjections(n.sortedInjections)

          /** Ground types */
          case gBool: GBoolN =>
            write(GBOOL) *> write(gBool.v)

          case gInt: GIntN =>
            write(GINT) *> write(gInt.v)

          case gBigInt: GBigIntN =>
            write(GBIG_INT) *> write(gBigInt.v)

          case gString: GStringN =>
            write(GSTRING) *> write(gString.v)

          case gByteArray: GByteArrayN =>
            write(GBYTE_ARRAY) *> write(gByteArray.v)

          case gUri: GUriN =>
            write(GURI) *> write(gUri.v)

          /** Collections */
          case eList: EListN =>
            write(ELIST) *> write(eList.ps) *> write(eList.remainder)

          case eTuple: ETupleN =>
            write(ETUPLE) *> write(eTuple.ps)

          case eSet: ESetN =>
            write(ESET) *> write(eSet.sortedPs) *> write(eSet.remainder)

          case eMap: EMapN =>
            write(EMAP) *>
              writeKVPairs(eMap.sortedPs) *>
              write(eMap.remainder)

          /** Vars */
          case bVar: BoundVarN =>
            write(BOUND_VAR) *> write(bVar.idx)

          case fVar: FreeVarN =>
            write(FREE_VAR) *> write(fVar.idx)

          case _: WildcardN.type =>
            write(WILDCARD)

          /** Operations */
          case op: Operation1ParN =>
            val tag = op match {
              case _: ENegN => ENEG
              case _: ENotN => ENOT
            }
            write(tag) *> write(op.p)

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
            write(tag) *> write(op.p1) *> write(op.p2)

          case eMethod: EMethodN =>
            write(EMETHOD) *>
              write(eMethod.methodName) *>
              write(eMethod.target) *>
              write(eMethod.arguments)

          case eMatches: EMatchesN =>
            write(EMATCHES) *> write(eMatches.target) *> write(eMatches.pattern)

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
            write(CONNECTIVE_NOT) *> write(connNot.p)

          case connAnd: ConnAndN =>
            write(CONNECTIVE_AND) *> write(connAnd.ps)

          case connOr: ConnOrN =>
            write(CONNECTIVE_OR) *> write(connOr.ps)

          case connVarRef: ConnVarRefN =>
            write(CONNECTIVE_VARREF) *> write(connVarRef.index) *> write(connVarRef.depth)

          /** Auxiliary types */
          case bind: ReceiveBindN =>
            write(RECEIVE_BIND) *>
              write(bind.patterns) *>
              write(bind.source) *>
              write(bind.remainder) *>
              write(bind.freeCount)

          case mCase: MatchCaseN =>
            write(MATCH_CASE) *>
              write(mCase.pattern) *>
              write(mCase.source) *>
              write(mCase.freeCount)

          /** Other types */
          case bundle: BundleN =>
            write(BUNDLE) *>
              write(bundle.body) *>
              write(bundle.writeFlag) *>
              write(bundle.readFlag)

          case _ => throw new Exception("Not defined type")
        }
      }
    }

    Serializer.write(par) <* Eval.now(cos.flush())
  }

  def deserialize(input: InputStream): Eval[ParN] = {
    val cis = CodedInputStream.newInstance(input)

    // Terminal expressions
    def readBytes: Eval[Array[Byte]] = Eval.now(cis.readByteArray())
    def readTag: Eval[Byte]          = Eval.now(cis.readRawByte)
    def readBool: Eval[Boolean]      = Eval.now(cis.readBool)
    def readInt: Eval[Int]           = Eval.now(cis.readInt32)
    def readLong: Eval[Long]         = Eval.now(cis.readInt64)
    def readString: Eval[String]     = Eval.now(cis.readString)
    def readBigInt: Eval[BigInt]     = readBytes.map(BigInt(_))

    // Read a sequence, flatMap prevents stackoverflow (force heap objects)
    def readSeq[T](f: () => Eval[T]): Eval[Seq[T]] =
      readLength.flatMap(count => Seq.range(1, count).map(_ => f()).sequence)

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def readVar: Eval[VarN] =
      readPar.map {
        case v: VarN => v
        case _       => throw new Exception("Value must be Var")
      }

    def readVarOpt: Eval[Option[VarN]] =
      readBool.flatMap(x => if (x) readVar.map(Some(_)) else Eval.now(none))

    def readKVPair: Eval[(ParN, ParN)]      = (readPar, readPar).mapN((_, _))
    def readInjection: Eval[(String, ParN)] = (readString, readPar).mapN((_, _))

    def readLength: Eval[Int] = Eval.later(cis.readUInt32())

    def readStrings: Eval[Seq[String]]            = readSeq(() => readString)
    def readPars: Eval[Seq[ParN]]                 = readSeq(() => readPar)
    def readKVPairs: Eval[Seq[(ParN, ParN)]]      = readSeq(() => readKVPair)
    def readInjections: Eval[Seq[(String, ParN)]] = readSeq(() => readInjection)

    /** Auxiliary types deserialization */
    def readReceiveBinds: Eval[Seq[ReceiveBindN]] = {
      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def matchReceiveBind(tag: Byte): Eval[ReceiveBindN] = tag match {
        case RECEIVE_BIND =>
          for {
            patterns  <- readPars
            source    <- readPar
            remainder <- readVarOpt
            freeCount <- readInt
          } yield ReceiveBindN(patterns, source, remainder, freeCount)
        case _ => throw new Exception("Invalid tag for ReceiveBindN deserialization")
      }
      def readReceiveBind = readTagAndMatch(matchReceiveBind)
      readSeq(() => readReceiveBind)
    }

    def readMatchCases: Eval[Seq[MatchCaseN]] = {
      @SuppressWarnings(Array("org.wartremover.warts.Throw"))
      def matchMCase(tag: Byte): Eval[MatchCaseN] = tag match {
        case MATCH_CASE =>
          for {
            pattern   <- readPar
            source    <- readPar
            freeCount <- readInt
          } yield MatchCaseN(pattern, source, freeCount)
        case _ => throw new Exception("Invalid tag for matchMCase deserialization")
      }
      def readMatchCase = readTagAndMatch(matchMCase)
      readSeq(() => readMatchCase)
    }

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def matchPar(tag: Byte): Eval[ParN] = tag match {

      /** Basic types */
      case PARPROC =>
        readPars.map(ParProcN(_))

      case SEND =>
        for {
          chan       <- readPar
          dataSeq    <- readPars
          persistent <- readBool
        } yield SendN(chan, dataSeq, persistent)

      case RECEIVE =>
        for {
          binds      <- readReceiveBinds
          body       <- readPar
          persistent <- readBool
          peek       <- readBool
          bindCount  <- readInt
        } yield ReceiveN(binds, body, persistent, peek, bindCount)

      case MATCH =>
        for {
          target <- readPar
          cases  <- readMatchCases
        } yield MatchN(target, cases)

      case NEW =>
        for {
          bindCount  <- readInt
          p          <- readPar
          uri        <- readStrings
          injections <- readInjections
        } yield NewN(bindCount, p, uri, injections)

      /** Ground types */
      case NIL => Eval.now(NilN)

      case GBOOL =>
        readBool.map(GBoolN(_))

      case GINT =>
        readLong.map(GIntN(_))

      case GBIG_INT =>
        readBigInt.map(GBigIntN(_))

      case GSTRING =>
        readString.map(GStringN(_))

      case GBYTE_ARRAY =>
        readBytes.map(GByteArrayN(_))

      case GURI =>
        readString.map(GUriN(_))

      /** Collections */
      case ELIST =>
        for {
          ps        <- readPars
          remainder <- readVarOpt
        } yield EListN(ps, remainder)

      case ETUPLE =>
        readPars.map(ETupleN(_))

      case ESET =>
        for {
          ps        <- readPars
          remainder <- readVarOpt
        } yield ESetN(ps, remainder)

      case EMAP =>
        for {
          ps        <- readKVPairs
          remainder <- readVarOpt
        } yield EMapN(ps, remainder)

      /** Vars */
      case BOUND_VAR =>
        readInt.map(BoundVarN(_))

      case FREE_VAR =>
        readInt.map(FreeVarN(_))

      case WILDCARD => Eval.now(WildcardN)

      /** Unforgeable names */
      case UPRIVATE =>
        readBytes.map(UPrivateN(_))

      case UDEPLOY_ID =>
        readBytes.map(UDeployIdN(_))

      case UDEPLOYER_ID =>
        readBytes.map(UDeployerIdN(_))

      // TODO: Temporary solution for easier conversion from old types - change type in the future
      case SYS_AUTH_TOKEN =>
        readBytes.as(USysAuthTokenN())

      /** Operations */
      case ENEG =>
        readPar.map(ENegN(_))

      case ENOT =>
        readPar.map(ENotN(_))

      case EPLUS =>
        (readPar, readPar).mapN(EPlusN(_, _))

      case EMINUS =>
        (readPar, readPar).mapN(EMinusN(_, _))

      case EMULT =>
        (readPar, readPar).mapN(EMultN(_, _))

      case EDIV =>
        (readPar, readPar).mapN(EDivN(_, _))

      case EMOD =>
        (readPar, readPar).mapN(EModN(_, _))

      case ELT =>
        (readPar, readPar).mapN(ELtN(_, _))

      case ELTE =>
        (readPar, readPar).mapN(ELteN(_, _))

      case EGT =>
        (readPar, readPar).mapN(EGtN(_, _))

      case EGTE =>
        (readPar, readPar).mapN(EGteN(_, _))

      case EEQ =>
        (readPar, readPar).mapN(EEqN(_, _))

      case ENEQ =>
        (readPar, readPar).mapN(ENeqN(_, _))

      case EAND =>
        (readPar, readPar).mapN(EAndN(_, _))

      case ESHORTAND =>
        (readPar, readPar).mapN(EShortAndN(_, _))

      case EOR =>
        (readPar, readPar).mapN(EOrN(_, _))

      case ESHORTOR =>
        (readPar, readPar).mapN(EShortOrN(_, _))

      case EPLUSPLUS =>
        (readPar, readPar).mapN(EPlusPlusN(_, _))

      case EMINUSMINUS =>
        (readPar, readPar).mapN(EMinusMinusN(_, _))

      case EPERCENT =>
        (readPar, readPar).mapN(EPercentPercentN(_, _))

      case EMETHOD =>
        (readString, readPar, readPars).mapN(EMethodN(_, _, _))

      case EMATCHES =>
        (readPar, readPar).mapN(EMatchesN(_, _))

      /** Connective */
      case CONNECTIVE_BOOL      => Eval.now(ConnBoolN)
      case CONNECTIVE_INT       => Eval.now(ConnIntN)
      case CONNECTIVE_BIG_INT   => Eval.now(ConnBigIntN)
      case CONNECTIVE_STRING    => Eval.now(ConnStringN)
      case CONNECTIVE_URI       => Eval.now(ConnUriN)
      case CONNECTIVE_BYTEARRAY => Eval.now(ConnByteArrayN)

      case CONNECTIVE_NOT =>
        readPar.map(ConnNotN(_))

      case CONNECTIVE_AND =>
        readPars.map(ConnAndN(_))

      case CONNECTIVE_OR =>
        readPars.map(ConnOrN(_))

      case CONNECTIVE_VARREF =>
        (readInt, readInt).mapN(ConnVarRefN(_, _))

      /** Other types */
      case BUNDLE =>
        (readPar, readBool, readBool).mapN(BundleN(_, _, _))

      case _ => throw new Exception("Invalid tag for ParN deserialization")
    }

    def readTagAndMatch[T](f: Byte => Eval[T]): Eval[T] = readTag.flatMap(f)
    def readPar: Eval[ParN]                             = readTagAndMatch(matchPar)

    readPar
  }
}
