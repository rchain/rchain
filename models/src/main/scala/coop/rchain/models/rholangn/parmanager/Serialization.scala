package coop.rchain.models.rholangn.parmanager

import cats.Eval
import cats.syntax.all._
import com.google.protobuf.{CodedInputStream, CodedOutputStream}
import coop.rchain.models.rholangn._
import coop.rchain.models.rholangn.parmanager.Constants._

import java.io.{ByteArrayInputStream, ByteArrayOutputStream, InputStream, OutputStream}
import scala.util.Using

/** Wrapper for protobuf serialization of primitive types. */
private class ProtobufPrimitiveWriter(output: CodedOutputStream) {
  def write(x: Byte): Eval[Unit]        = Eval.later(output.writeRawByte(x))
  def write(x: Array[Byte]): Eval[Unit] = Eval.later(output.writeRawBytes(x))
  def write(x: Boolean): Eval[Unit]     = Eval.later(output.writeBoolNoTag(x))
  def write(x: Int): Eval[Unit]         = Eval.later(output.writeUInt32NoTag(x))
  def write(x: Long): Eval[Unit]        = Eval.later(output.writeUInt64NoTag(x))
  def write(x: String): Eval[Unit]      = Eval.later(output.writeStringNoTag(x))
}

/** Wrapper for protobuf serialization with recursive function. */
private class ProtobufRecWriter(writer: ProtobufPrimitiveWriter, rec: RhoTypeN => Eval[Unit]) {
  // Terminal expressions
  def write(x: Byte): Eval[Unit]        = writer.write(x)
  def write(x: Array[Byte]): Eval[Unit] = writer.write(x)
  def write(x: Boolean): Eval[Unit]     = writer.write(x)
  def write(x: Int): Eval[Unit]         = writer.write(x)
  def write(x: Long): Eval[Unit]        = writer.write(x)
  def write(x: String): Eval[Unit]      = writer.write(x)
  def write(x: BigInt): Eval[Unit]      = write(x.toByteArray)

  // Recursive traversal
  def write(x: RhoTypeN): Eval[Unit] = rec(x)

  // Recursive traversal of a sequence
  def write(seq: Seq[RhoTypeN]): Eval[Unit] = writeSeq[RhoTypeN](seq, write)

  def write(pOpt: Option[RhoTypeN]): Eval[Unit] =
    pOpt.map(write(true) *> write(_)).getOrElse(write(false))

  def writeTuplePar(kv: (RhoTypeN, RhoTypeN)): Eval[Unit] =
    write(kv._1) *> write(kv._2)

  def writeTupleStringPar(kv: (String, RhoTypeN)): Eval[Unit] =
    write(kv._1) *> write(kv._2)

  // Writes serialized value of a sequence
  def writeSeq[T](seq: Seq[T], f: T => Eval[Unit]): Eval[Unit] =
    write(seq.size) *> seq.traverse_(f)
}

/** Wrapper for protobuf de-serialization of primitive types. */
private class ProtobufReader(input: CodedInputStream) {
  def readByte: Eval[Byte]         = Eval.later(input.readRawByte())
  def readBytes: Eval[Array[Byte]] = Eval.later(input.readByteArray())
  def readBool: Eval[Boolean]      = Eval.later(input.readBool())
  def readInt: Eval[Int]           = Eval.later(input.readInt32())
  def readLong: Eval[Long]         = Eval.later(input.readInt64())
  def readString: Eval[String]     = Eval.later(input.readString())
}

object Serialization {

  // TODO: Properly handle errors
  @SuppressWarnings(Array("org.wartremover.warts.TryPartial"))
  def serializeToBytes(par: RhoTypeN): Eval[Array[Byte]] =
    par.serializedSize.flatMap { serSize =>
      Using(new ByteArrayOutputStream(serSize)) { baos =>
        Serialization.serialize(par, baos).map { _ =>
          baos.flush()
          baos.toByteArray
        }
      }.get
    }

  // TODO: Properly handle errors
  @SuppressWarnings(Array("org.wartremover.warts.TryPartial"))
  def deserializeFromBytes(bv: Array[Byte]): ParN =
    Using(new ByteArrayInputStream(bv))(Serialization.deserialize(_).value).get

  // TODO: Properly handle errors with return type (remove throw)
  def serialize(par: RhoTypeN, output: OutputStream): Eval[Unit] = {
    val cos         = CodedOutputStream.newInstance(output)
    val protoWriter = new ProtobufPrimitiveWriter(cos)

    // Serializer with recursive traversal of the whole object at once
    lazy val serializer = new ProtobufRecWriter(protoWriter, writeRec)

    // Serializer with recursive traversal using memoized values
    // val serializer = new ProtobufRecWriter(protoWriter, _.serialized.flatMap(protoWriter.write))

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def writeRec(p: RhoTypeN): Eval[Unit] = Eval.defer {
      import serializer._

      p match {

        /* Terminal expressions (0-arity constructors) */
        /* =========================================== */

        case _: NilN.type            => write(NIL)
        case gBool: GBoolN           => write(GBOOL) *> write(gBool.v)
        case gInt: GIntN             => write(GINT) *> write(gInt.v)
        case gBigInt: GBigIntN       => write(GBIG_INT) *> write(gBigInt.v)
        case gString: GStringN       => write(GSTRING) *> write(gString.v)
        case gByteArray: GByteArrayN => write(GBYTE_ARRAY) *> write(gByteArray.v)
        case gUri: GUriN             => write(GURI) *> write(gUri.v)
        case _: WildcardN.type       => write(WILDCARD)

        /* Unforgeable names */
        case unf: UnforgeableN =>
          val unfKind = unf match {
            case _: UPrivateN      => UPRIVATE
            case _: UDeployIdN     => UDEPLOY_ID
            case _: UDeployerIdN   => UDEPLOYER_ID
            case _: USysAuthTokenN => SYS_AUTH_TOKEN
          }
          write(unfKind) *> write(unf.v)

        /* Vars */
        case bVar: BoundVarN => write(BOUND_VAR) *> write(bVar.idx)
        case fVar: FreeVarN  => write(FREE_VAR) *> write(fVar.idx)
        case rVar: ConnVarRefN =>
          write(CONNECTIVE_VARREF) *> write(rVar.index) *> write(rVar.depth)

        /* Simple types */
        case _: ConnBoolN.type      => write(CONNECTIVE_BOOL)
        case _: ConnIntN.type       => write(CONNECTIVE_INT)
        case _: ConnBigIntN.type    => write(CONNECTIVE_BIG_INT)
        case _: ConnStringN.type    => write(CONNECTIVE_STRING)
        case _: ConnUriN.type       => write(CONNECTIVE_URI)
        case _: ConnByteArrayN.type => write(CONNECTIVE_BYTEARRAY)

        /* Unary expressions (1-arity constructors) */
        /* ======================================== */

        case op: Operation1ParN =>
          val tag = op match {
            case _: ENegN => ENEG
            case _: ENotN => ENOT
          }
          write(tag) *> write(op.p)

        case bundle: BundleN =>
          write(BUNDLE) *> write(bundle.body) *> write(bundle.writeFlag) *> write(bundle.readFlag)

        /* Connective */
        case connNot: ConnNotN => write(CONNECTIVE_NOT) *> write(connNot.p)

        /* Binary expressions (2-arity constructors) */
        /* ========================================= */

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

        case eMatches: EMatchesN =>
          write(EMATCHES) *> write(eMatches.target) *> write(eMatches.pattern)

        /* N-ary parameter expressions (N-arity constructors) */
        /* ================================================== */

        case pProc: ParProcN => write(PARPROC) *> write(pProc.sortedPs)

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

        case m: MatchN => write(MATCH) *> write(m.target) *> write(m.cases)

        case n: NewN =>
          write(NEW) *>
            write(n.bindCount) *>
            write(n.p) *>
            writeSeq[String](n.sortedUri, write) *>
            writeSeq[(String, ParN)](n.sortedInjections, writeTupleStringPar)

        /* Collections */
        case eList: EListN   => write(ELIST) *> write(eList.ps) *> write(eList.remainder)
        case eTuple: ETupleN => write(ETUPLE) *> write(eTuple.ps)
        case eSet: ESetN     => write(ESET) *> write(eSet.sortedPs) *> write(eSet.remainder)
        case eMap: EMapN =>
          write(EMAP) *>
            writeSeq[(ParN, ParN)](eMap.sortedPs, writeTuplePar) *>
            write(eMap.remainder)

        /* Connective */
        case connAnd: ConnAndN => write(CONNECTIVE_AND) *> write(connAnd.ps)
        case connOr: ConnOrN   => write(CONNECTIVE_OR) *> write(connOr.ps)

        case eMethod: EMethodN =>
          write(EMETHOD) *>
            write(eMethod.methodName) *>
            write(eMethod.target) *>
            write(eMethod.arguments)

        /* Auxiliary types */

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

        case unknownType => throw new Exception(s"Unknown type `$unknownType`")
      }
    }

    writeRec(par) <* Eval.later(cos.flush())
  }

  // TODO: Properly handle errors with return type (remove throw)
  def deserialize(input: InputStream): Eval[ParN] = {
    val cis    = CodedInputStream.newInstance(input)
    val reader = new ProtobufReader(cis)

    import reader._

    def readBigInt: Eval[BigInt] = readBytes.map(BigInt(_))

    // Reads a sequence
    def readSeq[T](v: Eval[T]): Eval[Seq[T]] = readInt.flatMap(Seq.range(0, _).as(v).sequence)

    // Reads par object with all nested objects
    // NOTE: defer is needed here to ensure correct deserialization
    def readPar: Eval[ParN] = Eval.defer(readByte) >>= matchPar

    // Reads sequence of pars
    def readPars: Eval[Seq[ParN]] = readSeq(readPar)

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def readVar: Eval[VarN] =
      readPar.map {
        case v: VarN => v
        case p       => throw new Exception(s"Value must be Var, found `$p`")
      }

    def readVarOpt: Eval[Option[VarN]] =
      readBool.flatMap(x => if (x) readVar.map(Some(_)) else Eval.now(none))

    def readTuplePar: Eval[(ParN, ParN)]         = (readPar, readPar).mapN((_, _))
    def readTupleStringPar: Eval[(String, ParN)] = (readString, readPar).mapN((_, _))

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def readReceiveBind(tag: Byte): Eval[ReceiveBindN] = tag match {
      case RECEIVE_BIND =>
        for {
          patterns  <- readPars
          source    <- readPar
          remainder <- readVarOpt
          freeCount <- readInt
        } yield ReceiveBindN(patterns, source, remainder, freeCount)
      case _ => throw new Exception(s"Invalid tag `$tag` for ReceiveBindN deserialization")
    }

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def readMatchMCase(tag: Byte): Eval[MatchCaseN] = tag match {
      case MATCH_CASE =>
        for {
          pattern   <- readPar
          source    <- readPar
          freeCount <- readInt
        } yield MatchCaseN(pattern, source, freeCount)
      case _ => throw new Exception(s"Invalid tag `$tag` for matchMCase deserialization")
    }

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def matchPar(tag: Byte): Eval[ParN] = tag match {

      /* Terminal expressions (0-arity constructors) */
      /* =========================================== */

      case NIL         => Eval.now(NilN)
      case GBOOL       => readBool.map(GBoolN(_))
      case GINT        => readLong.map(GIntN(_))
      case GBIG_INT    => readBigInt.map(GBigIntN(_))
      case GSTRING     => readString.map(GStringN(_))
      case GBYTE_ARRAY => readBytes.map(GByteArrayN(_))
      case GURI        => readString.map(GUriN(_))
      case WILDCARD    => Eval.now(WildcardN)

      /* Unforgeable names */
      case UPRIVATE => readBytes.map(UPrivateN(_))

      /* Vars */
      case BOUND_VAR         => readInt.map(BoundVarN(_))
      case FREE_VAR          => readInt.map(FreeVarN(_))
      case CONNECTIVE_VARREF => (readInt, readInt).mapN(ConnVarRefN(_, _))

      /* Simple types */
      case CONNECTIVE_BOOL      => Eval.now(ConnBoolN)
      case CONNECTIVE_INT       => Eval.now(ConnIntN)
      case CONNECTIVE_BIG_INT   => Eval.now(ConnBigIntN)
      case CONNECTIVE_STRING    => Eval.now(ConnStringN)
      case CONNECTIVE_URI       => Eval.now(ConnUriN)
      case CONNECTIVE_BYTEARRAY => Eval.now(ConnByteArrayN)

      case UDEPLOY_ID   => readBytes.map(UDeployIdN(_))
      case UDEPLOYER_ID => readBytes.map(UDeployerIdN(_))

      // TODO: Temporary solution for easier conversion from old types - change type in the future
      case SYS_AUTH_TOKEN => readBytes.as(USysAuthTokenN())

      /* Unary expressions (1-arity constructors) */
      /* ======================================== */

      case ENEG => readPar.map(ENegN(_))
      case ENOT => readPar.map(ENotN(_))

      case BUNDLE => (readPar, readBool, readBool).mapN(BundleN(_, _, _))

      /* Connective */
      case CONNECTIVE_NOT => readPar.map(ConnNotN(_))

      /* Binary expressions (2-arity constructors) */
      /* ========================================= */

      case EPLUS       => (readPar, readPar).mapN(EPlusN(_, _))
      case EMINUS      => (readPar, readPar).mapN(EMinusN(_, _))
      case EMULT       => (readPar, readPar).mapN(EMultN(_, _))
      case EDIV        => (readPar, readPar).mapN(EDivN(_, _))
      case EMOD        => (readPar, readPar).mapN(EModN(_, _))
      case ELT         => (readPar, readPar).mapN(ELtN(_, _))
      case ELTE        => (readPar, readPar).mapN(ELteN(_, _))
      case EGT         => (readPar, readPar).mapN(EGtN(_, _))
      case EGTE        => (readPar, readPar).mapN(EGteN(_, _))
      case EEQ         => (readPar, readPar).mapN(EEqN(_, _))
      case ENEQ        => (readPar, readPar).mapN(ENeqN(_, _))
      case EAND        => (readPar, readPar).mapN(EAndN(_, _))
      case ESHORTAND   => (readPar, readPar).mapN(EShortAndN(_, _))
      case EOR         => (readPar, readPar).mapN(EOrN(_, _))
      case ESHORTOR    => (readPar, readPar).mapN(EShortOrN(_, _))
      case EPLUSPLUS   => (readPar, readPar).mapN(EPlusPlusN(_, _))
      case EMINUSMINUS => (readPar, readPar).mapN(EMinusMinusN(_, _))
      case EPERCENT    => (readPar, readPar).mapN(EPercentPercentN(_, _))

      case EMATCHES => (readPar, readPar).mapN(EMatchesN(_, _))

      /* N-ary parameter expressions (N-arity constructors) */
      /* ================================================== */

      case PARPROC => readPars.map(ParProcN(_))

      case SEND => (readPar, readPars, readBool).mapN(SendN(_, _, _))

      case RECEIVE =>
        (readSeq(readByte >>= readReceiveBind), readPar, readBool, readBool, readInt)
          .mapN(ReceiveN(_, _, _, _, _))

      case MATCH =>
        (readPar, readSeq(readByte >>= readMatchMCase)).mapN(MatchN(_, _))

      case NEW =>
        (readInt, readPar, readSeq(readString), readSeq(readTupleStringPar)).mapN(NewN(_, _, _, _))

      /* Collections */
      case ELIST  => (readPars, readVarOpt).mapN(EListN(_, _))
      case ETUPLE => readPars.map(ETupleN(_))
      case ESET   => (readPars, readVarOpt).mapN(ESetN(_, _))
      case EMAP   => (readSeq(readTuplePar), readVarOpt).mapN(EMapN(_, _))

      /* Connective */
      case CONNECTIVE_AND => readPars.map(ConnAndN(_))
      case CONNECTIVE_OR  => readPars.map(ConnOrN(_))

      case EMETHOD => (readString, readPar, readPars).mapN(EMethodN(_, _, _))

      case _ => throw new Exception(s"Invalid tag `$tag` for ParN deserialization")
    }

    readPar
  }
}
