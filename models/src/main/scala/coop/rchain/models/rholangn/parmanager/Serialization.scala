package coop.rchain.models.rholangn.parmanager

import cats.Eval
import cats.syntax.all._
import coop.rchain.models.rholangn._
import coop.rchain.models.rholangn.parmanager.Constants._
import coop.rchain.models.rholangn.parmanager.primitive.{PrimitiveReader, PrimitiveWriter}

object Serialization {

  /**
    * Serialization of the Rholang AST types.
    *
    * @param inp Rholang AST root object
    * @param wrt Writer of primitive types
    * @param memo Use memoization for all children fields recursively
    */
  // TODO: Properly handle errors with return type (remove throw)
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def serialize(inp: RhoTypeN, wrt: PrimitiveWriter[Eval], memo: Boolean): Eval[Unit] = Eval.defer {
    // Recursive traversal with or without memoization of all children objects
    val writePar: RhoTypeN => Eval[Unit] =
      if (memo)
        // Recursive traversal using memoized values
        _.serialized.flatMap(wrt.writeRaw)
      else
        // Recursive traversal of the whole object without memoization of intermediaries
        serialize(_, wrt, memo)

    val rhoWriter: RhoRecWriter[Eval] = RhoRecWriter(wrt, writePar)

    import rhoWriter._
    import wrt._

    def writeReceiveBind(p: ReceiveBindN) =
      write(RECEIVE_BIND) *>
        writeSeq(p.patterns) *>
        writePar(p.source) *>
        writeOpt(p.remainder) *>
        write(p.freeCount)

    def writeMatchCase(p: MatchCaseN) =
      write(MATCH_CASE) *>
        writePar(p.pattern) *>
        writePar(p.source) *>
        write(p.freeCount)

    inp match {

      /* Terminal expressions (0-arity constructors) */
      /* =========================================== */

      case _: NilN.type      => write(NIL)
      case p: GBoolN         => write(GBOOL) *> write(p.v)
      case p: GIntN          => write(GINT) *> write(p.v)
      case p: GBigIntN       => write(GBIG_INT) *> writeBigInt(p.v)
      case p: GStringN       => write(GSTRING) *> write(p.v)
      case p: GByteArrayN    => write(GBYTE_ARRAY) *> write(p.v)
      case p: GUriN          => write(GURI) *> write(p.v)
      case _: WildcardN.type => write(WILDCARD)

      /* Unforgeable names */
      case p: UnforgeableN =>
        val unfKind = p match {
          case _: UPrivateN      => UPRIVATE
          case _: UDeployIdN     => UDEPLOY_ID
          case _: UDeployerIdN   => UDEPLOYER_ID
          case _: USysAuthTokenN => SYS_AUTH_TOKEN
        }
        write(unfKind) *> write(p.v)

      /* Vars */
      case p: BoundVarN   => write(BOUND_VAR) *> write(p.idx)
      case p: FreeVarN    => write(FREE_VAR) *> write(p.idx)
      case p: ConnVarRefN => write(CONNECTIVE_VARREF) *> write(p.index) *> write(p.depth)

      /* Simple types */
      case _: ConnBoolN.type      => write(CONNECTIVE_BOOL)
      case _: ConnIntN.type       => write(CONNECTIVE_INT)
      case _: ConnBigIntN.type    => write(CONNECTIVE_BIG_INT)
      case _: ConnStringN.type    => write(CONNECTIVE_STRING)
      case _: ConnUriN.type       => write(CONNECTIVE_URI)
      case _: ConnByteArrayN.type => write(CONNECTIVE_BYTEARRAY)

      /* Unary expressions (1-arity constructors) */
      /* ======================================== */

      case p: Operation1ParN =>
        val tag = p match {
          case _: ENegN => ENEG
          case _: ENotN => ENOT
        }
        write(tag) *> writePar(p.p)

      case p: BundleN =>
        write(BUNDLE) *> writePar(p.body) *> write(p.writeFlag) *> write(p.readFlag)

      /* Connective */
      case p: ConnNotN => write(CONNECTIVE_NOT) *> writePar(p.p)

      /* Binary expressions (2-arity constructors) */
      /* ========================================= */

      case p: Operation2ParN =>
        val tag = p match {
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
        write(tag) *> writePar(p.p1) *> writePar(p.p2)

      case p: EMatchesN => write(EMATCHES) *> writePar(p.target) *> writePar(p.pattern)

      /* N-ary parameter expressions (N-arity constructors) */
      /* ================================================== */

      case p: ParProcN => write(PARPROC) *> p.psSorted.flatMap(writeSeq)

      case p: SendN => write(SEND) *> writePar(p.chan) *> writeSeq(p.args) *> write(p.persistent)

      case p: ReceiveN =>
        write(RECEIVE) *>
          p.bindsSorted.flatMap(writeSeq(_, writeReceiveBind)) *>
          writePar(p.body) *>
          write(p.persistent) *>
          write(p.peek) *>
          write(p.bindCount)

      case p: MatchN => write(MATCH) *> writePar(p.target) *> writeSeq(p.cases, writeMatchCase)

      case p: NewN =>
        write(NEW) *>
          write(p.bindCount) *>
          writePar(p.p) *>
          p.urisSorted.flatMap(writeSeq) *>
          p.injectionsSorted.flatMap(writeSeq(_, writeTuplePar))

      /* Collections */
      case p: EListN  => write(ELIST) *> writeSeq(p.ps) *> writeOpt(p.remainder)
      case p: ETupleN => write(ETUPLE) *> writeSeq(p.ps)
      case p: ESetN   => write(ESET) *> p.psSorted.flatMap(writeSeq) *> writeOpt(p.remainder)
      case p: EMapN =>
        write(EMAP) *> p.psSorted.flatMap(writeSeq(_, writeTuplePar)) *> writeOpt(p.remainder)

      /* Connective */
      case p: ConnAndN => write(CONNECTIVE_AND) *> writeSeq(p.ps)
      case p: ConnOrN  => write(CONNECTIVE_OR) *> writeSeq(p.ps)

      case eMethod: EMethodN =>
        write(EMETHOD) *>
          write(eMethod.methodName) *>
          writePar(eMethod.target) *>
          writeSeq(eMethod.args)

      /* Auxiliary types */
      case p: ReceiveBindN =>
        write(RECEIVE_BIND) *>
          writeSeq(p.patterns) *>
          writePar(p.source) *>
          writeOpt(p.remainder) *>
          write(p.freeCount)

      case p: MatchCaseN =>
        write(MATCH_CASE) *>
          writePar(p.pattern) *>
          writePar(p.source) *>
          write(p.freeCount)

      case p => throw new Exception(s"Unknown type `$p`")
    }
  }

  // TODO: Properly handle errors with return type (remove throw)
  def deserialize(primitiveReader: PrimitiveReader[Eval]): Eval[ParN] = {
    import primitiveReader._

    def readBigInt: Eval[BigInt] = readBytes.map(BigInt(_))

    // Reads a sequence
    def readSeq[T](v: Eval[T]): Eval[Seq[T]] = readInt.flatMap(Seq.range(0, _).as(v).sequence)

    // Reads par object with all nested objects
    def readPar: Eval[ParN] = readByte >>= matchPar

    // Reads sequence of pars
    def readPars: Eval[Seq[ParN]] = readSeq(readPar)

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def readVar: Eval[VarN] =
      readPar.map {
        case v: VarN => v
        case p       => throw new Exception(s"Expected VarN, found `$p`")
      }

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def readGString: Eval[GStringN] =
      readPar.map {
        case v: GStringN => v
        case p           => throw new Exception(s"Expected GStringN, found `$p`")
      }

    def readVarOpt: Eval[Option[VarN]] =
      readBool.flatMap(x => if (x) readVar.map(Some(_)) else Eval.now(none))

    def readTuplePar: Eval[(ParN, ParN)]           = (readPar, readPar).mapN((_, _))
    def readTupleStringPar: Eval[(GStringN, ParN)] = (readGString, readPar).mapN((_, _))

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def readReceiveBind(tag: Byte): Eval[ReceiveBindN] = tag match {
      case RECEIVE_BIND => (readPars, readPar, readVarOpt, readInt).mapN(ReceiveBindN(_, _, _, _))
      case _            => throw new Exception(s"Invalid tag `$tag` for ReceiveBindN deserialization")
    }

    @SuppressWarnings(Array("org.wartremover.warts.Throw"))
    def readMatchMCase(tag: Byte): Eval[MatchCaseN] = tag match {
      case MATCH_CASE => (readPar, readPar, readInt).mapN(MatchCaseN(_, _, _))
      case _          => throw new Exception(s"Invalid tag `$tag` for matchMCase deserialization")
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
      case UPRIVATE     => readBytes.map(UPrivateN(_))
      case UDEPLOY_ID   => readBytes.map(UDeployIdN(_))
      case UDEPLOYER_ID => readBytes.map(UDeployerIdN(_))
      // TODO: Temporary solution for easier conversion from old types - change type in the future
      case SYS_AUTH_TOKEN => readBytes.as(USysAuthTokenN())

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

      case MATCH => (readPar, readSeq(readByte >>= readMatchMCase)).mapN(MatchN(_, _))

      case NEW =>
        (readInt, readPar, readSeq(readGString), readSeq(readTupleStringPar)).mapN(NewN(_, _, _, _))

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
