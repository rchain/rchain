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
    * @param p Rholang AST root object
    * @param wrt Writer of primitive types
    * @param memo Use memoization for all children fields recursively
    */
  // TODO: Properly handle errors with return type (remove throw)
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def serialize(p: RhoTypeN, wrt: PrimitiveWriter[Eval], memo: Boolean): Eval[Unit] = Eval.defer {
    // Recursive traversal with or without memoization of all children objects
    val writePar: RhoTypeN => Eval[Unit] =
      if (memo)
        // Recursive traversal using memoized values
        _.serialized.flatMap(wrt.writeRaw)
      else
        // Recursive traversal of the whole object without memoization of intermediaries
        serialize(_, wrt, memo)

    val rhoWriter: RhoRecWriter[Eval] = RhoRecWriter(wrt, writePar)

    import wrt._
    import rhoWriter._

    p match {

      /* Terminal expressions (0-arity constructors) */
      /* =========================================== */

      case _: NilN.type            => write(NIL)
      case gBool: GBoolN           => write(GBOOL) *> write(gBool.v)
      case gInt: GIntN             => write(GINT) *> write(gInt.v)
      case gBigInt: GBigIntN       => write(GBIG_INT) *> writeBigInt(gBigInt.v)
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
        write(tag) *> writePar(op.p)

      case b: BundleN =>
        write(BUNDLE) *> writePar(b.body) *> write(b.writeFlag) *> write(b.readFlag)

      /* Connective */
      case connNot: ConnNotN => write(CONNECTIVE_NOT) *> writePar(connNot.p)

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
        write(tag) *> writePar(op.p1) *> writePar(op.p2)

      case eMatches: EMatchesN =>
        write(EMATCHES) *> writePar(eMatches.target) *> writePar(eMatches.pattern)

      /* N-ary parameter expressions (N-arity constructors) */
      /* ================================================== */

      case pProc: ParProcN => write(PARPROC) *> writeSeq(pProc.sortedPs)

      case send: SendN =>
        write(SEND) *>
          writePar(send.chan) *>
          writeSeq(send.data) *>
          write(send.persistent)

      case receive: ReceiveN =>
        write(RECEIVE) *>
          writeSeq(receive.sortedBinds) *>
          writePar(receive.body) *>
          write(receive.persistent) *>
          write(receive.peek) *>
          write(receive.bindCount)

      case m: MatchN => write(MATCH) *> writePar(m.target) *> writeSeq(m.cases, writePar)

      case n: NewN =>
        write(NEW) *>
          write(n.bindCount) *>
          writePar(n.p) *>
          writeSeq[String](n.sortedUri, write) *>
          writeSeq[(String, ParN)](n.sortedInjections, writeTupleStringPar(_))

      /* Collections */
      case eList: EListN   => write(ELIST) *> writeSeq(eList.ps) *> writeOpt(eList.remainder)
      case eTuple: ETupleN => write(ETUPLE) *> writeSeq(eTuple.ps)
      case eSet: ESetN     => write(ESET) *> writeSeq(eSet.sortedPs) *> writeOpt(eSet.remainder)
      case eMap: EMapN =>
        write(EMAP) *>
          writeSeq[(ParN, ParN)](eMap.sortedPs, writeTuplePar(_)) *>
          writeOpt(eMap.remainder)

      /* Connective */
      case connAnd: ConnAndN => write(CONNECTIVE_AND) *> writeSeq(connAnd.ps)
      case connOr: ConnOrN   => write(CONNECTIVE_OR) *> writeSeq(connOr.ps)

      case eMethod: EMethodN =>
        write(EMETHOD) *>
          write(eMethod.methodName) *>
          writePar(eMethod.target) *>
          writeSeq(eMethod.arguments)

      /* Auxiliary types */
      case bind: ReceiveBindN =>
        write(RECEIVE_BIND) *>
          writeSeq(bind.patterns) *>
          writePar(bind.source) *>
          writeOpt(bind.remainder) *>
          write(bind.freeCount)

      case mCase: MatchCaseN =>
        write(MATCH_CASE) *>
          writePar(mCase.pattern) *>
          writePar(mCase.source) *>
          write(mCase.freeCount)

      case unknownType => throw new Exception(s"Unknown type `$unknownType`")
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
