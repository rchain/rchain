package coop.rchain.models.rholangn.parmanager

import cats.Eval
import coop.rchain.models.rholangn._
import coop.rchain.models.rholangn.parmanager.Constants._
import coop.rchain.models.rholangn.parmanager.primitive.PrimitiveWriter
import cats.syntax.all._

private[parmanager] object RhoHash {

  /**
    * Serialization of the Rholang AST types.
    *
    * @param p    Rholang AST root object
    * @param wrt  Writer of primitive types
    * @param memo Use memoization for all children fields recursively
    */
  // TODO: Properly handle errors with return type (remove throw)
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def serializeForHash(p: RhoTypeN, wrt: PrimitiveWriter[Eval]): Eval[Unit] =
    Eval.defer {
      val rhoWriter: RhoRecWriter[Eval] = RhoRecWriter(wrt, _.rhoHash.flatMap(wrt.writeRaw))
      import rhoWriter._
      import wrt._

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
}
