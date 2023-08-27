package coop.rchain.models.rholangn.parmanager

import cats.Eval
import cats.syntax.all._
import coop.rchain.models.rholangn._
import coop.rchain.models.rholangn.parmanager.Constants._
import coop.rchain.models.rholangn.parmanager.protobuf.ProtoBlakeHashing._

object RhoHash {

  def arE(b: Byte): Eval[Array[Byte]] = Eval.now(Array[Byte](b))

  def hashOpt(opt: Option[ParN]): Eval[Array[Byte]] =
    opt.map(x => (hashTrue, x.rhoHash).mapN(_ ++ _)).getOrElse(hashFalse)

  def hashReceiveBind(p: ReceiveBindN): Eval[Array[Byte]] =
    (arE(RECEIVE_BIND)
      +++ p.patterns.traverse(_.rhoHash)
      ++ hash(p.freeCount)
      ++ p.source.rhoHash
      ++ hashOpt(p.remainder))
      .map(hash)

  def hashMatchCase(p: MatchCaseN): Eval[Array[Byte]] =
    (arE(MATCH_CASE) ++ p.pattern.rhoHash ++ p.source.rhoHash ++ hash(p.freeCount)).map(hash)

  /**
    * Computes hash of the Rholang AST types.
    *
    * @param input Rholang AST root object
    */
  // TODO: Properly handle errors with return type (remove throw)
  @SuppressWarnings(Array("org.wartremover.warts.Throw"))
  def hashRec(input: RhoTypeN): Eval[Array[Byte]] = Eval.defer {
    input match {

      /* Terminal expressions (0-arity constructors) */
      /* =========================================== */

      case _: NilN.type | _: GBoolN | _: GIntN | _: GBigIntN | _: GStringN | _: GByteArrayN |
          _: GUriN | _: WildcardN.type
          /* Unforgeable names */
          | _: UnforgeableN
          /* Vars */
          | _: BoundVarN | _: FreeVarN | _: ConnVarRefN
          /* Simple types */
          | _: ConnBoolN.type | _: ConnIntN.type | _: ConnBigIntN.type | _: ConnStringN.type |
          _: ConnUriN.type | _: ConnByteArrayN.type =>
        input.serialized.map(hash)

      /* Unary expressions (1-arity constructors) */
      /* ======================================== */

      case p: Operation1ParN =>
        val (tag, op) = p match {
          case e: ENegN => (ENEG, e.p)
          case e: ENotN => (ENOT, e.p)
        }
        (arE(tag) ++ op.rhoHash).map(hash)

      case p: BundleN => (arE(BUNDLE) ++ p.body.rhoHash).map(hash)

      /* Connective */
      case p: ConnNotN => (arE(CONNECTIVE_NOT) ++ p.p.rhoHash).map(hash)

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
        (arE(tag) ++ p.p1.rhoHash ++ p.p2.rhoHash).map(hash)

      case p: EMatchesN => (arE(EMATCHES) ++ p.target.rhoHash ++ p.pattern.rhoHash).map(hash)

      /* N-ary parameter expressions (N-arity constructors) */
      /* ================================================== */

      case p: ParProcN => (arE(PARPROC) +|+ p.ps.traverse(_.rhoHash)).map(hash)

      case p: SendN =>
        (arE(SEND) ++ p.chan.rhoHash ++ hash(p.persistent) +|+ p.args.traverse(_.rhoHash)).map(hash)

      case p: ReceiveN =>
        (arE(RECEIVE)
          ++ hash(p.persistent)
          ++ hash(p.peek)
          ++ hash(p.bindCount)
          +|+ p.binds.traverse(_.rhoHash)
          ++ p.body.rhoHash)
          .map(hash)

      case p: MatchN => (arE(MATCH) ++ p.target.rhoHash +++ p.cases.traverse(_.rhoHash)).map(hash)

      case p: NewN =>
        (arE(NEW)
          ++ hash(p.bindCount)
          +|+ p.uri.traverse(_.rhoHash)
          +|+ p.injections.toSeq.traverse(_.bimap(_.rhoHash, _.rhoHash).mapN(_ ++ _))
          ++ p.p.rhoHash)
          .map(hash)

      /* Collections */
      case p: ETupleN => (arE(ETUPLE) +++ p.ps.traverse(_.rhoHash)).map(hash)
      case p: EListN  => (arE(ELIST) +++ p.ps.traverse(_.rhoHash) ++ hashOpt(p.remainder)).map(hash)
      case p: ESetN =>
        (arE(ESET) +|+ p.ps.toSeq.traverse(_.rhoHash) ++ hashOpt(p.remainder)).map(hash)
      case p: EMapN =>
        (arE(EMAP)
          +|+ p.ps.toSeq.traverse(_.bimap(_.rhoHash, _.rhoHash).mapN(_ ++ _))
          ++ hashOpt(p.remainder))
          .map(hash)

      /* Connective */
      case p: ConnAndN => (arE(CONNECTIVE_AND) +++ p.ps.traverse(_.rhoHash)).map(hash)
      case p: ConnOrN  => (arE(CONNECTIVE_OR) +++ p.ps.traverse(_.rhoHash)).map(hash)

      case p: EMethodN =>
        (arE(EMETHOD) ++ hash(p.methodName) +++ p.args.traverse(_.rhoHash) ++ p.target.rhoHash)
          .map(hash)

      case p => throw new Exception(s"Unknown type `$p`")
    }
  }
}
