package coop.rchain.rholang.interpreter.compiler

import java.util.UUID

import cats.effect.Sync
import cats.implicits._
import cats.{Applicative, MonadError}
import coop.rchain.models.Connective.ConnectiveInstance._
import coop.rchain.models.Expr.ExprInstance._
import coop.rchain.models.Var.VarInstance._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.{
  Bundle => AbsynBundle,
  Ground => AbsynGround,
  KeyValuePair => AbsynKeyValuePair,
  Send => AbsynSend,
  _
}
import monix.eval.Coeval

import scala.collection.convert.ImplicitConversionsToScala._
import scala.collection.immutable.{BitSet, Vector}
import scala.util.Try

sealed trait VarSort
case object ProcSort extends VarSort
case object NameSort extends VarSort

object BoolNormalizeMatcher {
  def normalizeMatch(b: BoolLiteral): GBool =
    b match {
      case _: BoolTrue  => GBool(true)
      case _: BoolFalse => GBool(false)
    }
}

object GroundNormalizeMatcher {
  def normalizeMatch[M[_]](g: AbsynGround)(implicit M: MonadError[M, Throwable]): M[Expr] =
    g match {
      case gb: GroundBool => Expr(BoolNormalizeMatcher.normalizeMatch(gb.boolliteral_)).pure[M]
      case gi: GroundInt =>
        M.fromTry(
            Try(gi.longliteral_.toLong).adaptError {
              case e: NumberFormatException => NormalizerError(e.getMessage)
            }
          )
          .map { long =>
            Expr(GInt(long))
          }
      case gs: GroundString => Expr(GString(stripString(gs.stringliteral_))).pure[M]
      case gu: GroundUri    => Expr(GUri(stripUri(gu.uriliteral_))).pure[M]
    }
  // This is necessary to remove the backticks. We don't use a regular
  // expression because they're always there.
  def stripUri(raw: String): String = {
    require(raw.length >= 2)
    raw.substring(1, raw.length - 1)
  }
  // Similarly, we need to remove quotes from strings, since we are using
  // a custom string token
  def stripString(raw: String): String = {
    require(raw.length >= 2)
    raw.substring(1, raw.length - 1)
  }
}

object RemainderNormalizeMatcher {
  def handleProcVar[M[_]](pv: ProcVar, knownFree: DeBruijnLevelMap[VarSort])(
      implicit sync: Sync[M]
  ): M[(Option[Var], DeBruijnLevelMap[VarSort])] =
    pv match {
      case pvw: ProcVarWildcard =>
        (
          Option(Var(Wildcard(Var.WildcardMsg()))),
          knownFree.addWildcard(SourcePosition(pvw.line_num, pvw.col_num))
        ).pure[M]
      case pvv: ProcVarVar =>
        val sourcePosition = SourcePosition(pvv.line_num, pvv.col_num)
        knownFree.get(pvv.var_) match {
          case None =>
            val newBindingsPair = knownFree.put((pvv.var_, ProcSort, sourcePosition))
            (Option(Var(FreeVar(knownFree.nextLevel))), newBindingsPair).pure[M]
          case Some(LevelContext(_, _, firstSourcePosition)) =>
            sync.raiseError(
              UnexpectedReuseOfProcContextFree(pvv.var_, firstSourcePosition, sourcePosition)
            )
        }
    }

  def normalizeMatchProc[M[_]](r: ProcRemainder, knownFree: DeBruijnLevelMap[VarSort])(
      implicit err: Sync[M]
  ): M[(Option[Var], DeBruijnLevelMap[VarSort])] =
    r match {
      case _: ProcRemainderEmpty => (None: Option[Var], knownFree).pure[M]
      case pr: ProcRemainderVar =>
        handleProcVar[M](pr.procvar_, knownFree)
    }

  def normalizeMatchName[M[_]](nr: NameRemainder, knownFree: DeBruijnLevelMap[VarSort])(
      implicit err: Sync[M]
  ): M[(Option[Var], DeBruijnLevelMap[VarSort])] =
    nr match {
      case _: NameRemainderEmpty => (None: Option[Var], knownFree).pure[M]
      case nr: NameRemainderVar =>
        handleProcVar[M](nr.procvar_, knownFree)
    }
}

object CollectionNormalizeMatcher {
  def normalizeMatch[M[_]](c: Collection, input: CollectVisitInputs)(
      implicit sync: Sync[M],
      env: Map[String, Par]
  ): M[CollectVisitOutputs] = {
    def foldMatch[T](
        knownFree: DeBruijnLevelMap[VarSort],
        listproc: List[Proc],
        constructor: (Seq[Par], AlwaysEqual[BitSet], Boolean) => T
    )(implicit toExpr: T => Expr): M[CollectVisitOutputs] = {
      val init = (Vector[Par](), knownFree, BitSet(), false)
      listproc
        .foldM(init) { (acc, proc) =>
          ProcNormalizeMatcher
            .normalizeMatch[M](proc, ProcVisitInputs(VectorPar(), input.env, acc._2))
            .map { result =>
              (
                result.par +: acc._1,
                result.knownFree,
                acc._3 | result.par.locallyFree,
                acc._4 || result.par.connectiveUsed
              )
            }
        }
        .map {
          case (ps, resultKnownFree, locallyFree, connectiveUsed) =>
            CollectVisitOutputs(
              constructor(ps.reverse, locallyFree, connectiveUsed),
              resultKnownFree
            )
        }
    }

    def foldMatchMap(
        knownFree: DeBruijnLevelMap[VarSort],
        remainder: Option[Var],
        listProc: List[AbsynKeyValuePair]
    ) = {
      val init = (Vector[(Par, Par)](), knownFree, BitSet(), false)
      listProc
        .foldM(init) { (acc, e) =>
          e match {
            case e: KeyValuePairImpl =>
              for {
                keyResult <- ProcNormalizeMatcher.normalizeMatch[M](
                              e.proc_1,
                              ProcVisitInputs(VectorPar(), input.env, acc._2)
                            )
                valResult <- ProcNormalizeMatcher.normalizeMatch[M](
                              e.proc_2,
                              ProcVisitInputs(VectorPar(), input.env, keyResult.knownFree)
                            )
              } yield (
                Vector((keyResult.par, valResult.par)) ++ acc._1,
                valResult.knownFree,
                acc._3 | keyResult.par.locallyFree | valResult.par.locallyFree,
                acc._4 || keyResult.par.connectiveUsed || valResult.par.connectiveUsed
              )
          }
        }
        .map { folded =>
          val resultKnownFree         = folded._2
          val remainderConnectiveUsed = remainder.exists(HasLocallyFree[Var].connectiveUsed(_))
          val remainderLocallyFree =
            remainder.map(HasLocallyFree[Var].locallyFree(_, 0)).getOrElse(BitSet())

          CollectVisitOutputs(
            ParMap(
              seq = folded._1.reverse,
              connectiveUsed = folded._4 || remainderConnectiveUsed,
              locallyFree = folded._3 | remainderLocallyFree,
              remainder = remainder
            ),
            resultKnownFree
          )
        }
    }

    c match {
      case cl: CollectList =>
        RemainderNormalizeMatcher
          .normalizeMatchProc[M](cl.procremainder_, input.knownFree)
          .flatMap {
            case (optionalRemainder, knownFree) =>
              val constructor: Option[Var] => (Seq[Par], AlwaysEqual[BitSet], Boolean) => EList =
                optionalRemainder =>
                  (ps, lf, cu) => {
                    val tmpEList = EList(ps, lf, cu, optionalRemainder)
                    tmpEList.withConnectiveUsed(
                      tmpEList.connectiveUsed || optionalRemainder.isDefined
                    )
                  }

              foldMatch(knownFree, cl.listproc_.toList, constructor(optionalRemainder))
          }

      case ct: CollectTuple =>
        val ps = ct.tuple_ match {
          case ts: TupleSingle   => Seq(ts.proc_)
          case tm: TupleMultiple => Seq(tm.proc_) ++ tm.listproc_.toList
        }
        foldMatch(input.knownFree, ps.toList, ETuple.apply)

      case cs: CollectSet =>
        RemainderNormalizeMatcher
          .normalizeMatchProc[M](cs.procremainder_, input.knownFree)
          .flatMap {
            case (optionalRemainder, knownFree) =>
              val constructor: Option[Var] => (Seq[Par], AlwaysEqual[BitSet], Boolean) => ParSet =
                optionalRemainder =>
                  (pars, locallyFree, connectiveUsed) => {
                    val tmpParSet =
                      ParSet(pars, connectiveUsed, Coeval.delay(locallyFree.get), optionalRemainder)
                    tmpParSet.copy(
                      connectiveUsed = tmpParSet.connectiveUsed || optionalRemainder.isDefined
                    )
                  }

              foldMatch(knownFree, cs.listproc_.toList, constructor(optionalRemainder))
          }

      case cm: CollectMap =>
        RemainderNormalizeMatcher
          .normalizeMatchProc[M](cm.procremainder_, input.knownFree)
          .flatMap {
            case (optionalRemainder, knownFree) =>
              foldMatchMap(knownFree, optionalRemainder, cm.listkeyvaluepair_.toList)
          }
    }
  }
}

object NameNormalizeMatcher {
  def normalizeMatch[M[_]](n: Name, input: NameVisitInputs)(
      implicit err: Sync[M],
      env: Map[String, Par]
  ): M[NameVisitOutputs] =
    n match {
      case wc: NameWildcard =>
        val wildcardBindResult =
          input.knownFree.addWildcard(SourcePosition(wc.line_num, wc.col_num))
        NameVisitOutputs(EVar(Wildcard(Var.WildcardMsg())), wildcardBindResult).pure[M]
      case n: NameVar =>
        input.env.get(n.var_) match {
          case Some(IndexContext(level, NameSort, _)) => {
            NameVisitOutputs(EVar(BoundVar(level)), input.knownFree).pure[M]
          }
          case Some(IndexContext(_, ProcSort, sourcePosition)) => {
            err.raiseError(
              UnexpectedNameContext(n.var_, sourcePosition, SourcePosition(n.line_num, n.col_num))
            )
          }
          case None => {
            input.knownFree.get(n.var_) match {
              case None =>
                val newBindingsPair =
                  input.knownFree.put((n.var_, NameSort, SourcePosition(n.line_num, n.col_num)))
                NameVisitOutputs(EVar(FreeVar(input.knownFree.nextLevel)), newBindingsPair).pure[M]
              case Some(LevelContext(_, _, sourcePosition)) =>
                err.raiseError(
                  UnexpectedReuseOfNameContextFree(
                    n.var_,
                    sourcePosition,
                    SourcePosition(n.line_num, n.col_num)
                  )
                )
            }
          }
        }

      case n: NameQuote => {
        ProcNormalizeMatcher
          .normalizeMatch[M](n.proc_, ProcVisitInputs(VectorPar(), input.env, input.knownFree))
          .map(
            procVisitResult => NameVisitOutputs(procVisitResult.par, procVisitResult.knownFree)
          )
      }
    }

}

object ProcNormalizeMatcher {
  // FIXME before adding any more implicits, or fields to the `*VisitInputs` classes, make the normalizer use
  // ApplicativeAsk / MonadState instead
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def normalizeMatch[M[_]](p: Proc, input: ProcVisitInputs)(
      implicit sync: Sync[M],
      env: Map[String, Par]
  ): M[ProcVisitOutputs] = Sync[M].defer {
    def unaryExp[T](subProc: Proc, input: ProcVisitInputs, constructor: Par => T)(
        implicit toExprInstance: T => Expr
    ): M[ProcVisitOutputs] =
      normalizeMatch[M](subProc, input.copy(par = VectorPar()))
        .map(
          subResult =>
            ProcVisitOutputs(
              input.par.prepend(constructor(subResult.par), input.env.depth),
              subResult.knownFree
            )
        )

    def binaryExp[T](
        subProcLeft: Proc,
        subProcRight: Proc,
        input: ProcVisitInputs,
        constructor: (Par, Par) => T
    )(implicit toExprInstance: T => Expr): M[ProcVisitOutputs] =
      for {
        leftResult <- normalizeMatch[M](subProcLeft, input.copy(par = VectorPar()))
        rightResult <- normalizeMatch[M](
                        subProcRight,
                        input.copy(par = VectorPar(), knownFree = leftResult.knownFree)
                      )
      } yield ProcVisitOutputs(
        input.par.prepend(constructor(leftResult.par, rightResult.par), input.env.depth),
        rightResult.knownFree
      )

    def normalizeIfElse(
        valueProc: Proc,
        trueBodyProc: Proc,
        falseBodyProc: Proc,
        input: ProcVisitInputs
    ): M[ProcVisitOutputs] =
      for {
        targetResult <- normalizeMatch[M](valueProc, input)
        trueCaseBody <- normalizeMatch[M](
                         trueBodyProc,
                         ProcVisitInputs(VectorPar(), input.env, targetResult.knownFree)
                       )
        falseCaseBody <- normalizeMatch[M](
                          falseBodyProc,
                          ProcVisitInputs(VectorPar(), input.env, trueCaseBody.knownFree)
                        )
        desugaredIf = Match(
          targetResult.par,
          Vector(
            MatchCase(GBool(true), trueCaseBody.par, 0),
            MatchCase(GBool(false), falseCaseBody.par, 0)
          ),
          targetResult.par.locallyFree | trueCaseBody.par.locallyFree | falseCaseBody.par.locallyFree,
          targetResult.par.connectiveUsed || trueCaseBody.par.connectiveUsed || falseCaseBody.par.connectiveUsed
        )
      } yield ProcVisitOutputs(input.par.prepend(desugaredIf), falseCaseBody.knownFree)

    def failOnInvalidConnective(
        depth: Int,
        nameRes: NameVisitOutputs
    ): Either[InterpreterError, NameVisitOutputs] =
      if (input.env.depth == 0) {
        Either
          .fromOption(
            nameRes.knownFree.connectives
              .collectFirst {
                case (_: ConnOrBody, sourcePosition) =>
                  PatternReceiveError(s"\\/ (disjunction) at $sourcePosition")
                case (_: ConnNotBody, sourcePosition) =>
                  PatternReceiveError(s"~ (negation) at $sourcePosition")
              },
            nameRes
          )
          .swap
      } else Right(nameRes)

    p match {
      case p: PNegation =>
        normalizeMatch[M](
          p.proc_,
          ProcVisitInputs(VectorPar(), input.env, DeBruijnLevelMap.empty)
        ).map(
          bodyResult =>
            ProcVisitOutputs(
              input.par.prepend(Connective(ConnNotBody(bodyResult.par)), input.env.depth),
              input.knownFree
                .addConnective(
                  ConnNotBody(bodyResult.par),
                  SourcePosition(p.line_num, p.col_num)
                )
            )
        )

      case p: PConjunction =>
        for {
          leftResult <- normalizeMatch[M](
                         p.proc_1,
                         ProcVisitInputs(VectorPar(), input.env, input.knownFree)
                       )
          rightResult <- normalizeMatch[M](
                          p.proc_2,
                          ProcVisitInputs(VectorPar(), input.env, leftResult.knownFree)
                        )
          lp = leftResult.par
          resultConnective = lp.singleConnective() match {
            case Some(Connective(ConnAndBody(ConnectiveBody(ps)))) =>
              Connective(ConnAndBody(ConnectiveBody(ps :+ rightResult.par)))
            case _ =>
              Connective(ConnAndBody(ConnectiveBody(Vector(lp, rightResult.par))))
          }
        } yield ProcVisitOutputs(
          input.par.prepend(resultConnective, input.env.depth),
          rightResult.knownFree
            .addConnective(
              resultConnective.connectiveInstance,
              SourcePosition(p.line_num, p.col_num)
            )
        )

      case p: PDisjunction =>
        for {
          leftResult <- normalizeMatch[M](
                         p.proc_1,
                         ProcVisitInputs(VectorPar(), input.env, DeBruijnLevelMap.empty)
                       )
          rightResult <- normalizeMatch[M](
                          p.proc_2,
                          ProcVisitInputs(VectorPar(), input.env, DeBruijnLevelMap.empty)
                        )
          lp = leftResult.par
          resultConnective = lp.singleConnective() match {
            case Some(Connective(ConnOrBody(ConnectiveBody(ps)))) =>
              Connective(ConnOrBody(ConnectiveBody(ps :+ rightResult.par)))
            case _ =>
              Connective(ConnOrBody(ConnectiveBody(Vector(lp, rightResult.par))))
          }
        } yield ProcVisitOutputs(
          input.par.prepend(resultConnective, input.env.depth),
          input.knownFree
            .addConnective(
              resultConnective.connectiveInstance,
              SourcePosition(p.line_num, p.col_num)
            )
        )

      case p: PSimpleType =>
        p.simpletype_ match {
          case _: SimpleTypeBool =>
            ProcVisitOutputs(
              input.par
                .prepend(Connective(ConnBool(true)), input.env.depth)
                .withConnectiveUsed(true),
              input.knownFree
            ).pure[M]
          case _: SimpleTypeInt =>
            ProcVisitOutputs(
              input.par
                .prepend(Connective(ConnInt(true)), input.env.depth)
                .withConnectiveUsed(true),
              input.knownFree
            ).pure[M]
          case _: SimpleTypeString =>
            ProcVisitOutputs(
              input.par
                .prepend(Connective(ConnString(true)), input.env.depth)
                .withConnectiveUsed(true),
              input.knownFree
            ).pure[M]
          case _: SimpleTypeUri =>
            ProcVisitOutputs(
              input.par
                .prepend(Connective(ConnUri(true)), input.env.depth)
                .withConnectiveUsed(true),
              input.knownFree
            ).pure[M]
          case _: SimpleTypeByteArray =>
            ProcVisitOutputs(
              input.par
                .prepend(Connective(ConnByteArray(true)), input.env.depth)
                .withConnectiveUsed(true),
              input.knownFree
            ).pure[M]
        }

      case p: PGround =>
        GroundNormalizeMatcher
          .normalizeMatch[M](p.ground_)
          .map(
            expr =>
              ProcVisitOutputs(
                input.par.prepend(expr, input.env.depth),
                input.knownFree
              )
          )

      case p: PCollect =>
        CollectionNormalizeMatcher
          .normalizeMatch[M](p.collection_, CollectVisitInputs(input.env, input.knownFree))
          .map(
            collectResult =>
              ProcVisitOutputs(
                input.par.prepend(collectResult.expr, input.env.depth),
                collectResult.knownFree
              )
          )

      case p: PVar =>
        p.procvar_ match {
          case pvv: ProcVarVar =>
            input.env.get(pvv.var_) match {
              case Some(IndexContext(level, ProcSort, _)) =>
                ProcVisitOutputs(
                  input.par.prepend(EVar(BoundVar(level)), input.env.depth),
                  input.knownFree
                ).pure[M]
              case Some(IndexContext(_, NameSort, sourcePosition)) =>
                sync.raiseError(
                  UnexpectedProcContext(
                    pvv.var_,
                    sourcePosition,
                    SourcePosition(pvv.line_num, pvv.col_num)
                  )
                )
              case None =>
                input.knownFree.get(pvv.var_) match {
                  case None =>
                    val newBindingsPair =
                      input.knownFree.put(
                        (pvv.var_, ProcSort, SourcePosition(pvv.line_num, pvv.col_num))
                      )
                    ProcVisitOutputs(
                      input.par
                        .prepend(EVar(FreeVar(input.knownFree.nextLevel)), input.env.depth)
                        .withConnectiveUsed(true),
                      newBindingsPair
                    ).pure[M]
                  case Some(LevelContext(_, _, firstSourcePosition)) =>
                    sync.raiseError(
                      UnexpectedReuseOfProcContextFree(
                        pvv.var_,
                        firstSourcePosition,
                        SourcePosition(pvv.line_num, pvv.col_num)
                      )
                    )
                }
            }
          case _: ProcVarWildcard =>
            ProcVisitOutputs(
              input.par
                .prepend(EVar(Wildcard(Var.WildcardMsg())), input.env.depth)
                .withConnectiveUsed(true),
              input.knownFree.addWildcard(SourcePosition(p.line_num, p.col_num))
            ).pure[M]
        }

      case p: PVarRef =>
        input.env.find(p.var_) match {
          case None =>
            sync.raiseError(UnboundVariableRef(p.var_, p.line_num, p.col_num))
          case Some((IndexContext(idx, kind, sourcePosition), depth)) =>
            kind match {
              case ProcSort =>
                p.varrefkind_ match {
                  case _: VarRefKindProc =>
                    ProcVisitOutputs(
                      input.par
                        .prepend(Connective(VarRefBody(VarRef(idx, depth))), input.env.depth),
                      input.knownFree
                    ).pure[M]
                  case _ =>
                    sync.raiseError(
                      UnexpectedProcContext(
                        p.var_,
                        sourcePosition,
                        SourcePosition(p.line_num, p.col_num)
                      )
                    )
                }
              case NameSort =>
                p.varrefkind_ match {
                  case _: VarRefKindName =>
                    ProcVisitOutputs(
                      input.par
                        .prepend(Connective(VarRefBody(VarRef(idx, depth))), input.env.depth),
                      input.knownFree
                    ).pure[M]
                  case _ =>
                    sync.raiseError(
                      UnexpectedNameContext(
                        p.var_,
                        sourcePosition,
                        SourcePosition(p.line_num, p.col_num)
                      )
                    )
                }
            }
        }

      case _: PNil => ProcVisitOutputs(input.par, input.knownFree).pure[M]

      case p: PEval =>
        NameNormalizeMatcher
          .normalizeMatch[M](p.name_, NameVisitInputs(input.env, input.knownFree))
          .map(
            nameMatchResult =>
              ProcVisitOutputs(
                input.par ++ nameMatchResult.chan,
                nameMatchResult.knownFree
              )
          )

      case p: PMethod => {
        for {
          targetResult <- normalizeMatch[M](p.proc_, input.copy(par = Par()))
          target       = targetResult.par
          initAcc = (
            List[Par](),
            ProcVisitInputs(Par(), input.env, targetResult.knownFree),
            BitSet(),
            false
          )
          argResults <- p.listproc_.toList.reverse.foldM(initAcc)((acc, e) => {
                         normalizeMatch[M](e, acc._2).map(
                           procMatchResult =>
                             (
                               procMatchResult.par :: acc._1,
                               ProcVisitInputs(Par(), input.env, procMatchResult.knownFree),
                               acc._3 | procMatchResult.par.locallyFree,
                               acc._4 || procMatchResult.par.connectiveUsed
                             )
                         )
                       })
        } yield ProcVisitOutputs(
          input.par.prepend(
            EMethod(
              p.var_,
              targetResult.par,
              argResults._1,
              target.locallyFree | argResults._3,
              target.connectiveUsed || argResults._4
            ),
            input.env.depth
          ),
          argResults._2.knownFree
        )
      }

      case p: PNot => unaryExp(p.proc_, input, ENot.apply)
      case p: PNeg => unaryExp(p.proc_, input, ENeg.apply)

      case p: PMult           => binaryExp(p.proc_1, p.proc_2, input, EMult.apply)
      case p: PDiv            => binaryExp(p.proc_1, p.proc_2, input, EDiv.apply)
      case p: PMod            => binaryExp(p.proc_1, p.proc_2, input, EMod.apply)
      case p: PPercentPercent => binaryExp(p.proc_1, p.proc_2, input, EPercentPercent.apply)
      case p: PAdd            => binaryExp(p.proc_1, p.proc_2, input, EPlus.apply)
      case p: PMinus          => binaryExp(p.proc_1, p.proc_2, input, EMinus.apply)
      case p: PPlusPlus       => binaryExp(p.proc_1, p.proc_2, input, EPlusPlus.apply)
      case p: PMinusMinus     => binaryExp(p.proc_1, p.proc_2, input, EMinusMinus.apply)

      case p: PLt  => binaryExp(p.proc_1, p.proc_2, input, ELt.apply)
      case p: PLte => binaryExp(p.proc_1, p.proc_2, input, ELte.apply)
      case p: PGt  => binaryExp(p.proc_1, p.proc_2, input, EGt.apply)
      case p: PGte => binaryExp(p.proc_1, p.proc_2, input, EGte.apply)

      case p: PEq  => binaryExp(p.proc_1, p.proc_2, input, EEq.apply)
      case p: PNeq => binaryExp(p.proc_1, p.proc_2, input, ENeq.apply)

      case p: PAnd     => binaryExp(p.proc_1, p.proc_2, input, EAnd.apply)
      case p: POr      => binaryExp(p.proc_1, p.proc_2, input, EOr.apply)
      case p: PMatches =>
        // In case of 'matches' expression the free variables from the pattern are thrown away
        // and only the ones from the target are used.
        // This is because the "target matches pattern" should have the same semantics as
        // "match target { pattern => true ; _ => false}
        // so free variables from pattern should not be visible at the top level

        for {
          leftResult <- normalizeMatch[M](p.proc_1, input.copy(par = VectorPar()))
          rightResult <- normalizeMatch[M](
                          p.proc_2,
                          ProcVisitInputs(
                            VectorPar(),
                            input.env.push,
                            DeBruijnLevelMap.empty
                          )
                        )
        } yield ProcVisitOutputs(
          input.par.prepend(EMatches(leftResult.par, rightResult.par), input.env.depth),
          leftResult.knownFree
        )
      case p: PExprs =>
        normalizeMatch[M](p.proc_, input)

      case p: PSend =>
        for {
          nameMatchResult <- NameNormalizeMatcher.normalizeMatch[M](
                              p.name_,
                              NameVisitInputs(input.env, input.knownFree)
                            )
          initAcc = (
            Vector[Par](),
            ProcVisitInputs(VectorPar(), input.env, nameMatchResult.knownFree),
            BitSet(),
            false
          )
          dataResults <- p.listproc_.toList.reverse.foldM(initAcc)(
                          (acc, e) => {
                            normalizeMatch[M](e, acc._2).map(
                              procMatchResult =>
                                (
                                  procMatchResult.par +: acc._1,
                                  ProcVisitInputs(
                                    VectorPar(),
                                    input.env,
                                    procMatchResult.knownFree
                                  ),
                                  acc._3 | procMatchResult.par.locallyFree,
                                  acc._4 || procMatchResult.par.connectiveUsed
                                )
                            )
                          }
                        )
          persistent = p.send_ match {
            case _: SendSingle   => false
            case _: SendMultiple => true
          }
        } yield ProcVisitOutputs(
          input.par.prepend(
            Send(
              nameMatchResult.chan,
              dataResults._1,
              persistent,
              ParLocallyFree
                .locallyFree(nameMatchResult.chan, input.env.depth) | dataResults._3,
              ParLocallyFree.connectiveUsed(nameMatchResult.chan) || dataResults._4
            )
          ),
          dataResults._2.knownFree
        )

      case p: PSendSynch =>
        val identifier = UUID.randomUUID().toString
        val nameVar    = new NameVar(identifier)

        val send: PSend = {
          p.listproc_.prepend(new PEval(nameVar))
          new PSend(p.name_, new SendSingle(), p.listproc_)
        }

        val receive: PInput = {

          val listName = new ListName()
          listName.add(new NameWildcard)

          val listLinearBind = new ListLinearBind()
          listLinearBind.add(new LinearBindImpl(listName, new NameRemainderEmpty, nameVar))

          new PInput(
            new ReceiptLinear(new LinearSimple(listLinearBind)),
            p.synchsendcont_ match {
              case _: EmptyCont               => new PNil()
              case nonEmptyCont: NonEmptyCont => nonEmptyCont.proc_
            }
          )
        }

        val listName = new ListNameDecl()
        listName.add(new NameDeclSimpl(identifier))
        normalizeMatch[M](new PNew(listName, new PPar(send, receive)), input)

      case p: PContr => {
        // A free variable can only be used once in any of the parameters.
        // And we start with the empty free variable map because these free
        // variables aren't free in the surrounding context: they're binders
        for {
          nameMatchResult <- NameNormalizeMatcher
                              .normalizeMatch[M](
                                p.name_,
                                NameVisitInputs(input.env, input.knownFree)
                              )
          initAcc = (Vector[Par](), DeBruijnLevelMap.empty[VarSort], BitSet())
          // Note that we go over these in the order they were given and reverse
          // down below. This is because it makes more sense to number the free
          // variables in the order given, rather than in reverse.
          formalsResults <- p.listname_.toList.foldM(initAcc)(
                             (acc, n: Name) => {
                               NameNormalizeMatcher
                                 .normalizeMatch[M](
                                   n,
                                   NameVisitInputs(input.env.push, acc._2)
                                 )
                                 .flatMap { res =>
                                   failOnInvalidConnective(input.env.depth, res)
                                     .fold(
                                       err => Sync[M].raiseError[NameVisitOutputs](err),
                                       _.pure[M]
                                     )
                                 }
                                 .map(
                                   result =>
                                     (
                                       result.chan +: acc._1,
                                       result.knownFree,
                                       acc._3 | ParLocallyFree
                                         .locallyFree(result.chan, input.env.depth + 1)
                                     )
                                 )
                             }
                           )
          remainderResult <- RemainderNormalizeMatcher
                              .normalizeMatchName[M](p.nameremainder_, formalsResults._2)
          newEnv     = input.env.absorbFree(remainderResult._2)
          boundCount = remainderResult._2.countNoWildcards
          bodyResult <- ProcNormalizeMatcher.normalizeMatch[M](
                         p.proc_,
                         ProcVisitInputs(VectorPar(), newEnv, nameMatchResult.knownFree)
                       )
        } yield ProcVisitOutputs(
          input.par.prepend(
            Receive(
              binds = List(
                ReceiveBind(
                  formalsResults._1.reverse,
                  nameMatchResult.chan,
                  remainderResult._1,
                  boundCount
                )
              ),
              body = bodyResult.par,
              persistent = true,
              peek = false,
              bindCount = boundCount,
              locallyFree = ParLocallyFree
                .locallyFree(nameMatchResult.chan, input.env.depth) | formalsResults._3
                | (bodyResult.par.locallyFree
                  .from(boundCount)
                  .map(x => x - boundCount)),
              connectiveUsed = ParLocallyFree
                .connectiveUsed(nameMatchResult.chan) || bodyResult.par.connectiveUsed
            )
          ),
          bodyResult.knownFree
        )
      }

      case p: PInput => {
        // To handle the most common case where we can sort the binds because
        // they're from different sources, Each channel's list of patterns starts its free variables at 0.
        // We check for overlap at the end after sorting. We could check before, but it'd be an extra step.

        // We split this into parts. First we process all the sources, then we process all the bindings.
        def processSources(sources: List[(List[Name], Name, NameRemainder)]): M[
          (Vector[(List[Name], Par, NameRemainder)], DeBruijnLevelMap[VarSort], BitSet, Boolean)
        ] = {
          val initAcc =
            (Vector[(List[Name], Par, NameRemainder)](), input.knownFree, BitSet(), false)
          sources
            .foldM(initAcc)((acc, e) => {
              NameNormalizeMatcher
                .normalizeMatch[M](e._2, NameVisitInputs(input.env, acc._2))
                .map(
                  sourceResult =>
                    (
                      (e._1, sourceResult.chan, e._3) +: acc._1,
                      sourceResult.knownFree,
                      acc._3 | ParLocallyFree.locallyFree(sourceResult.chan, input.env.depth),
                      acc._4 || ParLocallyFree.connectiveUsed(sourceResult.chan)
                    )
                )
            })
            .map(foldResult => (foldResult._1.reverse, foldResult._2, foldResult._3, foldResult._4))
        }

        def processBindings(
            bindings: Vector[(List[Name], Par, NameRemainder)]
        ): M[Vector[(Vector[Par], Par, Option[Var], DeBruijnLevelMap[VarSort], BitSet)]] =
          bindings.traverse {
            case (names: List[Name], chan: Par, nr: NameRemainder) => {
              val initAcc = (Vector[Par](), DeBruijnLevelMap.empty[VarSort], BitSet())
              names
                .foldM(initAcc)((acc, n: Name) => {
                  NameNormalizeMatcher
                    .normalizeMatch[M](n, NameVisitInputs(input.env.push, acc._2))
                    .flatMap { res =>
                      failOnInvalidConnective(input.env.depth, res)
                        .fold(err => Sync[M].raiseError[NameVisitOutputs](err), _.pure[M])
                    }
                    .map(
                      result =>
                        (
                          result.chan +: acc._1,
                          result.knownFree,
                          acc._3 | ParLocallyFree.locallyFree(result.chan, input.env.depth + 1)
                        )
                    )
                })
                .flatMap {
                  case (patterns, knownFree, locallyFree) =>
                    RemainderNormalizeMatcher
                      .normalizeMatchName[M](nr, knownFree)
                      .map(
                        remainderResult =>
                          (
                            patterns.reverse,
                            chan,
                            remainderResult._1,
                            remainderResult._2,
                            locallyFree
                          )
                      )
                }
            }
          }

        val resM = p.receipt_ match {
          case rl: ReceiptLinear =>
            rl.receiptlinearimpl_ match {
              case ls: LinearSimple =>
                ls.listlinearbind_.toList
                  .traverse {
                    case lbi: LinearBindImpl =>
                      (lbi.listname_.toList, lbi.name_, lbi.nameremainder_).pure[M]
                  }
                  .map(x => (x, false, false))
            }
          case rl: ReceiptRepeated =>
            rl.receiptrepeatedimpl_ match {
              case ls: RepeatedSimple =>
                ls.listrepeatedbind_.toList
                  .traverse {
                    case lbi: RepeatedBindImpl =>
                      (lbi.listname_.toList, lbi.name_, lbi.nameremainder_).pure[M]
                  }
                  .map(x => (x, true, false))
            }
          case rl: ReceiptPeek =>
            rl.receiptpeekimpl_ match {
              case ls: PeekSimple =>
                ls.listpeekbind_.toList
                  .traverse {
                    case lbi: PeekBindImpl =>
                      (lbi.listname_.toList, lbi.name_, lbi.nameremainder_).pure[M]
                  }
                  .map(x => (x, false, true))
              case default =>
                sync.raiseError(NormalizerError(s"Unknown receipt impl type $default"))
            }
          case default => sync.raiseError(NormalizerError(s"Unknown receipt type $default"))
        }

        for {
          res                                                              <- resM
          (bindingsRaw, persistent, peek)                                  = res
          sourcesP                                                         <- processSources(bindingsRaw)
          (sources, thisLevelFree, sourcesLocallyFree, sourcesConnectives) = sourcesP
          bindingsProcessed                                                <- processBindings(sources)
          bindingsFree                                                     = bindingsProcessed.map(binding => binding._5).foldLeft(BitSet())(_ | _)
          bindingsTrimmed                                                  = bindingsProcessed.map(b => (b._1, b._2, b._3, b._4))
          receipts <- ReceiveBindsSortMatcher
                       .preSortBinds[M, VarSort](bindingsTrimmed)
          // Check if receive contains the same channels
          channels        = receipts.map(_._1.source)
          hasSameChannels = channels.size > channels.toSet.size
          _ <- ReceiveOnSameChannelsError(p.line_num, p.col_num)
                .raiseError[M, Unit]
                .whenA(hasSameChannels)
          mergedFrees <- receipts.toList
                          .foldM[M, DeBruijnLevelMap[VarSort]](DeBruijnLevelMap.empty)(
                            (env, receipt) =>
                              env.merge(receipt._2) match {
                                case (newEnv, Nil) => (newEnv: DeBruijnLevelMap[VarSort]).pure[M]
                                case (_, (shadowingVar, sourcePosition) :: _) =>
                                  val Some(LevelContext(_, _, firstSourcePosition)) =
                                    env.get(shadowingVar)
                                  sync.raiseError(
                                    UnexpectedReuseOfNameContextFree(
                                      shadowingVar,
                                      firstSourcePosition,
                                      sourcePosition
                                    )
                                  )
                              }
                          )
          bindCount  = mergedFrees.countNoWildcards
          binds      = receipts.map(receipt => receipt._1)
          updatedEnv = input.env.absorbFree(mergedFrees)
          bodyResult <- normalizeMatch[M](
                         p.proc_,
                         ProcVisitInputs(VectorPar(), updatedEnv, thisLevelFree)
                       )
          connective = sourcesConnectives || bodyResult.par.connectiveUsed
        } yield ProcVisitOutputs(
          input.par.prepend(
            Receive(
              binds,
              bodyResult.par,
              persistent,
              peek,
              bindCount,
              sourcesLocallyFree | bindingsFree | (bodyResult.par.locallyFree
                .from(bindCount)
                .map(x => x - bindCount)),
              connective
            )
          ),
          bodyResult.knownFree
        )

      }

      case p: PPar =>
        sync.suspend {
          for {
            result       <- normalizeMatch[M](p.proc_1, input)
            chainedInput = input.copy(knownFree = result.knownFree, par = result.par)
            chainedRes   <- normalizeMatch[M](p.proc_2, chainedInput)
          } yield chainedRes
        }

      case p: PNew =>
        val deployIdUri   = "rho:rchain:deployId"
        val deployerIdUri = "rho:rchain:deployerId"
        // TODO: bindings within a single new shouldn't have overlapping names.
        val newTaggedBindings = p.listnamedecl_.toVector.map {
          case n: NameDeclSimpl => (None, n.var_, NameSort, n.line_num, n.col_num)
          case n: NameDeclUrn =>
            (
              Some(GroundNormalizeMatcher.stripUri(n.uriliteral_)),
              n.var_,
              NameSort,
              n.line_num,
              n.col_num
            )
        }
        // This sorts the None's first, and the uris by lexicographical order.
        // We do this here because the sorting affects the numbering of variables inside the body.
        val sortBindings       = newTaggedBindings.sortBy(row => row._1)
        val newBindings        = sortBindings.map(row => (row._2, row._3, SourcePosition(row._4, row._5)))
        val uris               = sortBindings.flatMap(row => row._1)
        val newEnv             = input.env.put(newBindings.toList)
        val newCount           = newEnv.count - input.env.count
        val requiresDeployId   = uris.contains(deployIdUri)
        val requiresDeployerId = uris.contains(deployerIdUri)

        def missingEnvElement(name: String, uri: String) =
          NormalizerError(s"`$uri` was used in rholang usage context where $name is not available.")
        if (requiresDeployId && env.get(deployIdUri).forall(_.singleDeployId().isEmpty))
          missingEnvElement("DeployId", deployIdUri).raiseError[M, ProcVisitOutputs]
        else if (requiresDeployerId && env.get(deployerIdUri).forall(_.singleDeployerId().isEmpty))
          missingEnvElement("DeployerId", deployerIdUri).raiseError[M, ProcVisitOutputs]
        else {
          normalizeMatch[M](p.proc_, ProcVisitInputs(VectorPar(), newEnv, input.knownFree)).map {
            bodyResult =>
              val resultNew = New(
                bindCount = newCount,
                p = bodyResult.par,
                uri = uris,
                injections = env,
                locallyFree = bodyResult.par.locallyFree.from(newCount).map(x => x - newCount)
              )
              ProcVisitOutputs(input.par.prepend(resultNew), bodyResult.knownFree)
          }
        }

      case b: PBundle =>
        def error(targetResult: ProcVisitOutputs): M[ProcVisitOutputs] = {
          val errMsg = {
            def at(variable: String, sourcePosition: SourcePosition): String =
              variable + " line: " + sourcePosition.row + ", column: " + sourcePosition.column
            val wildcardsPositions = targetResult.knownFree.wildcards.map(at("", _))
            val freeVarsPositions = targetResult.knownFree.levelBindings.map {
              case (n, LevelContext(_, _, sourcePosition)) => at(s"`$n`", sourcePosition)
            }
            wildcardsPositions.mkString(" Wildcards at positions: ", ", ", ".") ++
              freeVarsPositions.mkString(" Free variables at positions: ", ", ", ".")
          }
          sync.raiseError(
            UnexpectedBundleContent(
              s"Bundle's content must not have free variables or wildcards.$errMsg"
            )
          )
        }

        import BundleOps._
        for {
          targetResult <- normalizeMatch[M](b.proc_, input.copy(par = VectorPar()))
          outermostBundle = b.bundle_ match {
            case _: BundleReadWrite => Bundle(targetResult.par, writeFlag = true, readFlag = true)
            case _: BundleRead      => Bundle(targetResult.par, writeFlag = false, readFlag = true)
            case _: BundleWrite     => Bundle(targetResult.par, writeFlag = true, readFlag = false)
            case _: BundleEquiv     => Bundle(targetResult.par, writeFlag = false, readFlag = false)
          }
          res <- if (targetResult.par.connectives.nonEmpty) {
                  sync.raiseError(
                    UnexpectedBundleContent(
                      s"Illegal top level connective in bundle at position: line: ${b.line_num}, column: ${b.col_num}."
                    )
                  )
                } else if (targetResult.knownFree.wildcards.nonEmpty || targetResult.knownFree.levelBindings.nonEmpty) {
                  error(targetResult)
                } else {
                  val newBundle: Bundle = targetResult.par.singleBundle() match {
                    case Some(single) => outermostBundle.merge(single)
                    case None         => outermostBundle
                  }
                  ProcVisitOutputs(input.par.prepend(newBundle), input.knownFree).pure[M]
                }
        } yield res

      case p: PLet => {

        p.decls_ match {

          case concDeclsImpl: ConcDeclsImpl =>
            def extractNamesAndProcs(decl: Decl): (ListName, NameRemainder, ListProc) =
              decl match {
                case declImpl: DeclImpl =>
                  (declImpl.listname_, declImpl.nameremainder_, declImpl.listproc_)
              }

            val (listNames, listNameRemainders, listProcs) =
              (extractNamesAndProcs(p.decl_) :: concDeclsImpl.listconcdecl_.toList.map {
                case concDeclImpl: ConcDeclImpl => extractNamesAndProcs(concDeclImpl.decl_)
              }).unzip3

            /*
             It is not necessary to use UUIDs to achieve concurrent let declarations.
             While there is the possibility for collisions with either variables declared by the user
             or variables declared within this translation, the chances for collision are astronomically
             small (see analysis here: https://towardsdatascience.com/are-uuids-really-unique-57eb80fc2a87).
             A strictly correct approach would be one that performs a ADT rather than an AST translation, which
             was not done here due to time constraints.
             */
            val variableNames = List.fill(listNames.size)(UUID.randomUUID().toString)

            val psends = variableNames.zip(listProcs).map {
              case (variableName, listProc) =>
                new PSend(new NameVar(variableName), new SendSingle(), listProc)
            }

            val pinput = {
              val listLinearBind = new ListLinearBind()
              for (linearBind <- variableNames.zip(listNames).zip(listNameRemainders).map {
                                  case ((variableName, listName), nameRemainder) =>
                                    new LinearBindImpl(
                                      listName,
                                      nameRemainder,
                                      new NameVar(variableName)
                                    )
                                })
                listLinearBind.add(linearBind)
              new PInput(new ReceiptLinear(new LinearSimple(listLinearBind)), p.proc_)
            }

            val ppar = {
              val procs = psends :+ pinput
              procs.drop(2).foldLeft(new PPar(procs.head, procs(1))) {
                case (ppar, proc) => new PPar(ppar, proc)
              }
            }

            val pnew = {
              val listNameDecl = new ListNameDecl()
              for (nameDecl <- variableNames.map(new NameDeclSimpl(_)))
                listNameDecl.add(nameDecl)
              new PNew(listNameDecl, ppar)
            }

            normalizeMatch[M](pnew, input)

          /*
          Let processes with a single bind or with sequential binds ";" are converted into match processes rather
          than input processes, so that each sequential bind doesn't add a new unforgeable name to the tuplespace.
          The Rholang 1.1 spec defines them as the latter. Because the Rholang 1.1 spec defines let processes in terms
          of a output process in concurrent composition with an input process, the let process appears to quote the
          process on the RHS of "<-" and bind it to the pattern on LHS. For example, in

              let x <- 1 in { Nil }

          the process (value) "1" is quoted and bound to "x" as a name. There is no way to perform an AST transformation
          of sequential let into a match process and still preserve these semantics, so we have to do an ADT transformation.
           */
          case _ =>
            val newContinuation =
              p.decls_ match {
                case _: EmptyDeclImpl => p.proc_
                case linearDeclsImpl: LinearDeclsImpl =>
                  val newDecl =
                    linearDeclsImpl.listlineardecl_.head match {
                      case impl: LinearDeclImpl => impl.decl_
                    }
                  val newDecls =
                    if (linearDeclsImpl.listlineardecl_.size == 1)
                      new EmptyDeclImpl()
                    else {
                      val newListLinearDecls = new ListLinearDecl()
                      for (linearDecl <- linearDeclsImpl.listlineardecl_.tail)
                        newListLinearDecls.add(linearDecl)
                      new LinearDeclsImpl(newListLinearDecls)
                    }
                  new PLet(newDecl, newDecls, p.proc_)
              }

            def listProcToEList(
                listProc: List[Proc],
                knownFree: DeBruijnLevelMap[VarSort]
            ): M[ProcVisitOutputs] =
              listProc
                .foldM((Vector.empty[Par], knownFree, BitSet.empty, false)) {
                  case ((vectorPar, knownFree, locallyFree, connectiveUsed), proc) =>
                    ProcNormalizeMatcher
                      .normalizeMatch[M](proc, ProcVisitInputs(VectorPar(), input.env, knownFree))
                      .map {
                        case ProcVisitOutputs(par, updatedKnownFree) =>
                          (
                            par +: vectorPar,
                            updatedKnownFree,
                            locallyFree | par.locallyFree,
                            connectiveUsed | par.connectiveUsed
                          )
                      }
                }
                .map {
                  case (vectorPar, knownFree, locallyFree, connectiveUsed) =>
                    ProcVisitOutputs(
                      EList(vectorPar.reverse, locallyFree, connectiveUsed, none[Var]),
                      knownFree
                    )
                }

            // Largely similar to how consume patterns are processed.
            def listNameToEList(
                listName: List[Name],
                nameRemainder: NameRemainder
            ): M[ProcVisitOutputs] =
              RemainderNormalizeMatcher
                .normalizeMatchName(nameRemainder, DeBruijnLevelMap.empty[VarSort]) >>= {
                case (optionalVar, remainderKnownFree) =>
                  listName
                    .foldM((Vector.empty[Par], remainderKnownFree, BitSet.empty)) {
                      case ((vectorPar, knownFree, locallyFree), name) =>
                        NameNormalizeMatcher
                          .normalizeMatch[M](name, NameVisitInputs(input.env.push, knownFree))
                          .map {
                            case NameVisitOutputs(par, updatedKnownFree) =>
                              (
                                par +: vectorPar,
                                updatedKnownFree,
                                // Use input.env.depth + 1 because the pattern was evaluated w.r.t input.env.push,
                                // and more generally because locally free variables become binders in the pattern position
                                locallyFree | ParLocallyFree.locallyFree(par, input.env.depth + 1)
                              )
                          }
                    }
                    .map {
                      case (vectorPar, knownFree, locallyFree) =>
                        ProcVisitOutputs(
                          EList(vectorPar.reverse, locallyFree, connectiveUsed = true, optionalVar),
                          knownFree
                        )
                    }
              }

            p.decl_ match {
              case declImpl: DeclImpl =>
                listProcToEList(declImpl.listproc_.toList, input.knownFree) >>= {
                  case ProcVisitOutputs(valueListPar, valueKnownFree) =>
                    listNameToEList(declImpl.listname_.toList, declImpl.nameremainder_) >>= {
                      case ProcVisitOutputs(patternListPar, patternKnownFree) =>
                        normalizeMatch[M](
                          newContinuation,
                          ProcVisitInputs(
                            VectorPar(),
                            input.env.absorbFree(patternKnownFree),
                            valueKnownFree
                          )
                        ).map {
                          case ProcVisitOutputs(continuationPar, continuationKnownFree) =>
                            ProcVisitOutputs(
                              input.par.prepend(
                                Match(
                                  target = valueListPar,
                                  cases = Seq(
                                    MatchCase(
                                      patternListPar,
                                      continuationPar,
                                      patternKnownFree.countNoWildcards
                                    )
                                  ),
                                  locallyFree = valueListPar.locallyFree | patternListPar.locallyFree | continuationPar.locallyFree
                                    .from(patternKnownFree.countNoWildcards)
                                    .map(_ - patternKnownFree.countNoWildcards),
                                  connectiveUsed = valueListPar.connectiveUsed || continuationPar.connectiveUsed
                                )
                              ),
                              continuationKnownFree
                            )
                        }
                    }
                }
            }
        }
      }

      case p: PMatch => {

        def liftCase(c: Case): M[(Proc, Proc)] = c match {
          case ci: CaseImpl => Applicative[M].pure[(Proc, Proc)]((ci.proc_1, ci.proc_2))
          case _ =>
            sync.raiseError(UnrecognizedNormalizerError("Unexpected Case implementation."))
        }

        for {
          targetResult <- normalizeMatch[M](p.proc_, input.copy(par = VectorPar()))
          cases        <- p.listcase_.toList.traverse(liftCase)

          initAcc = (Vector[MatchCase](), targetResult.knownFree, BitSet(), false)
          casesResult <- cases.foldM(initAcc)(
                          (acc, caseImpl) =>
                            caseImpl match {
                              case (pattern, caseBody) => {
                                for {
                                  patternResult <- normalizeMatch[M](
                                                    pattern,
                                                    ProcVisitInputs(
                                                      VectorPar(),
                                                      input.env.push,
                                                      DeBruijnLevelMap.empty
                                                    )
                                                  )
                                  caseEnv    = input.env.absorbFree(patternResult.knownFree)
                                  boundCount = patternResult.knownFree.countNoWildcards
                                  caseBodyResult <- normalizeMatch[M](
                                                     caseBody,
                                                     ProcVisitInputs(VectorPar(), caseEnv, acc._2)
                                                   )
                                } yield (
                                  MatchCase(patternResult.par, caseBodyResult.par, boundCount) +: acc._1,
                                  caseBodyResult.knownFree,
                                  acc._3 | patternResult.par.locallyFree | caseBodyResult.par.locallyFree
                                    .from(boundCount)
                                    .map(x => x - boundCount),
                                  acc._4 || caseBodyResult.par.connectiveUsed
                                )
                              }
                            }
                        )
        } yield ProcVisitOutputs(
          input.par.prepend(
            Match(
              targetResult.par,
              casesResult._1.reverse,
              casesResult._3 | targetResult.par.locallyFree,
              casesResult._4 || targetResult.par.connectiveUsed
            )
          ),
          casesResult._2
        )
      }

      case p: PIf =>
        normalizeIfElse(p.proc_1, p.proc_2, new PNil(), input.copy(par = VectorPar()))
          .map(n => n.copy(par = n.par ++ input.par))
      case p: PIfElse =>
        normalizeIfElse(p.proc_1, p.proc_2, p.proc_3, input.copy(par = VectorPar()))
          .map(n => n.copy(par = n.par ++ input.par))

      case _ =>
        sync.raiseError(UnrecognizedNormalizerError("Compilation of construct not yet supported."))
    }
  }

}

/** Input data to the normalizer
  *
  * @param par collection of things that might be run in parallel
  * @param env
  * @param knownFree
  */
final case class ProcVisitInputs(
    par: Par,
    env: IndexMapChain[VarSort],
    knownFree: DeBruijnLevelMap[VarSort]
)
// Returns the update Par and an updated map of free variables.
final case class ProcVisitOutputs(par: Par, knownFree: DeBruijnLevelMap[VarSort])

final case class NameVisitInputs(env: IndexMapChain[VarSort], knownFree: DeBruijnLevelMap[VarSort])
final case class NameVisitOutputs(chan: Par, knownFree: DeBruijnLevelMap[VarSort])

final case class CollectVisitInputs(
    env: IndexMapChain[VarSort],
    knownFree: DeBruijnLevelMap[VarSort]
)
final case class CollectVisitOutputs(expr: Expr, knownFree: DeBruijnLevelMap[VarSort])
