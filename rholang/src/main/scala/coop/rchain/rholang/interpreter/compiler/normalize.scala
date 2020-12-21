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
  def handleProcVar[M[_]](pv: ProcVar, knownFree: FreeMap[VarSort])(
      implicit sync: Sync[M]
  ): M[(Option[Var], FreeMap[VarSort])] =
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
          case Some(FreeContext(_, _, firstSourcePosition)) =>
            sync.raiseError(
              UnexpectedReuseOfProcContextFree(pvv.var_, firstSourcePosition, sourcePosition)
            )
        }
    }

  def normalizeMatchProc[M[_]](r: ProcRemainder, knownFree: FreeMap[VarSort])(
      implicit err: Sync[M]
  ): M[(Option[Var], FreeMap[VarSort])] =
    r match {
      case _: ProcRemainderEmpty => (None: Option[Var], knownFree).pure[M]
      case pr: ProcRemainderVar =>
        handleProcVar[M](pr.procvar_, knownFree)
    }

  def normalizeMatchName[M[_]](nr: NameRemainder, knownFree: FreeMap[VarSort])(
      implicit err: Sync[M]
  ): M[(Option[Var], FreeMap[VarSort])] =
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
        knownFree: FreeMap[VarSort],
        listproc: List[Proc],
        constructor: (Seq[Par], AlwaysEqual[BitSet], Boolean) => T
    )(implicit toExpr: T => Expr): M[CollectVisitOutputs] = {
      val init = (Vector[Par](), knownFree, BitSet(), false)
      listproc
        .foldM(init) { (acc, proc) =>
          ProcNormalizeMatcher
            .normalizeMatch[M](proc, ProcVisitInputs(VectorPar(), input.boundMapChain, acc._2))
            .map { result =>
              (
                result.par +: acc._1,
                result.freeMap,
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
        knownFree: FreeMap[VarSort],
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
                              ProcVisitInputs(VectorPar(), input.boundMapChain, acc._2)
                            )
                valResult <- ProcNormalizeMatcher.normalizeMatch[M](
                              e.proc_2,
                              ProcVisitInputs(VectorPar(), input.boundMapChain, keyResult.freeMap)
                            )
              } yield (
                Vector((keyResult.par, valResult.par)) ++ acc._1,
                valResult.freeMap,
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
          .normalizeMatchProc[M](cl.procremainder_, input.freeMap)
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
        foldMatch(input.freeMap, ps.toList, ETuple.apply)

      case cs: CollectSet =>
        RemainderNormalizeMatcher
          .normalizeMatchProc[M](cs.procremainder_, input.freeMap)
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
          .normalizeMatchProc[M](cm.procremainder_, input.freeMap)
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
          input.freeMap.addWildcard(SourcePosition(wc.line_num, wc.col_num))
        NameVisitOutputs(EVar(Wildcard(Var.WildcardMsg())), wildcardBindResult).pure[M]
      case n: NameVar =>
        input.boundMapChain.get(n.var_) match {
          case Some(BoundContext(level, NameSort, _)) => {
            NameVisitOutputs(EVar(BoundVar(level)), input.freeMap).pure[M]
          }
          case Some(BoundContext(_, ProcSort, sourcePosition)) => {
            err.raiseError(
              UnexpectedNameContext(n.var_, sourcePosition, SourcePosition(n.line_num, n.col_num))
            )
          }
          case None => {
            input.freeMap.get(n.var_) match {
              case None =>
                val newBindingsPair =
                  input.freeMap.put((n.var_, NameSort, SourcePosition(n.line_num, n.col_num)))
                NameVisitOutputs(EVar(FreeVar(input.freeMap.nextLevel)), newBindingsPair).pure[M]
              case Some(FreeContext(_, _, sourcePosition)) =>
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
          .normalizeMatch[M](
            n.proc_,
            ProcVisitInputs(VectorPar(), input.boundMapChain, input.freeMap)
          )
          .map(
            procVisitResult => NameVisitOutputs(procVisitResult.par, procVisitResult.freeMap)
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
              input.par.prepend(constructor(subResult.par), input.boundMapChain.depth),
              subResult.freeMap
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
                        input.copy(par = VectorPar(), freeMap = leftResult.freeMap)
                      )
      } yield ProcVisitOutputs(
        input.par.prepend(constructor(leftResult.par, rightResult.par), input.boundMapChain.depth),
        rightResult.freeMap
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
                         ProcVisitInputs(VectorPar(), input.boundMapChain, targetResult.freeMap)
                       )
        falseCaseBody <- normalizeMatch[M](
                          falseBodyProc,
                          ProcVisitInputs(VectorPar(), input.boundMapChain, trueCaseBody.freeMap)
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
      } yield ProcVisitOutputs(input.par.prepend(desugaredIf), falseCaseBody.freeMap)

    def failOnInvalidConnective(
        depth: Int,
        nameRes: NameVisitOutputs
    ): Either[InterpreterError, NameVisitOutputs] =
      if (input.boundMapChain.depth == 0) {
        Either
          .fromOption(
            nameRes.freeMap.connectives
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
          ProcVisitInputs(VectorPar(), input.boundMapChain, FreeMap.empty)
        ).map(
          bodyResult =>
            ProcVisitOutputs(
              input.par.prepend(Connective(ConnNotBody(bodyResult.par)), input.boundMapChain.depth),
              input.freeMap
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
                         ProcVisitInputs(VectorPar(), input.boundMapChain, input.freeMap)
                       )
          rightResult <- normalizeMatch[M](
                          p.proc_2,
                          ProcVisitInputs(VectorPar(), input.boundMapChain, leftResult.freeMap)
                        )
          lp = leftResult.par
          resultConnective = lp.singleConnective() match {
            case Some(Connective(ConnAndBody(ConnectiveBody(ps)))) =>
              Connective(ConnAndBody(ConnectiveBody(ps :+ rightResult.par)))
            case _ =>
              Connective(ConnAndBody(ConnectiveBody(Vector(lp, rightResult.par))))
          }
        } yield ProcVisitOutputs(
          input.par.prepend(resultConnective, input.boundMapChain.depth),
          rightResult.freeMap
            .addConnective(
              resultConnective.connectiveInstance,
              SourcePosition(p.line_num, p.col_num)
            )
        )

      case p: PDisjunction =>
        for {
          leftResult <- normalizeMatch[M](
                         p.proc_1,
                         ProcVisitInputs(VectorPar(), input.boundMapChain, FreeMap.empty)
                       )
          rightResult <- normalizeMatch[M](
                          p.proc_2,
                          ProcVisitInputs(VectorPar(), input.boundMapChain, FreeMap.empty)
                        )
          lp = leftResult.par
          resultConnective = lp.singleConnective() match {
            case Some(Connective(ConnOrBody(ConnectiveBody(ps)))) =>
              Connective(ConnOrBody(ConnectiveBody(ps :+ rightResult.par)))
            case _ =>
              Connective(ConnOrBody(ConnectiveBody(Vector(lp, rightResult.par))))
          }
        } yield ProcVisitOutputs(
          input.par.prepend(resultConnective, input.boundMapChain.depth),
          input.freeMap
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
                .prepend(Connective(ConnBool(true)), input.boundMapChain.depth)
                .withConnectiveUsed(true),
              input.freeMap
            ).pure[M]
          case _: SimpleTypeInt =>
            ProcVisitOutputs(
              input.par
                .prepend(Connective(ConnInt(true)), input.boundMapChain.depth)
                .withConnectiveUsed(true),
              input.freeMap
            ).pure[M]
          case _: SimpleTypeString =>
            ProcVisitOutputs(
              input.par
                .prepend(Connective(ConnString(true)), input.boundMapChain.depth)
                .withConnectiveUsed(true),
              input.freeMap
            ).pure[M]
          case _: SimpleTypeUri =>
            ProcVisitOutputs(
              input.par
                .prepend(Connective(ConnUri(true)), input.boundMapChain.depth)
                .withConnectiveUsed(true),
              input.freeMap
            ).pure[M]
          case _: SimpleTypeByteArray =>
            ProcVisitOutputs(
              input.par
                .prepend(Connective(ConnByteArray(true)), input.boundMapChain.depth)
                .withConnectiveUsed(true),
              input.freeMap
            ).pure[M]
        }

      case p: PGround =>
        GroundNormalizeMatcher
          .normalizeMatch[M](p.ground_)
          .map(
            expr =>
              ProcVisitOutputs(
                input.par.prepend(expr, input.boundMapChain.depth),
                input.freeMap
              )
          )

      case p: PCollect =>
        CollectionNormalizeMatcher
          .normalizeMatch[M](p.collection_, CollectVisitInputs(input.boundMapChain, input.freeMap))
          .map(
            collectResult =>
              ProcVisitOutputs(
                input.par.prepend(collectResult.expr, input.boundMapChain.depth),
                collectResult.freeMap
              )
          )

      case p: PVar =>
        p.procvar_ match {
          case pvv: ProcVarVar =>
            input.boundMapChain.get(pvv.var_) match {
              case Some(BoundContext(level, ProcSort, _)) =>
                ProcVisitOutputs(
                  input.par.prepend(EVar(BoundVar(level)), input.boundMapChain.depth),
                  input.freeMap
                ).pure[M]
              case Some(BoundContext(_, NameSort, sourcePosition)) =>
                sync.raiseError(
                  UnexpectedProcContext(
                    pvv.var_,
                    sourcePosition,
                    SourcePosition(pvv.line_num, pvv.col_num)
                  )
                )
              case None =>
                input.freeMap.get(pvv.var_) match {
                  case None =>
                    val newBindingsPair =
                      input.freeMap.put(
                        (pvv.var_, ProcSort, SourcePosition(pvv.line_num, pvv.col_num))
                      )
                    ProcVisitOutputs(
                      input.par
                        .prepend(EVar(FreeVar(input.freeMap.nextLevel)), input.boundMapChain.depth)
                        .withConnectiveUsed(true),
                      newBindingsPair
                    ).pure[M]
                  case Some(FreeContext(_, _, firstSourcePosition)) =>
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
                .prepend(EVar(Wildcard(Var.WildcardMsg())), input.boundMapChain.depth)
                .withConnectiveUsed(true),
              input.freeMap.addWildcard(SourcePosition(p.line_num, p.col_num))
            ).pure[M]
        }

      case p: PVarRef =>
        input.boundMapChain.find(p.var_) match {
          case None =>
            sync.raiseError(UnboundVariableRef(p.var_, p.line_num, p.col_num))
          case Some((BoundContext(idx, kind, sourcePosition), depth)) =>
            kind match {
              case ProcSort =>
                p.varrefkind_ match {
                  case _: VarRefKindProc =>
                    ProcVisitOutputs(
                      input.par
                        .prepend(
                          Connective(VarRefBody(VarRef(idx, depth))),
                          input.boundMapChain.depth
                        ),
                      input.freeMap
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
                        .prepend(
                          Connective(VarRefBody(VarRef(idx, depth))),
                          input.boundMapChain.depth
                        ),
                      input.freeMap
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

      case _: PNil => ProcVisitOutputs(input.par, input.freeMap).pure[M]

      case p: PEval =>
        NameNormalizeMatcher
          .normalizeMatch[M](p.name_, NameVisitInputs(input.boundMapChain, input.freeMap))
          .map(
            nameMatchResult =>
              ProcVisitOutputs(
                input.par ++ nameMatchResult.par,
                nameMatchResult.freeMap
              )
          )

      case p: PMethod => {
        for {
          targetResult <- normalizeMatch[M](p.proc_, input.copy(par = Par()))
          target       = targetResult.par
          initAcc = (
            List[Par](),
            ProcVisitInputs(Par(), input.boundMapChain, targetResult.freeMap),
            BitSet(),
            false
          )
          argResults <- p.listproc_.toList.reverse.foldM(initAcc)((acc, e) => {
                         normalizeMatch[M](e, acc._2).map(
                           procMatchResult =>
                             (
                               procMatchResult.par :: acc._1,
                               ProcVisitInputs(Par(), input.boundMapChain, procMatchResult.freeMap),
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
            input.boundMapChain.depth
          ),
          argResults._2.freeMap
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
                            input.boundMapChain.push,
                            FreeMap.empty
                          )
                        )
        } yield ProcVisitOutputs(
          input.par.prepend(EMatches(leftResult.par, rightResult.par), input.boundMapChain.depth),
          leftResult.freeMap
        )
      case p: PExprs =>
        normalizeMatch[M](p.proc_, input)

      case p: PSend =>
        for {
          nameMatchResult <- NameNormalizeMatcher.normalizeMatch[M](
                              p.name_,
                              NameVisitInputs(input.boundMapChain, input.freeMap)
                            )
          initAcc = (
            Vector[Par](),
            ProcVisitInputs(VectorPar(), input.boundMapChain, nameMatchResult.freeMap),
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
                                    input.boundMapChain,
                                    procMatchResult.freeMap
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
              nameMatchResult.par,
              dataResults._1,
              persistent,
              ParLocallyFree
                .locallyFree(nameMatchResult.par, input.boundMapChain.depth) | dataResults._3,
              ParLocallyFree.connectiveUsed(nameMatchResult.par) || dataResults._4
            )
          ),
          dataResults._2.freeMap
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

          val listReceipt = new ListReceipt()
          listReceipt.add(new ReceiptLinear(new LinearSimple(listLinearBind)))

          new PInput(
            listReceipt,
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
                                NameVisitInputs(input.boundMapChain, input.freeMap)
                              )
          initAcc = (Vector[Par](), FreeMap.empty[VarSort], BitSet())
          // Note that we go over these in the order they were given and reverse
          // down below. This is because it makes more sense to number the free
          // variables in the order given, rather than in reverse.
          formalsResults <- p.listname_.toList.foldM(initAcc)(
                             (acc, n: Name) => {
                               NameNormalizeMatcher
                                 .normalizeMatch[M](
                                   n,
                                   NameVisitInputs(input.boundMapChain.push, acc._2)
                                 )
                                 .flatMap { res =>
                                   failOnInvalidConnective(input.boundMapChain.depth, res)
                                     .fold(
                                       err => Sync[M].raiseError[NameVisitOutputs](err),
                                       _.pure[M]
                                     )
                                 }
                                 .map(
                                   result =>
                                     (
                                       result.par +: acc._1,
                                       result.freeMap,
                                       acc._3 | ParLocallyFree
                                         .locallyFree(result.par, input.boundMapChain.depth + 1)
                                     )
                                 )
                             }
                           )
          remainderResult <- RemainderNormalizeMatcher
                              .normalizeMatchName[M](p.nameremainder_, formalsResults._2)
          newEnv     = input.boundMapChain.absorbFree(remainderResult._2)
          boundCount = remainderResult._2.countNoWildcards
          bodyResult <- ProcNormalizeMatcher.normalizeMatch[M](
                         p.proc_,
                         ProcVisitInputs(VectorPar(), newEnv, nameMatchResult.freeMap)
                       )
        } yield ProcVisitOutputs(
          input.par.prepend(
            Receive(
              binds = List(
                ReceiveBind(
                  formalsResults._1.reverse,
                  nameMatchResult.par,
                  remainderResult._1,
                  boundCount
                )
              ),
              body = bodyResult.par,
              persistent = true,
              peek = false,
              bindCount = boundCount,
              locallyFree = ParLocallyFree
                .locallyFree(nameMatchResult.par, input.boundMapChain.depth) | formalsResults._3
                | (bodyResult.par.locallyFree
                  .from(boundCount)
                  .map(x => x - boundCount)),
              connectiveUsed = ParLocallyFree
                .connectiveUsed(nameMatchResult.par) || bodyResult.par.connectiveUsed
            )
          ),
          bodyResult.freeMap
        )
      }

      case p: PInput => {

        if (p.listreceipt_.size() > 1) {
          normalizeMatch[M](p.listreceipt_.reverse.foldLeft(p.proc_) { (proc, receipt) =>
            val listReceipt = new ListReceipt()
            listReceipt.add(receipt)
            new PInput(listReceipt, proc)
          }, input)
        } else {

          // To handle the most common case where we can sort the binds because
          // they're from different sources, Each channel's list of patterns starts its free variables at 0.
          // We check for overlap at the end after sorting. We could check before, but it'd be an extra step.
          // We split this into parts. First we process all the sources, then we process all the bindings.
          def processSources(
              sources: Vector[Name]
          ): M[(Vector[Par], FreeMap[VarSort], BitSet, Boolean)] =
            sources.foldM((Vector.empty[Par], input.freeMap, BitSet.empty, false)) {
              case ((vectorPar, knownFree, locallyFree, connectiveUsed), name) =>
                NameNormalizeMatcher
                  .normalizeMatch[M](name, NameVisitInputs(input.boundMapChain, knownFree))
                  .map {
                    case NameVisitOutputs(par, knownFree) =>
                      (
                        vectorPar :+ par,
                        knownFree,
                        locallyFree | ParLocallyFree.locallyFree(par, input.boundMapChain.depth),
                        connectiveUsed || ParLocallyFree.connectiveUsed(par)
                      )
                  }
            }

          def processPatterns(
              patterns: Vector[(Vector[Name], NameRemainder)]
          ): M[Vector[(Vector[Par], Option[Var], FreeMap[VarSort], BitSet)]] =
            patterns.traverse {
              case (names, nameRemainder) =>
                names
                  .foldM((Vector.empty[Par], FreeMap.empty[VarSort], BitSet.empty)) {
                    case ((vectorPar, knownFree, locallyFree), name) =>
                      NameNormalizeMatcher
                        .normalizeMatch[M](
                          name,
                          NameVisitInputs(input.boundMapChain.push, knownFree)
                        ) >>= {
                        case nameVisitOutputs @ NameVisitOutputs(par, knownFree) =>
                          failOnInvalidConnective(input.boundMapChain.depth, nameVisitOutputs)
                            .fold(
                              _.raiseError[M, (Vector[Par], FreeMap[VarSort], BitSet)],
                              _ =>
                                (
                                  vectorPar :+ par,
                                  knownFree,
                                  locallyFree | ParLocallyFree
                                    .locallyFree(par, input.boundMapChain.depth + 1)
                                ).pure[M]
                            )
                      }
                  } >>= {
                  case (vectorPar, knownFree, locallyFree) =>
                    RemainderNormalizeMatcher.normalizeMatchName(nameRemainder, knownFree).map {
                      case (optionalVar, knownFree) =>
                        (vectorPar, optionalVar, knownFree, locallyFree)
                    }
                }
            }

          // If we get to this point, we know p.listreceipt.size() == 1
          val (consumes, persistent, peek) =
            p.listreceipt_.head match {
              case rl: ReceiptLinear =>
                rl.receiptlinearimpl_ match {
                  case ls: LinearSimple =>
                    (ls.listlinearbind_.toVector.map {
                      case lbi: LinearBindImpl =>
                        ((lbi.listname_.toVector, lbi.nameremainder_), lbi.name_)
                    }, false, false)
                }
              case rr: ReceiptRepeated =>
                rr.receiptrepeatedimpl_ match {
                  case rs: RepeatedSimple =>
                    (rs.listrepeatedbind_.toVector.map {
                      case rbi: RepeatedBindImpl =>
                        ((rbi.listname_.toVector, rbi.nameremainder_), rbi.name_)
                    }, true, false)
                }
              case rp: ReceiptPeek =>
                rp.receiptpeekimpl_ match {
                  case ps: PeekSimple =>
                    (ps.listpeekbind_.toVector.map {
                      case pbi: PeekBindImpl =>
                        ((pbi.listname_.toVector, pbi.nameremainder_), pbi.name_)
                    }, false, true)
                }
            }

          val (patterns, names) = consumes.unzip

          for {
            processedSources                                                  <- processSources(names)
            (sources, sourcesFree, sourcesLocallyFree, sourcesConnectiveUsed) = processedSources
            processedPatterns                                                 <- processPatterns(patterns)
            receiveBindsAndFreeMaps <- ReceiveBindsSortMatcher.preSortBinds[M, VarSort](
                                        processedPatterns.zip(sources).map {
                                          case ((a, b, c, _), e) => (a, b, e, c)
                                        }
                                      )
            (receiveBinds, receiveBindFreeMaps) = receiveBindsAndFreeMaps.unzip
            channels                            = receiveBinds.map(_.source)
            hasSameChannels                     = channels.size > channels.toSet.size
            _ <- ReceiveOnSameChannelsError(p.line_num, p.col_num)
                  .raiseError[M, Unit]
                  .whenA(hasSameChannels)
            receiveBindsFreeMap <- receiveBindFreeMaps.toList.foldM(FreeMap.empty[VarSort]) {
                                    case (knownFree, receiveBindFreeMap) =>
                                      knownFree.merge(receiveBindFreeMap) match {
                                        case (updatedKnownFree, Nil) => updatedKnownFree.pure[M]
                                        case (_, (shadowingVar, sourcePosition) :: _) =>
                                          UnexpectedReuseOfNameContextFree(
                                            shadowingVar,
                                            knownFree.get(shadowingVar).get.sourcePosition,
                                            sourcePosition
                                          ).raiseError[M, FreeMap[VarSort]]
                                      }
                                  }
            procVisitOutputs <- normalizeMatch[M](
                                 p.proc_,
                                 ProcVisitInputs(
                                   VectorPar(),
                                   input.boundMapChain.absorbFree(receiveBindsFreeMap),
                                   sourcesFree
                                 )
                               )
          } yield {
            val bindCount = receiveBindsFreeMap.countNoWildcards
            ProcVisitOutputs(
              input.par.prepend(
                Receive(
                  receiveBinds,
                  procVisitOutputs.par,
                  persistent,
                  peek,
                  bindCount,
                  sourcesLocallyFree | processedPatterns
                    .map(_._4)
                    .fold(BitSet.empty)(_ | _) | procVisitOutputs.par.locallyFree
                    .from(bindCount)
                    .map(_ - bindCount),
                  sourcesConnectiveUsed || procVisitOutputs.par.connectiveUsed
                )
              ),
              procVisitOutputs.freeMap
            )
          }
        }
      }

      case p: PPar =>
        sync.suspend {
          for {
            result       <- normalizeMatch[M](p.proc_1, input)
            chainedInput = input.copy(freeMap = result.freeMap, par = result.par)
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
        val newEnv             = input.boundMapChain.put(newBindings.toList)
        val newCount           = newEnv.count - input.boundMapChain.count
        val requiresDeployId   = uris.contains(deployIdUri)
        val requiresDeployerId = uris.contains(deployerIdUri)

        def missingEnvElement(name: String, uri: String) =
          NormalizerError(s"`$uri` was used in rholang usage context where $name is not available.")
        if (requiresDeployId && env.get(deployIdUri).forall(_.singleDeployId().isEmpty))
          missingEnvElement("DeployId", deployIdUri).raiseError[M, ProcVisitOutputs]
        else if (requiresDeployerId && env.get(deployerIdUri).forall(_.singleDeployerId().isEmpty))
          missingEnvElement("DeployerId", deployerIdUri).raiseError[M, ProcVisitOutputs]
        else {
          normalizeMatch[M](p.proc_, ProcVisitInputs(VectorPar(), newEnv, input.freeMap)).map {
            bodyResult =>
              val resultNew = New(
                bindCount = newCount,
                p = bodyResult.par,
                uri = uris,
                injections = env,
                locallyFree = bodyResult.par.locallyFree.from(newCount).map(x => x - newCount)
              )
              ProcVisitOutputs(input.par.prepend(resultNew), bodyResult.freeMap)
          }
        }

      case b: PBundle =>
        def error(targetResult: ProcVisitOutputs): M[ProcVisitOutputs] = {
          val errMsg = {
            def at(variable: String, sourcePosition: SourcePosition): String =
              variable + " line: " + sourcePosition.row + ", column: " + sourcePosition.column
            val wildcardsPositions = targetResult.freeMap.wildcards.map(at("", _))
            val freeVarsPositions = targetResult.freeMap.levelBindings.map {
              case (n, FreeContext(_, _, sourcePosition)) => at(s"`$n`", sourcePosition)
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
                } else if (targetResult.freeMap.wildcards.nonEmpty || targetResult.freeMap.levelBindings.nonEmpty) {
                  error(targetResult)
                } else {
                  val newBundle: Bundle = targetResult.par.singleBundle() match {
                    case Some(single) => outermostBundle.merge(single)
                    case None         => outermostBundle
                  }
                  ProcVisitOutputs(input.par.prepend(newBundle), input.freeMap).pure[M]
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
              val listReceipt = new ListReceipt()
              listReceipt.add(new ReceiptLinear(new LinearSimple(listLinearBind)))
              new PInput(listReceipt, p.proc_)
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
                knownFree: FreeMap[VarSort]
            ): M[ProcVisitOutputs] =
              listProc
                .foldM((Vector.empty[Par], knownFree, BitSet.empty, false)) {
                  case ((vectorPar, knownFree, locallyFree, connectiveUsed), proc) =>
                    ProcNormalizeMatcher
                      .normalizeMatch[M](
                        proc,
                        ProcVisitInputs(VectorPar(), input.boundMapChain, knownFree)
                      )
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
                .normalizeMatchName(nameRemainder, FreeMap.empty[VarSort]) >>= {
                case (optionalVar, remainderKnownFree) =>
                  listName
                    .foldM((Vector.empty[Par], remainderKnownFree, BitSet.empty)) {
                      case ((vectorPar, knownFree, locallyFree), name) =>
                        NameNormalizeMatcher
                          .normalizeMatch[M](
                            name,
                            NameVisitInputs(input.boundMapChain.push, knownFree)
                          )
                          .map {
                            case NameVisitOutputs(par, updatedKnownFree) =>
                              (
                                par +: vectorPar,
                                updatedKnownFree,
                                // Use input.env.depth + 1 because the pattern was evaluated w.r.t input.env.push,
                                // and more generally because locally free variables become binders in the pattern position
                                locallyFree | ParLocallyFree
                                  .locallyFree(par, input.boundMapChain.depth + 1)
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
                listProcToEList(declImpl.listproc_.toList, input.freeMap) >>= {
                  case ProcVisitOutputs(valueListPar, valueKnownFree) =>
                    listNameToEList(declImpl.listname_.toList, declImpl.nameremainder_) >>= {
                      case ProcVisitOutputs(patternListPar, patternKnownFree) =>
                        normalizeMatch[M](
                          newContinuation,
                          ProcVisitInputs(
                            VectorPar(),
                            input.boundMapChain.absorbFree(patternKnownFree),
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

          initAcc = (Vector[MatchCase](), targetResult.freeMap, BitSet(), false)
          casesResult <- cases.foldM(initAcc)(
                          (acc, caseImpl) =>
                            caseImpl match {
                              case (pattern, caseBody) => {
                                for {
                                  patternResult <- normalizeMatch[M](
                                                    pattern,
                                                    ProcVisitInputs(
                                                      VectorPar(),
                                                      input.boundMapChain.push,
                                                      FreeMap.empty
                                                    )
                                                  )
                                  caseEnv    = input.boundMapChain.absorbFree(patternResult.freeMap)
                                  boundCount = patternResult.freeMap.countNoWildcards
                                  caseBodyResult <- normalizeMatch[M](
                                                     caseBody,
                                                     ProcVisitInputs(VectorPar(), caseEnv, acc._2)
                                                   )
                                } yield (
                                  MatchCase(patternResult.par, caseBodyResult.par, boundCount) +: acc._1,
                                  caseBodyResult.freeMap,
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

final case class ProcVisitInputs(
    par: Par,
    boundMapChain: BoundMapChain[VarSort],
    freeMap: FreeMap[VarSort]
)
// Returns the update Par and an updated map of free variables.
final case class ProcVisitOutputs(par: Par, freeMap: FreeMap[VarSort])

final case class NameVisitInputs(boundMapChain: BoundMapChain[VarSort], freeMap: FreeMap[VarSort])
final case class NameVisitOutputs(par: Par, freeMap: FreeMap[VarSort])

final case class CollectVisitInputs(
    boundMapChain: BoundMapChain[VarSort],
    freeMap: FreeMap[VarSort]
)
final case class CollectVisitOutputs(expr: Expr, freeMap: FreeMap[VarSort])
