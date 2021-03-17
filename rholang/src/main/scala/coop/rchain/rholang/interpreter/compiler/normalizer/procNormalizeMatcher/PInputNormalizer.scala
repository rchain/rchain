package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Connective.ConnectiveInstance.{ConnNotBody, ConnOrBody}
import coop.rchain.models.{Par, Receive, Var}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{
  DeBruijnLevelMap,
  LevelContext,
  NameVisitInputs,
  NameVisitOutputs,
  ProcVisitInputs,
  ProcVisitOutputs,
  ReceiveBindsSortMatcher,
  VarSort
}
import coop.rchain.rholang.interpreter.errors.{
  InterpreterError,
  NormalizerError,
  PatternReceiveError,
  ReceiveOnSameChannelsError,
  UnexpectedReuseOfNameContextFree
}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  LinearBindImpl,
  LinearSimple,
  Name,
  NameRemainder,
  PInput,
  PeekBindImpl,
  PeekSimple,
  ReceiptLinear,
  ReceiptPeek,
  ReceiptRepeated,
  RepeatedBindImpl,
  RepeatedSimple
}

import scala.collection.convert.ImplicitConversionsToScala._
import scala.collection.immutable.{BitSet, Vector}

object PInputNormalizer {
  def normalize[F[_]: Sync](p: PInput, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] = {
    // To handle the most common case where we can sort the binds because
    // they're from different sources, Each channel's list of patterns starts its free variables at 0.
    // We check for overlap at the end after sorting. We could check before, but it'd be an extra step.

    // We split this into parts. First we process all the sources, then we process all the bindings.
    def processSources(sources: List[(List[Name], Name, NameRemainder)]): F[
      (Vector[(List[Name], Par, NameRemainder)], DeBruijnLevelMap[VarSort], BitSet, Boolean)
    ] = {
      val initAcc =
        (Vector[(List[Name], Par, NameRemainder)](), input.knownFree, BitSet(), false)
      sources
        .foldM(initAcc)((acc, e) => {
          NameNormalizeMatcher
            .normalizeMatch[F](e._2, NameVisitInputs(input.env, acc._2))
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
    ): F[Vector[(Vector[Par], Par, Option[Var], DeBruijnLevelMap[VarSort], BitSet)]] =
      bindings.traverse {
        case (names: List[Name], chan: Par, nr: NameRemainder) => {
          val initAcc = (Vector[Par](), DeBruijnLevelMap.empty[VarSort], BitSet())
          names
            .foldM(initAcc)((acc, n: Name) => {
              NameNormalizeMatcher
                .normalizeMatch[F](n, NameVisitInputs(input.env.push, acc._2))
                .flatMap { res =>
                  Utils
                    .failOnInvalidConnective(input, input.env.depth, res)
                    .fold(err => Sync[F].raiseError[NameVisitOutputs](err), _.pure[F])
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
                  .normalizeMatchName[F](nr, knownFree)
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
                  (lbi.listname_.toList, lbi.name_, lbi.nameremainder_).pure[F]
              }
              .map(x => (x, false, false))
        }
      case rl: ReceiptRepeated =>
        rl.receiptrepeatedimpl_ match {
          case ls: RepeatedSimple =>
            ls.listrepeatedbind_.toList
              .traverse {
                case lbi: RepeatedBindImpl =>
                  (lbi.listname_.toList, lbi.name_, lbi.nameremainder_).pure[F]
              }
              .map(x => (x, true, false))
        }
      case rl: ReceiptPeek =>
        rl.receiptpeekimpl_ match {
          case ls: PeekSimple =>
            ls.listpeekbind_.toList
              .traverse {
                case lbi: PeekBindImpl =>
                  (lbi.listname_.toList, lbi.name_, lbi.nameremainder_).pure[F]
              }
              .map(x => (x, false, true))
          case default =>
            Sync[F].raiseError(NormalizerError(s"Unknown receipt impl type $default"))
        }
      case default => Sync[F].raiseError(NormalizerError(s"Unknown receipt type $default"))
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
                   .preSortBinds[F, VarSort](bindingsTrimmed)
      // Check if receive contains the same channels
      channels        = receipts.map(_._1.source)
      hasSameChannels = channels.size > channels.toSet.size
      _ <- ReceiveOnSameChannelsError(p.line_num, p.col_num)
            .raiseError[F, Unit]
            .whenA(hasSameChannels)
      mergedFrees <- receipts.toList
                      .foldM[F, DeBruijnLevelMap[VarSort]](DeBruijnLevelMap.empty)(
                        (env, receipt) =>
                          env.merge(receipt._2) match {
                            case (newEnv, Nil) => (newEnv: DeBruijnLevelMap[VarSort]).pure[F]
                            case (_, (shadowingVar, sourcePosition) :: _) =>
                              val Some(LevelContext(_, _, firstSourcePosition)) =
                                env.get(shadowingVar)
                              Sync[F].raiseError(
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
      bodyResult <- normalizeMatch[F](
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
}
