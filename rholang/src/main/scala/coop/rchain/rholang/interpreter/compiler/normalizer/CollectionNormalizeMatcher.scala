package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._
import coop.rchain.models.{Par, Var}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{KeyValuePair => AbsynKeyValuePair, _}
import coop.rchain.rholang.interpreter.compiler._

import scala.jdk.CollectionConverters._

object CollectionNormalizeMatcher {
  def normalizeMatch[F[_]: Sync](c: Collection, input: CollectVisitInputs)(
      implicit env: Map[String, Par]
  ): F[CollectVisitOutputs] = {
    def foldMatch(
        knownFree: FreeMap[VarSort],
        listproc: List[Proc],
        constructor: Seq[ParN] => ExprN
    ): F[CollectVisitOutputs] = {
      val init = (Vector[ParN](), knownFree)
      listproc
        .foldM(init) { (acc, proc) =>
          ProcNormalizeMatcher
            .normalizeMatch[F](proc, ProcVisitInputs(NilN(), input.boundMapChain, acc._2))
            .map { result =>
              (result.par +: acc._1, result.freeMap)
            }
        }
        .map {
          case (ps, resultKnownFree) =>
            CollectVisitOutputs(
              toProtoExpr(constructor(ps.reverse)),
              resultKnownFree
            )
        }
    }

    def foldMatchMap(
        knownFree: FreeMap[VarSort],
        remainder: Option[Var],
        listProc: List[AbsynKeyValuePair]
    ): F[CollectVisitOutputs] = {
      val init = (Seq[(ParN, ParN)](), knownFree)
      listProc
        .foldM(init) { (acc, e) =>
          e match {
            case e: KeyValuePairImpl =>
              for {
                keyResult <- ProcNormalizeMatcher.normalizeMatch[F](
                              e.proc_1,
                              ProcVisitInputs(NilN(), input.boundMapChain, acc._2)
                            )
                valResult <- ProcNormalizeMatcher.normalizeMatch[F](
                              e.proc_2,
                              ProcVisitInputs(
                                NilN(),
                                input.boundMapChain,
                                keyResult.freeMap
                              )
                            )
              } yield (
                Seq((keyResult.par, valResult.par)) ++ acc._1,
                valResult.freeMap
              )
          }
        }
        .map { folded =>
          val resultKnownFree = folded._2
          CollectVisitOutputs(
            toProtoExpr(EMapN(folded._1.reverse, fromProtoVarOpt(remainder))),
            resultKnownFree
          )
        }
    }

    c match {
      case cl: CollectList =>
        RemainderNormalizeMatcher
          .normalizeMatchProc[F](cl.procremainder_, input.freeMap)
          .flatMap {
            case (optionalRemainder, knownFree) =>
              val constructor: Option[VarN] => Seq[ParN] => ExprN =
                optionalRemainder => ps => EListN(ps, optionalRemainder)
              foldMatch(knownFree, cl.listproc_.asScala.toList, constructor(optionalRemainder))
          }

      case ct: CollectTuple =>
        val ps = ct.tuple_ match {
          case ts: TupleSingle   => Seq(ts.proc_)
          case tm: TupleMultiple => Seq(tm.proc_) ++ tm.listproc_.asScala.toList
        }
        foldMatch(input.freeMap, ps.toList, ETupleN.apply)

      case cs: CollectSet =>
        RemainderNormalizeMatcher
          .normalizeMatchProc[F](cs.procremainder_, input.freeMap)
          .flatMap {
            case (optionalRemainder, knownFree) =>
              val constructor: Option[VarN] => Seq[ParN] => ExprN =
                optionalRemainder => pars => ESetN(pars, optionalRemainder)
              foldMatch(knownFree, cs.listproc_.asScala.toList, constructor(optionalRemainder))
          }

      case cm: CollectMap =>
        RemainderNormalizeMatcher
          .normalizeMatchProc[F](cm.procremainder_, input.freeMap)
          .flatMap {
            case (optionalRemainder, knownFree) =>
              foldMatchMap(
                knownFree,
                toProtoVarOpt(optionalRemainder),
                cm.listkeyvaluepair_.asScala.toList
              )
          }

    }
  }
}
