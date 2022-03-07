package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.{
  AlwaysEqual,
  EList,
  ETuple,
  Expr,
  HasLocallyFree,
  Par,
  ParMap,
  ParSet,
  Var
}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{KeyValuePair => AbsynKeyValuePair, _}
import coop.rchain.rholang.interpreter.compiler._
import monix.eval.Coeval

import scala.collection.convert.ImplicitConversionsToScala._
import scala.collection.immutable.{BitSet, Vector}

object CollectionNormalizeMatcher {
  def normalizeMatch[F[_]: Sync](c: Collection, input: CollectVisitInputs)(
      implicit env: Map[String, Par]
  ): F[CollectVisitOutputs] = {
    def foldMatch[T](
        knownFree: FreeMap[VarSort],
        listproc: List[Proc],
        constructor: (Seq[Par], AlwaysEqual[BitSet], Boolean) => T
    )(implicit toExpr: T => Expr): F[CollectVisitOutputs] = {
      val init = (Vector[Par](), knownFree, BitSet(), false)
      listproc
        .foldM(init) { (acc, proc) =>
          ProcNormalizeMatcher
            .normalizeMatch[F](proc, ProcVisitInputs(VectorPar(), input.boundMapChain, acc._2))
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
    ): F[CollectVisitOutputs] = {
      val init = (Vector[(Par, Par)](), knownFree, BitSet(), false)
      listProc
        .foldM(init) { (acc, e) =>
          e match {
            case e: KeyValuePairImpl =>
              for {
                keyResult <- ProcNormalizeMatcher.normalizeMatch[F](
                              e.proc_1,
                              ProcVisitInputs(VectorPar(), input.boundMapChain, acc._2)
                            )
                valResult <- ProcNormalizeMatcher.normalizeMatch[F](
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
            remainder.map(HasLocallyFree[Var].locallyFree(_, depth = 0)).getOrElse(BitSet())

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
          .normalizeMatchProc[F](cl.procremainder_, input.freeMap)
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
          .normalizeMatchProc[F](cs.procremainder_, input.freeMap)
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
          .normalizeMatchProc[F](cm.procremainder_, input.freeMap)
          .flatMap {
            case (optionalRemainder, knownFree) =>
              foldMatchMap(knownFree, optionalRemainder, cm.listkeyvaluepair_.toList)
          }
    }
  }
}
