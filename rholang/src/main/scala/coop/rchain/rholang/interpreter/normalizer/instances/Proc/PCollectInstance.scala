package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholang.implicits.VectorPar
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
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  CollectList,
  CollectMap,
  CollectSet,
  CollectTuple,
  Collection,
  KeyValuePairImpl,
  PCollect,
  Proc,
  TupleMultiple,
  TupleSingle
}
import coop.rchain.rholang.interpreter.compiler.{DeBruijnLevelMap, VarSort}
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import monix.eval.Coeval
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{KeyValuePair => AbsynKeyValuePair, _}
import coop.rchain.models.rholang.implicits._
import scala.collection.convert.ImplicitConversionsToScala._
import coop.rchain.rholang.interpreter.compiler.Visit._

import scala.collection.immutable.{BitSet, Vector}

trait PCollectInstance {
  implicit def PCollectInstance[F[_]: Sync]
      : Normalizer[F, PCollect, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PCollect, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PCollect, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        normalizeMatch(p.collection_, CollectVisitInputs(input.env, input.knownFree)).map(
          collectResult =>
            ProcVisitOutputs(
              input.par.prepend(collectResult.expr, input.env.depth),
              collectResult.knownFree
            )
        )
    }

  private def normalizeMatch[F[_]: Sync](c: Collection, input: CollectVisitInputs)(
      implicit env: Map[String, Par]
  ): F[CollectVisitOutputs] = {
    def foldMatch[T](
        knownFree: DeBruijnLevelMap[VarSort],
        listproc: List[Proc],
        constructor: (Seq[Par], AlwaysEqual[BitSet], Boolean) => T
    )(implicit toExpr: T => Expr): F[CollectVisitOutputs] = {
      val init = (Vector[Par](), knownFree, BitSet(), false)
      listproc
        .foldM(init) { (acc, proc) =>
          Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
            .normalize(proc, ProcVisitInputs(VectorPar(), input.env, acc._2))
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
    ): F[CollectVisitOutputs] = {
      val init = (Vector[(Par, Par)](), knownFree, BitSet(), false)
      listProc
        .foldM(init) { (acc, e) =>
          e match {
            case e: KeyValuePairImpl =>
              for {
                keyResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                              .normalize(
                                e.proc_1,
                                ProcVisitInputs(VectorPar(), input.env, acc._2)
                              )
                valResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                              .normalize(
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
        normalizeMatchProc[F](cl.procremainder_, input.knownFree)
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
        normalizeMatchProc[F](cs.procremainder_, input.knownFree)
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
        normalizeMatchProc[F](cm.procremainder_, input.knownFree)
          .flatMap {
            case (optionalRemainder, knownFree) =>
              foldMatchMap(knownFree, optionalRemainder, cm.listkeyvaluepair_.toList)
          }
    }
  }
}
