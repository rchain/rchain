package coop.rchain.rholang.interpreter.compiler.normalizer

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.Send
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{NameVisitInputs, ProcVisitInputs, ProcVisitOutputs}
import scala.collection.convert.ImplicitConversionsToScala._

import coop.rchain.rholang.ast.rholang_mercury.Absyn.{PSend, SendMultiple, SendSingle}

import scala.collection.immutable.{BitSet, Vector}

object PSendNormalizer {
  def normalize[F[_]: Sync](p: PSend, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] =
    for {
      nameMatchResult <- NameNormalizeMatcher.normalizeMatch[F](
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
                        normalizeMatch[F](e, acc._2).map(
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
}
