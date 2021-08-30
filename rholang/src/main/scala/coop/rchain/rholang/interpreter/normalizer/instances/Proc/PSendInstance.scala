package coop.rchain.rholang.interpreter.normalizer.instances.Proc
import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.{Par, Send}
import coop.rchain.models.rholang.implicits.{ParExtension, ParLocallyFree, VectorPar}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{Name, PSend, Proc, SendMultiple, SendSingle}
import coop.rchain.rholang.interpreter.compiler.{
  NameVisitInputs,
  NameVisitOutputs,
  ProcVisitInputs,
  ProcVisitOutputs
}
import coop.rchain.rholang.interpreter.normalizer.Normalizer

import scala.collection.convert.ImplicitConversionsToScala._
import scala.collection.immutable.{BitSet, Vector}

trait PSendInstance {
  implicit def PSendInstance[F[_]: Sync]
      : Normalizer[F, PSend, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PSend, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: PSend, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] =
        for {
          nameMatchResult <- Normalizer[F, Name, NameVisitInputs, NameVisitOutputs, Par].normalize(
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
                            Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                              .normalize(e, acc._2)
                              .map(
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
}
