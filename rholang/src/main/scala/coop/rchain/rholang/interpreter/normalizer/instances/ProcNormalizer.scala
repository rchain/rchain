package coop.rchain.rholang.interpreter.normalizer.instances

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits.{ParExtension, VectorPar}
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.errors.UnrecognizedNormalizerError
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.rholang.interpreter.normalizer.instances.Proc._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.Visit._
trait ProcNormalizer
    extends PBundleInstance
    with PCollectInstance
    with PConjunctionInstance
    with PContrInstance
    with PDisjunctionInstance
    with PEvalInstance
    with PGroundInstance
    with PIfInstance
    with PInputInstance
    with PMatchesInstance
    with PMatchInstance
    with PMethodInstance
    with PNegationInstance
    with PNewInstance
    with PParInstance
    with PSendInstance
    with PSimpleTypeInstance
    with PVarInstance
    with PVarRefInstance
    with NameInstance {
  implicit def procInstance[F[_]: Sync]
      : Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(p: Proc, input: ProcVisitInputs[Par])(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs[Par]] = Sync[F].defer {
        def unaryExp[T](subProc: Proc, input: ProcVisitInputs[Par], constructor: Par => T)(
            implicit toExprInstance: T => Expr
        ): F[ProcVisitOutputs[Par]] =
          normalize(subProc, input.copy(par = VectorPar()))
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
            input: ProcVisitInputs[Par],
            constructor: (Par, Par) => T
        )(implicit toExprInstance: T => Expr): F[ProcVisitOutputs[Par]] =
          for {
            leftResult <- normalize(subProcLeft, input.copy(par = VectorPar()))
            rightResult <- normalize(
                            subProcRight,
                            input.copy(par = VectorPar(), knownFree = leftResult.knownFree)
                          )
          } yield ProcVisitOutputs(
            input.par.prepend(constructor(leftResult.par, rightResult.par), input.env.depth),
            rightResult.knownFree
          )

        p match {
          case p: PNegation =>
            Normalizer[F, PNegation, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PConjunction =>
            Normalizer[F, PConjunction, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PDisjunction =>
            Normalizer[F, PDisjunction, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PSimpleType =>
            Normalizer[F, PSimpleType, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PGround =>
            Normalizer[F, PGround, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PCollect =>
            Normalizer[F, PCollect, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PVar =>
            Normalizer[F, PVar, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PVarRef =>
            Normalizer[F, PVarRef, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case _: PNil => ProcVisitOutputs(input.par, input.knownFree).pure[F]

          case p: PEval =>
            Normalizer[F, PEval, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PMethod =>
            Normalizer[F, PMethod, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

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

          case p: PAnd => binaryExp(p.proc_1, p.proc_2, input, EAnd.apply)
          case p: POr  => binaryExp(p.proc_1, p.proc_2, input, EOr.apply)
          case p: PMatches =>
            Normalizer[F, PMatches, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PExprs => normalize(p.proc_, input)

          case p: PSend =>
            Normalizer[F, PSend, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PContr =>
            Normalizer[F, PContr, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PInput =>
            Normalizer[F, PInput, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PPar =>
            Normalizer[F, PPar, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PNew =>
            Normalizer[F, PNew, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case b: PBundle =>
            Normalizer[F, PBundle, ProcVisitInputs, ProcVisitOutputs, Par].normalize(b, input)

          case p: PMatch =>
            Normalizer[F, PMatch, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case p: PIf =>
            Normalizer[F, PIf, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)
          case p: PIfElse =>
            Normalizer[F, PIfElse, ProcVisitInputs, ProcVisitOutputs, Par].normalize(p, input)

          case _ =>
            Sync[F].raiseError(
              UnrecognizedNormalizerError("Compilation of construct not yet supported.")
            )
        }
      }
    }
}
