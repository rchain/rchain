package coop.rchain.rholang.interpreter.normalizer.instances

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholang.implicits._
import coop.rchain.models._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.Visit.{ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.interpreter.errors.UnrecognizedNormalizerError
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.rholang.interpreter.normalizer.instances.ProcSourceNormalizer.{
  Position,
  SourcePar
}

trait ProcSourceNormalizer extends ProcNormalizer with ProcSourceConvert {
  implicit def convertParToSourcePar[F[_]: Sync, S](
      implicit
      procInstance: Normalizer[F, S, ProcVisitInputs, ProcVisitOutputs, Par],
      s: (S, ProcVisitOutputs[Par]) => ProcVisitOutputs[SourcePar]
  ): Normalizer[F, S, ProcVisitInputs, ProcVisitOutputs, SourcePar] =
    new Normalizer[F, S, ProcVisitInputs, ProcVisitOutputs, SourcePar] {
      override def normalize(p: S, input: ProcVisitInputs[SourcePar])(
          implicit env: Map[String, SourcePar]
      ): F[ProcVisitOutputs[SourcePar]] =
        procInstance
          .normalize(p, ProcVisitInputs[Par](input.par.par, input.env, input.knownFree))(
            env.mapValues(s => s.par)
          )
          .map(pv => s(p, pv))
    }

  implicit def procSourceNormalizer[F[_]: Sync]
      : Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, SourcePar] =
    new Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, SourcePar] {
      override def normalize(p: Proc, input: ProcVisitInputs[SourcePar])(
          implicit env: Map[String, SourcePar]
      ): F[ProcVisitOutputs[SourcePar]] = {

        def unaryExp[T](
            subProc: Proc,
            input: ProcVisitInputs[SourcePar],
            constructor: SourcePar => T
        )(
            implicit toExprInstance: T => Expr
        ): F[ProcVisitOutputs[SourcePar]] =
          normalize(subProc, input.copy(par = SourcePar(Position.empty, VectorPar())))
            .map(
              subResult =>
                ProcVisitOutputs(
                  SourcePar(
                    input.par.position,
                    input.par.par.prepend(constructor(subResult.par), input.env.depth)
                  ),
                  subResult.knownFree
                )
            )

        def binaryExp[T](
            subProcLeft: Proc,
            subProcRight: Proc,
            input: ProcVisitInputs[SourcePar],
            constructor: (SourcePar, SourcePar) => T
        )(implicit toExprInstance: T => Expr): F[ProcVisitOutputs[SourcePar]] =
          for {
            leftResult <- normalize(
                           subProcLeft,
                           input.copy(par = SourcePar(Position.empty, VectorPar()))
                         )
            rightResult <- normalize(
                            subProcRight,
                            input.copy(
                              par = SourcePar(Position.empty, VectorPar()),
                              knownFree = leftResult.knownFree
                            )
                          )
          } yield ProcVisitOutputs(
            SourcePar(
              input.par.position,
              input.par.par.prepend(constructor(leftResult.par, rightResult.par), input.env.depth)
            ),
            rightResult.knownFree
          )

        p match {
          case p: PNegation =>
            Normalizer[F, PNegation, ProcVisitInputs, ProcVisitOutputs, SourcePar]
              .normalize(p, input)

          case p: PConjunction =>
            Normalizer[F, PConjunction, ProcVisitInputs, ProcVisitOutputs, SourcePar]
              .normalize(p, input)

          case p: PDisjunction =>
            Normalizer[F, PDisjunction, ProcVisitInputs, ProcVisitOutputs, SourcePar]
              .normalize(p, input)

          case p: PSimpleType =>
            Normalizer[F, PSimpleType, ProcVisitInputs, ProcVisitOutputs, SourcePar]
              .normalize(p, input)

          case p: PGround =>
            Normalizer[F, PGround, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case p: PCollect =>
            Normalizer[F, PCollect, ProcVisitInputs, ProcVisitOutputs, SourcePar]
              .normalize(p, input)

          case p: PVar =>
            Normalizer[F, PVar, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case p: PVarRef =>
            Normalizer[F, PVarRef, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case _: PNil => ProcVisitOutputs(input.par, input.knownFree).pure[F]

          case p: PEval =>
            Normalizer[F, PEval, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case p: PMethod =>
            Normalizer[F, PMethod, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case p: PNot => unaryExp(p.proc_, input, p => ENot.apply(p.par))
          case p: PNeg => unaryExp(p.proc_, input, p => ENeg.apply(p.par))

          case p: PMult =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EMult.apply(p1.par, p2.par))
          case p: PDiv =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EDiv.apply(p1.par, p2.par))
          case p: PMod =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EMod.apply(p1.par, p2.par))
          case p: PPercentPercent =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EPercentPercent.apply(p1.par, p2.par))
          case p: PAdd =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EPlus.apply(p1.par, p2.par))
          case p: PMinus =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EMinus.apply(p1.par, p2.par))
          case p: PPlusPlus =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EPlusPlus.apply(p1.par, p2.par))
          case p: PMinusMinus =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EMinusMinus.apply(p1.par, p2.par))

          case p: PLt => binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => ELt.apply(p1.par, p2.par))
          case p: PLte =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => ELte.apply(p1.par, p2.par))
          case p: PGt => binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EGt.apply(p1.par, p2.par))
          case p: PGte =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EGte.apply(p1.par, p2.par))

          case p: PEq => binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EEq.apply(p1.par, p2.par))
          case p: PNeq =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => ENeq.apply(p1.par, p2.par))

          case p: PAnd =>
            binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EAnd.apply(p1.par, p2.par))
          case p: POr => binaryExp(p.proc_1, p.proc_2, input, (p1, p2) => EOr.apply(p1.par, p2.par))
          case p: PMatches =>
            Normalizer[F, PMatches, ProcVisitInputs, ProcVisitOutputs, SourcePar]
              .normalize(p, input)

          case p: PExprs => normalize(p.proc_, input)

          case p: PSend =>
            Normalizer[F, PSend, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case p: PContr =>
            Normalizer[F, PContr, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case p: PInput =>
            Normalizer[F, PInput, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case p: PPar =>
            Normalizer[F, PPar, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case p: PNew =>
            Normalizer[F, PNew, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case b: PBundle =>
            Normalizer[F, PBundle, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(b, input)

          case p: PMatch =>
            Normalizer[F, PMatch, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case p: PIf =>
            Normalizer[F, PIf, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)
          case p: PIfElse =>
            Normalizer[F, PIfElse, ProcVisitInputs, ProcVisitOutputs, SourcePar].normalize(p, input)

          case _ =>
            Sync[F].raiseError(
              UnrecognizedNormalizerError("Compilation of construct not yet supported.")
            )
        }
      }
    }
}

trait ProcSourceConvert {

  implicit def convert[P](source: P, out: ProcVisitOutputs[Par])(
      implicit c: (P, Par) => SourcePar
  ): ProcVisitOutputs[SourcePar] = ProcVisitOutputs(c(source, out.par), out.knownFree)

  implicit def PBundleConvert(proc: PBundle, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PCollectConvert(proc: PCollect, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PConjunctionConvert(proc: PConjunction, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PContrConvert(proc: PContr, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PDisjunctionConvert(proc: PDisjunction, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PEvalConvert(proc: PEval, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PGroundConvert(proc: PGround, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PIfConvert(proc: PIf, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PIfElseConvert(proc: PIfElse, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PInputConvert(proc: PInput, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PMatchConvert(proc: PMatch, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PMatchesConvert(proc: PMatches, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PNegationConvert(proc: PNegation, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PNewConvert(proc: PNew, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PParConvert(proc: PPar, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PSendConvert(proc: PSend, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PSimpleTypeConvert(proc: PSimpleType, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PVarConvert(proc: PVar, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PVarRefConvert(proc: PVarRef, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)

  implicit def PMethodConvert(proc: PMethod, par: Par): SourcePar =
    SourcePar(Position(proc.line_num, proc.col_num, proc.offset), par)
}

object ProcSourceNormalizer {

  final case class Position(row: Int, column: Int, offset: Int)
  object Position {
    def empty: Position = Position(0, 0, 0)
  }
  final case class SourcePar(position: Position, par: Par)

}
