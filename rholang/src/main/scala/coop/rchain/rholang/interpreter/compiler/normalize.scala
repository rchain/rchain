package coop.rchain.rholang.interpreter.compiler

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models._
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.interpreter.compiler.normalizer.processes._

sealed trait VarSort
case object ProcSort extends VarSort
case object NameSort extends VarSort

object ProcNormalizeMatcher {

  /**
    * Rholang normalizer entry point
    */
  def normalizeMatch[F[_]: Sync](p: Proc, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] = Sync[F].defer {
    def unaryExp[T](subProc: Proc, input: ProcVisitInputs, constructor: Par => T)(
        implicit toExprInstance: T => Expr
    ): F[ProcVisitOutputs] =
      normalizeMatch[F](subProc, input.copy(par = VectorPar()))
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
    )(implicit toExprInstance: T => Expr): F[ProcVisitOutputs] =
      for {
        leftResult <- normalizeMatch[F](subProcLeft, input.copy(par = VectorPar()))
        rightResult <- normalizeMatch[F](
                        subProcRight,
                        input.copy(par = VectorPar(), knownFree = leftResult.knownFree)
                      )
      } yield ProcVisitOutputs(
        input.par.prepend(constructor(leftResult.par, rightResult.par), input.env.depth),
        rightResult.knownFree
      )

    p match {
      case p: PNegation =>
        PNegationNormalizer.normalize(p, input)

      case p: PConjunction =>
        PConjunctionNormalizer.normalize(p, input)

      case p: PDisjunction =>
        PDisjunctionNormalizer.normalize(p, input)

      case p: PSimpleType =>
        PSimpleTypeNormalizer.normalize(p, input)

      case p: PGround =>
        PGroundNormalizer.normalize(p, input)

      case p: PCollect =>
        PCollectNormalizer.normalize(p, input)

      case p: PVar =>
        PVarNormalizer.normalize(p, input)

      case p: PVarRef =>
        PVarRefNormalizer.normalize(p, input)

      case _: PNil => ProcVisitOutputs(input.par, input.knownFree).pure[F]

      case p: PEval =>
        PEvalNormalizer.normalize(p, input)

      case p: PMethod =>
        PMethodNormalizer.normalize(p, input)

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

      case p: PAnd      => binaryExp(p.proc_1, p.proc_2, input, EAnd.apply)
      case p: POr       => binaryExp(p.proc_1, p.proc_2, input, EOr.apply)
      case p: PShortAnd => binaryExp(p.proc_1, p.proc_2, input, EShortAnd.apply)
      case p: PShortOr  => binaryExp(p.proc_1, p.proc_2, input, EShortOr.apply)
      case p: PMatches =>
        PMatchesNormalizer.normalize(p, input)

      case p: PExprs =>
        normalizeMatch[F](p.proc_, input)

      case p: PSend =>
        PSendNormalizer.normalize(p, input)

      case p: PContr =>
        PContrNormalizer.normalize(p, input)

      case p: PInput =>
        PInputNormalizer.normalize(p, input)

      case p: PPar =>
        PParNormalizer.normalize(p, input)

      case p: PNew =>
        PNewNormalizer.normalize(p, input)

      case b: PBundle =>
        PBundleNormalizer.normalize(b, input)

      case p: PMatch =>
        PMatchNormalizer.normalize(p, input)

      case p: PIf =>
        PIfNormalizer
          .normalize(p.proc_1, p.proc_2, new PNil(), input.copy(par = VectorPar()))
          .map(n => n.copy(par = n.par ++ input.par))
      case p: PIfElse =>
        PIfNormalizer
          .normalize(p.proc_1, p.proc_2, p.proc_3, input.copy(par = VectorPar()))
          .map(n => n.copy(par = n.par ++ input.par))

      case _ =>
        Sync[F].raiseError(
          UnrecognizedNormalizerError("Compilation of construct not yet supported.")
        )
    }
  }

}

/**
  * Input data to the normalizer
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
