package coop.rchain.rholang.interpreter.compiler

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models._
import coop.rchain.models.rholangn._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.normalizer.processes._
import coop.rchain.rholang.interpreter.errors._

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
    def unaryExp(
        subProc: Proc,
        input: ProcVisitInputs,
        constructor: ParN => ExprN
    ): F[ProcVisitOutputs] =
      normalizeMatch[F](subProc, input.copy(par = NilN()))
        .map(
          subResult =>
            ProcVisitOutputs(
              input.par.combine(constructor(subResult.par)),
              subResult.freeMap
            )
        )

    def binaryExp[T](
        subProcLeft: Proc,
        subProcRight: Proc,
        input: ProcVisitInputs,
        constructor: (ParN, ParN) => ExprN
    ): F[ProcVisitOutputs] =
      for {
        leftResult <- normalizeMatch[F](subProcLeft, input.copy(par = NilN()))
        rightResult <- normalizeMatch[F](
                        subProcRight,
                        input.copy(par = NilN(), freeMap = leftResult.freeMap)
                      )
      } yield ProcVisitOutputs(
        input.par.combine(constructor(leftResult.par, rightResult.par)),
        rightResult.freeMap
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

      case _: PNil => ProcVisitOutputs(input.par, input.freeMap).pure[F]

      case p: PEval =>
        PEvalNormalizer.normalize(p, input)

      case p: PMethod =>
        PMethodNormalizer.normalize(p, input)

      case p: PNot => unaryExp(p.proc_, input, ENotN.apply)
      case p: PNeg => unaryExp(p.proc_, input, ENegN.apply)

      case p: PMult           => binaryExp(p.proc_1, p.proc_2, input, EMultN.apply)
      case p: PDiv            => binaryExp(p.proc_1, p.proc_2, input, EDivN.apply)
      case p: PMod            => binaryExp(p.proc_1, p.proc_2, input, EModN.apply)
      case p: PPercentPercent => binaryExp(p.proc_1, p.proc_2, input, EPercentPercentN.apply)
      case p: PAdd            => binaryExp(p.proc_1, p.proc_2, input, EPlusN.apply)
      case p: PMinus          => binaryExp(p.proc_1, p.proc_2, input, EMinusN.apply)
      case p: PPlusPlus       => binaryExp(p.proc_1, p.proc_2, input, EPlusPlusN.apply)
      case p: PMinusMinus     => binaryExp(p.proc_1, p.proc_2, input, EMinusMinusN.apply)

      case p: PLt  => binaryExp(p.proc_1, p.proc_2, input, ELtN.apply)
      case p: PLte => binaryExp(p.proc_1, p.proc_2, input, ELteN.apply)
      case p: PGt  => binaryExp(p.proc_1, p.proc_2, input, EGtN.apply)
      case p: PGte => binaryExp(p.proc_1, p.proc_2, input, EGteN.apply)

      case p: PEq  => binaryExp(p.proc_1, p.proc_2, input, EEqN.apply)
      case p: PNeq => binaryExp(p.proc_1, p.proc_2, input, ENeqN.apply)

      case p: PAnd      => binaryExp(p.proc_1, p.proc_2, input, EAndN.apply)
      case p: POr       => binaryExp(p.proc_1, p.proc_2, input, EOrN.apply)
      case p: PShortAnd => binaryExp(p.proc_1, p.proc_2, input, EShortAndN.apply)
      case p: PShortOr  => binaryExp(p.proc_1, p.proc_2, input, EShortOrN.apply)
      case p: PMatches  => PMatchesNormalizer.normalize(p, input)

      case p: PExprs =>
        normalizeMatch[F](p.proc_, input)

      case p: PSend =>
        PSendNormalizer.normalize(p, input)

      case p: PSendSynch =>
        PSendSynchNormalizer.normalize(p, input)

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

      case p: PLet =>
        PLetNormalizer.normalize(p, input)

      case p: PMatch =>
        PMatchNormalizer.normalize(p, input)

      case p: PIf =>
        PIfNormalizer
          .normalize(p.proc_1, p.proc_2, new PNil(), input.copy(par = NilN()))
          .map(n => n.copy(par = n.par.combine(input.par)))
      case p: PIfElse =>
        PIfNormalizer
          .normalize(p.proc_1, p.proc_2, p.proc_3, input.copy(par = NilN()))
          .map(n => n.copy(par = n.par.combine(input.par)))

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
    par: ParN,
    boundMapChain: BoundMapChain[VarSort],
    freeMap: FreeMap[VarSort]
)
// Returns the update Par and an updated map of free variables.
final case class ProcVisitOutputs(par: ParN, freeMap: FreeMap[VarSort])

final case class NameVisitInputs(boundMapChain: BoundMapChain[VarSort], freeMap: FreeMap[VarSort])
final case class NameVisitOutputs(par: ParN, freeMap: FreeMap[VarSort])

final case class CollectVisitInputs(
    boundMapChain: BoundMapChain[VarSort],
    freeMap: FreeMap[VarSort]
)
final case class CollectVisitOutputs(expr: ExprN, freeMap: FreeMap[VarSort])
