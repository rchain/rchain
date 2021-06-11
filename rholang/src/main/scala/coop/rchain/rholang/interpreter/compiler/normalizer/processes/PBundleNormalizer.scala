package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.syntax.all._
import cats.effect.Sync
import coop.rchain.models.{BundleOps, Par}
import coop.rchain.models.rholang.implicits._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{
  LevelContext,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition
}
import coop.rchain.rholang.interpreter.errors.UnexpectedBundleContent
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  BundleEquiv,
  BundleRead,
  BundleReadWrite,
  BundleWrite,
  PBundle
}
import coop.rchain.models.Bundle

object PBundleNormalizer {
  def normalize[F[_]: Sync](b: PBundle, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] = {
    def error(targetResult: ProcVisitOutputs): F[ProcVisitOutputs] = {
      val errMsg = {
        def at(variable: String, sourcePosition: SourcePosition): String =
          variable + " line: " + sourcePosition.row + ", column: " + sourcePosition.column
        val wildcardsPositions = targetResult.knownFree.wildcards.map(at("", _))
        val freeVarsPositions = targetResult.knownFree.levelBindings.map {
          case (n, LevelContext(_, _, sourcePosition)) => at(s"`$n`", sourcePosition)
        }
        wildcardsPositions.mkString(" Wildcards at positions: ", ", ", ".") ++
          freeVarsPositions.mkString(" Free variables at positions: ", ", ", ".")
      }
      Sync[F].raiseError(
        UnexpectedBundleContent(
          s"Bundle's content must not have free variables or wildcards.$errMsg"
        )
      )
    }

    import BundleOps._
    for {
      targetResult <- normalizeMatch[F](b.proc_, input.copy(par = VectorPar()))
      outermostBundle = b.bundle_ match {
        case _: BundleReadWrite => Bundle(targetResult.par, writeFlag = true, readFlag = true)
        case _: BundleRead      => Bundle(targetResult.par, writeFlag = false, readFlag = true)
        case _: BundleWrite     => Bundle(targetResult.par, writeFlag = true, readFlag = false)
        case _: BundleEquiv     => Bundle(targetResult.par, writeFlag = false, readFlag = false)
      }
      res <- if (targetResult.par.connectives.nonEmpty) {
              Sync[F].raiseError(
                UnexpectedBundleContent(
                  s"Illegal top level connective in bundle at position: line: ${b.line_num}, column: ${b.col_num}."
                )
              )
            } else if (targetResult.knownFree.wildcards.nonEmpty || targetResult.knownFree.levelBindings.nonEmpty) {
              error(targetResult)
            } else {
              val newBundle: Bundle = targetResult.par.singleBundle() match {
                case Some(single) => outermostBundle.merge(single)
                case None         => outermostBundle
              }
              ProcVisitOutputs(input.par.prepend(newBundle), input.knownFree).pure[F]
            }
    } yield res
  }
}
