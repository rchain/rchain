package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Par
import coop.rchain.models.rholangN._
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{
  FreeContext,
  ProcVisitInputs,
  ProcVisitOutputs,
  SourcePosition
}
import coop.rchain.rholang.interpreter.errors.UnexpectedBundleContent

object PBundleNormalizer {
  def normalize[F[_]: Sync](b: PBundle, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] = {
    def error(targetResult: ProcVisitOutputs): F[ProcVisitOutputs] = {
      val errMsg = {
        def at(variable: String, sourcePosition: SourcePosition): String =
          s"$variable at $sourcePosition"
        val wildcardsPositions = targetResult.freeMap.wildcards.map(at("", _))
        val freeVarsPositions = targetResult.freeMap.levelBindings.map {
          case (n, FreeContext(_, _, sourcePosition)) => at(s"`$n`", sourcePosition)
        }
        val errMsgWildcards =
          if (wildcardsPositions.nonEmpty)
            wildcardsPositions.mkString(" Wildcards positions ", ", ", ".")
          else ""
        val errMsgFreeVars =
          if (freeVarsPositions.nonEmpty)
            freeVarsPositions.mkString(" Free variables positions ", ", ", ".")
          else ""
        errMsgWildcards + errMsgFreeVars
      }
      Sync[F].raiseError(
        UnexpectedBundleContent(
          s"Bundle's content must not have free variables or wildcards.$errMsg"
        )
      )
    }

    def connectivesExistOnTop(p: ParN): Boolean =
      p match {
        case _: ConnectiveN  => true
        case pProc: ParProcN => pProc.ps.exists(connectivesExistOnTop)
        case _               => false
      }

    for {
      targetResult <- normalizeMatch[F](b.proc_, input.copy(par = NilN()))
      target       = targetResult.par
      outermostBundle = b.bundle_ match {
        case _: BundleReadWrite => BundleN(target, writeFlag = true, readFlag = true)
        case _: BundleRead      => BundleN(target, writeFlag = false, readFlag = true)
        case _: BundleWrite     => BundleN(target, writeFlag = true, readFlag = false)
        case _: BundleEquiv     => BundleN(target, writeFlag = false, readFlag = false)
      }

      res <- if (connectivesExistOnTop(target)) {
              Sync[F].raiseError(
                UnexpectedBundleContent(
                  s"Illegal top level connective in bundle at position: line: ${b.line_num}, column: ${b.col_num}."
                )
              )
            } else if (targetResult.freeMap.wildcards.nonEmpty || targetResult.freeMap.levelBindings.nonEmpty) {
              error(targetResult)
            } else {
              val newBundle: BundleN = target match {
                case b: BundleN => outermostBundle.merge(b)
                case _          => outermostBundle
              }
              val outPar: ParN = input.par.combine(newBundle)
              ProcVisitOutputs(outPar, input.freeMap).pure[F]
            }
    } yield res
  }
}
