package coop.rchain.rholang.interpreter.normalizer.instances.Proc

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.rholang.implicits.{ParExtension, VectorPar}
import coop.rchain.models.{Bundle, BundleOps, Par}
import coop.rchain.rholang.ast.rholang_mercury.Absyn.{
  BundleEquiv,
  BundleRead,
  BundleReadWrite,
  BundleWrite,
  PBundle,
  Proc
}
import coop.rchain.rholang.interpreter.compiler.{LevelContext, SourcePosition}
import coop.rchain.rholang.interpreter.errors.UnexpectedBundleContent
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.rholang.interpreter.compiler.Visit._

trait PBundleInstance {
  implicit def PBundleInstance[F[_]: Sync]
      : Normalizer[F, PBundle, ProcVisitInputs, ProcVisitOutputs, Par] =
    new Normalizer[F, PBundle, ProcVisitInputs, ProcVisitOutputs, Par] {
      override def normalize(b: PBundle, input: ProcVisitInputs)(
          implicit env: Map[String, Par]
      ): F[ProcVisitOutputs] = {
        def error(targetResult: ProcVisitOutputs): F[ProcVisitOutputs] = {
          val errMsg = {
            def at(variable: String, sourcePosition: SourcePosition): String =
              s"$variable at $sourcePosition"
            val wildcardsPositions = targetResult.knownFree.wildcards.map(at("", _))
            val freeVarsPositions = targetResult.knownFree.levelBindings.map {
              case (n, LevelContext(_, _, sourcePosition)) => at(s"`$n`", sourcePosition)
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

        import BundleOps._
        for {
          targetResult <- Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
                           .normalize(b.proc_, input.copy(par = VectorPar()))
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

}
