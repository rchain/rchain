package coop.rchain.rholang.interpreter.builder.instances

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits.VectorPar
import coop.rchain.models.rholang.sorter.Sortable
import coop.rchain.rholang.ast.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.interpreter.builder.{ADTBuilder, ASTBuilder}
import coop.rchain.rholang.interpreter.compiler.{
  DeBruijnLevelMap,
  IndexMapChain,
  LevelContext,
  ProcNormalizeMatcher,
  ProcVisitInputs,
  ProcVisitOutputs
}
import coop.rchain.rholang.interpreter.errors.{
  TopLevelFreeVariablesNotAllowedError,
  TopLevelLogicalConnectivesNotAllowedError,
  TopLevelWildcardsNotAllowedError
}
import coop.rchain.rholang.interpreter.normalizer.Normalizer

import java.io.{Reader, StringReader}

trait ADTBuilderInstances {

  implicit def procInstance[F[_]: Sync]: ADTBuilder[F, Proc, Par] = new ADTBuilder[F, Proc, Par] {
    override def buildWithEnv(source: Proc, normalizerEnv: Map[String, Par]): F[Par] =
      for {
        par       <- normalizeTerm(source)(normalizerEnv)
        sortedPar <- Sortable[Par].sortMatch(par)
      } yield sortedPar.term

    private def normalizeTerm(term: Proc)(implicit normalizerEnv: Map[String, Par]): F[Par] =
      Normalizer[F, Proc, ProcVisitInputs, ProcVisitOutputs, Par]
        .normalize(
          term,
          ProcVisitInputs(VectorPar(), IndexMapChain.empty, DeBruijnLevelMap.empty)
        )
        .flatMap { normalizedTerm =>
          if (normalizedTerm.knownFree.count > 0) {
            if (normalizedTerm.knownFree.wildcards.isEmpty && normalizedTerm.knownFree.connectives.isEmpty) {
              val topLevelFreeList = normalizedTerm.knownFree.levelBindings.map {
                case (name, LevelContext(_, _, sourcePosition)) => s"$name at $sourcePosition"
              }
              Sync[F].raiseError(
                TopLevelFreeVariablesNotAllowedError(topLevelFreeList.mkString(", "))
              )
            } else if (normalizedTerm.knownFree.connectives.nonEmpty) {
              def connectiveInstanceToString(conn: ConnectiveInstance): String =
                if (conn.isConnAndBody) "/\\ (conjunction)"
                else if (conn.isConnOrBody) "\\/ (disjunction)"
                else if (conn.isConnNotBody) "~ (negation)"
                else conn.toString

              val connectives = normalizedTerm.knownFree.connectives
                .map {
                  case (connType, sourcePosition) =>
                    s"${connectiveInstanceToString(connType)} at $sourcePosition"
                }
                .mkString(", ")
              Sync[F].raiseError(TopLevelLogicalConnectivesNotAllowedError(connectives))
            } else {
              val topLevelWildcardList = normalizedTerm.knownFree.wildcards.map { sourcePosition =>
                s"_ (wildcard) at $sourcePosition"
              }
              Sync[F].raiseError(
                TopLevelWildcardsNotAllowedError(topLevelWildcardList.mkString(", "))
              )
            }
          } else normalizedTerm.par.pure[F]
        }
  }

  implicit def stringParInstance[F[_]: Sync]: ADTBuilder[F, String, Par] =
    new ADTBuilder[F, String, Par] {
      override def buildWithEnv(source: String, normalizerEnv: Map[String, Par]): F[Par] =
        ASTBuilder[F, String, Proc]
          .build(source)
          .flatMap(procInstance.buildWithEnv(_, normalizerEnv))
    }

  implicit def readerParInstance[F[_]: Sync]: ADTBuilder[F, Reader, Par] =
    new ADTBuilder[F, Reader, Par] {
      override def buildWithEnv(source: Reader, normalizerEnv: Map[String, Par]): F[Par] =
        ASTBuilder[F, Reader, Proc]
          .build(source)
          .flatMap(procInstance.buildWithEnv(_, normalizerEnv))
    }

}
