package coop.rchain.rholang.interpreter

import java.io.{Reader, StringReader}

import cats.effect.Sync
import cats.implicits._
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits.VectorPar
import coop.rchain.models.rholang.sorter.Sortable
import coop.rchain.rholang.interpreter.errors._
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc

trait ParBuilder[F[_]] {
  def buildNormalizedTerm(source: String): F[Par]

  def buildNormalizedTerm(reader: Reader): F[Par]

  def buildPar(proc: Proc): F[Par]
}

object ParBuilder {

  def apply[F[_]](implicit parBuilder: ParBuilder[F]): ParBuilder[F] = parBuilder

  implicit def parBuilder[F[_]](implicit F: Sync[F]): ParBuilder[F] = new ParBuilder[F] {
    def buildNormalizedTerm(source: String): F[Par] =
      buildNormalizedTerm(new StringReader(source))

    def buildNormalizedTerm(reader: Reader): F[Par] =
      for {
        proc <- buildAST(reader)
        par  <- buildPar(proc)
      } yield par

    def buildPar(proc: Proc): F[Par] =
      for {
        par       <- normalizeTerm(proc)
        sortedPar <- Sortable[Par].sortMatch(par)
      } yield sortedPar.term

    private def buildAST(reader: Reader): F[Proc] =
      for {
        lexer  <- lexer(reader)
        parser <- parser(lexer)
        proc <- F.delay(parser.pProc()).adaptError {
                 case ex: Exception if ex.getMessage.startsWith("Syntax error") =>
                   SyntaxError(ex.getMessage)
                 case er: Error
                     if er.getMessage.startsWith("Unterminated string at EOF, beginning at") =>
                   LexerError(er.getMessage)
                 case er: Error if er.getMessage.startsWith("Illegal Character") =>
                   LexerError(er.getMessage)
                 case er: Error if er.getMessage.startsWith("Unterminated string on line") =>
                   LexerError(er.getMessage)
                 case th: Throwable => UnrecognizedInterpreterError(th)
               }
      } yield proc

    private def normalizeTerm(term: Proc): F[Par] =
      ProcNormalizeMatcher
        .normalizeMatch[F](
          term,
          ProcVisitInputs(VectorPar(), IndexMapChain[VarSort](), DebruijnLevelMap[VarSort]())
        )
        .flatMap { normalizedTerm =>
          if (normalizedTerm.knownFree.count > 0) {
            if (normalizedTerm.knownFree.wildcards.isEmpty && normalizedTerm.knownFree.logicalConnectives.isEmpty) {
              val topLevelFreeList = normalizedTerm.knownFree.env.map {
                case (name, (_, _, line, col)) => s"$name at $line:$col"
              }
              F.raiseError(
                TopLevelFreeVariablesNotAllowedError(topLevelFreeList.mkString("", ", ", ""))
              )
            } else if (normalizedTerm.knownFree.logicalConnectives.nonEmpty) {
              def connectiveInstanceToString(conn: ConnectiveInstance): String =
                if (conn.isConnAndBody) "/\\ (conjunction)"
                else if (conn.isConnOrBody) "\\/ (disjunction)"
                else if (conn.isConnNotBody) "~ (negation)"
                else conn.toString

              val connectives = normalizedTerm.knownFree.logicalConnectives
                .map {
                  case (connType, line, col) =>
                    s"${connectiveInstanceToString(connType)} at $line:$col"
                }
                .mkString("", ", ", "")
              F.raiseError(TopLevelLogicalConnectivesNotAllowedError(connectives))
            } else {
              val topLevelWildcardList = normalizedTerm.knownFree.wildcards.map {
                case (line, col) => s"_ (wildcard) at $line:$col"
              }
              F.raiseError(
                TopLevelWildcardsNotAllowedError(topLevelWildcardList.mkString("", ", ", ""))
              )
            }
          } else normalizedTerm.pure[F].map(_.par)
        }

    /**
      * @note In lieu of a purely functional wrapper around the lexer and parser
      *       [[F.adaptError]] is used as a catch-all for errors thrown in their
      *       constructors.
      */
    private def lexer(fileReader: Reader): F[Yylex] =
      F.delay(new Yylex(fileReader)).adaptError {
        case th: Throwable => LexerError("Lexer construction error: " + th.getMessage)
      }

    private def parser(lexer: Yylex): F[parser] =
      F.delay(new parser(lexer, lexer.getSymbolFactory)).adaptError {
        case th: Throwable => ParserError("Parser construction error: " + th.getMessage)
      }
  }

}
