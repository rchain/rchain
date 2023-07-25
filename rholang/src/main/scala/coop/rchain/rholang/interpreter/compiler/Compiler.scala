package coop.rchain.rholang.interpreter.compiler

import cats.effect.Sync
import cats.syntax.all._
import coop.rchain.models.Connective.ConnectiveInstance
import coop.rchain.models.Par
import coop.rchain.models.rholang.implicits.VectorPar
import coop.rchain.models.rholang.sorter.Sortable
import coop.rchain.rholang.ast.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.ast.rholang_mercury.{parser, Yylex}
import coop.rchain.rholang.interpreter.errors._

import java.io.{Reader, StringReader}
import coop.rchain.models.rholangN.Bindings._
import coop.rchain.models.rholangN._

trait Compiler[F[_]] {

  def sourceToADT(source: String): F[Par] =
    sourceToADT(source, Map.empty[String, Par])

  def sourceToADT(source: String, normalizerEnv: Map[String, Par]): F[Par] =
    sourceToADT(new StringReader(source), normalizerEnv)

  def sourceToADT(reader: Reader): F[Par] =
    sourceToADT(reader, Map.empty[String, Par])

  def sourceToADT(reader: Reader, normalizerEnv: Map[String, Par]): F[Par]

  def astToADT(proc: Proc): F[Par] =
    astToADT(proc, Map.empty[String, Par])

  def astToADT(proc: Proc, normalizerEnv: Map[String, Par]): F[Par]

  def sourceToAST(source: String): F[Proc] =
    sourceToAST(new StringReader(source))

  def sourceToAST(reader: Reader): F[Proc]

}

object Compiler {

  def apply[F[_]](implicit compiler: Compiler[F]): Compiler[F] = compiler

  implicit def parBuilder[F[_]](implicit F: Sync[F]): Compiler[F] = new Compiler[F] {

    def sourceToADT(reader: Reader, normalizerEnv: Map[String, Par]): F[Par] =
      for {
        proc <- sourceToAST(reader)
        par  <- astToADT(proc, normalizerEnv)
      } yield par

    def astToADT(proc: Proc, normalizerEnv: Map[String, Par]): F[Par] =
      for {
        par       <- normalizeTerm(proc)(normalizerEnv)
        sortedPar <- Sortable[Par].sortMatch(par)
      } yield sortedPar.term

    def sourceToAST(reader: Reader): F[Proc] =
      for {
        lexer  <- lexer(reader)
        parser <- parser(lexer)
        proc <- F.delay(parser.pProc()).adaptError {
                 case ex: SyntaxError =>
                   ex
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

    private def normalizeTerm(term: Proc)(implicit normalizerEnv: Map[String, Par]): F[Par] =
      ProcNormalizeMatcher
        .normalizeMatch[F](
          term,
          ProcVisitInputs(NilN(), BoundMapChain.empty, FreeMap.empty)
        )
        .flatMap { normalizedTerm =>
          if (normalizedTerm.freeMap.count > 0) {
            if (normalizedTerm.freeMap.wildcards.isEmpty && normalizedTerm.freeMap.connectives.isEmpty) {
              val topLevelFreeList = normalizedTerm.freeMap.levelBindings.map {
                case (name, FreeContext(_, _, sourcePosition)) => s"$name at $sourcePosition"
              }
              F.raiseError(
                TopLevelFreeVariablesNotAllowedError(topLevelFreeList.mkString(", "))
              )
            } else if (normalizedTerm.freeMap.connectives.nonEmpty) {
              def connectiveInstanceToString(conn: ConnectiveInstance): String =
                if (conn.isConnAndBody) "/\\ (conjunction)"
                else if (conn.isConnOrBody) "\\/ (disjunction)"
                else if (conn.isConnNotBody) "~ (negation)"
                else conn.toString

              val connectives = normalizedTerm.freeMap.connectives
                .map {
                  case (connType, sourcePosition) =>
                    s"${connectiveInstanceToString(connType)} at $sourcePosition"
                }
                .mkString(", ")
              F.raiseError(TopLevelLogicalConnectivesNotAllowedError(connectives))
            } else {
              val topLevelWildcardList = normalizedTerm.freeMap.wildcards.map { sourcePosition =>
                s"_ (wildcard) at $sourcePosition"
              }
              F.raiseError(
                TopLevelWildcardsNotAllowedError(topLevelWildcardList.mkString(", "))
              )
            }
          } else normalizedTerm.par.pure[F]
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

    private def parser(lexer: Yylex): F[ErrorHandlingParser] =
      F.delay(new ErrorHandlingParser(lexer, lexer.getSymbolFactory)).adaptError {
        case th: Throwable => ParserError("Parser construction error: " + th.getMessage)
      }
  }

}

/**
  * Signal errors to the caller rather than printing them to System.err.
  *
  * Please excuse the use of throw; we didn't design the CUP API.
  *
  * Ref Section 4. Customizing the Parser in
  * CUP User's Manual Last updated 06/2014 (v0.11b)
  * http://www2.cs.tum.edu/projects/cup/docs.php#parser
  */
@SuppressWarnings(Array("org.wartremover.warts.Throw"))
class ErrorHandlingParser(s: Yylex, sf: java_cup.runtime.SymbolFactory) extends parser(s, sf) {
  import java_cup.runtime.ComplexSymbolFactory.ComplexSymbol
  import java_cup.runtime.Symbol

  override def unrecovered_syntax_error(cur_token: Symbol): Unit =
    throw SyntaxError(
      cur_token match {
        case cs: ComplexSymbol =>
          s"syntax error(${cs.getName}): ${s
            .yytext()} at ${cs.getLeft.getLine}:${cs.getLeft.getColumn}-${cs.getRight.getLine}:${cs.getRight.getColumn}"
        case _ => cur_token.toString()
      }
    )

  /**
    *  "This method is called by the parser as soon as a syntax error
    *  is detected (but before error recovery is attempted). In the
    *  default implementation it calls: `report_error("Syntax error",
    *  null);`." -- section 4.
    *
    * The Rholang grammar has no error recovery productions, so this is
    * always immediately followed by a call to
    * `unrecovered_syntax_error`.
    */
  override def syntax_error(cur_token: Symbol): Unit = ()

  /** always followed by report_fatal_error, so noop is appropriate
    */
  override def report_error(message: String, info: Object): Unit = ()

  override def report_fatal_error(message: String, info: Object): Unit =
    throw ParserError(message + info)
}
