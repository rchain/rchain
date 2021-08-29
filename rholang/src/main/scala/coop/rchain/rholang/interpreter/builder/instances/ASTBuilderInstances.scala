package coop.rchain.rholang.interpreter.builder.instances

import cats.effect.Sync
import cats.syntax.all._
import cats.instances
import coop.rchain.rholang.ast.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.ast.rholang_mercury.Yylex
import coop.rchain.rholang.interpreter.builder.ASTBuilder
import coop.rchain.rholang.interpreter.compiler.ErrorHandlingParser
import coop.rchain.rholang.interpreter.errors.{
  LexerError,
  ParserError,
  SyntaxError,
  UnrecognizedInterpreterError
}

import java.io.{Reader, StringReader}

trait ASTBuilderInstances {
  implicit def stringReaderInstance[F[_]: Sync] = new ASTBuilder[F, Reader, Proc] {
    override def build(source: Reader): F[Proc] =
      for {
        lexer  <- lexer(source)
        parser <- parser(lexer)
        proc <- Sync[F].delay(parser.pProc()).adaptError {
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

    private def lexer(fileReader: Reader): F[Yylex] =
      Sync[F].delay(new Yylex(fileReader)).adaptError {
        case th: Throwable => LexerError("Lexer construction error: " + th.getMessage)
      }

    private def parser(lexer: Yylex): F[ErrorHandlingParser] =
      Sync[F].delay(new ErrorHandlingParser(lexer, lexer.getSymbolFactory)).adaptError {
        case th: Throwable => ParserError("Parser construction error: " + th.getMessage)
      }
  }

  implicit def stringInstance[F[_]: Sync] = new ASTBuilder[F, String, Proc] {
    override def build(source: String): F[Proc] =
      stringReaderInstance.build(new StringReader(source))
  }
}
