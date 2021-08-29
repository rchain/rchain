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
