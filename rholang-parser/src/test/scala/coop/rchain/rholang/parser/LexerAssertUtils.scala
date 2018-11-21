package coop.rchain.rholang.parser

import java.util

import coop.rchain.rholang.parser.lexer.RhoLexer
import coop.rchain.rholang.parser.log.Diagnostic.Kind.{ERROR, NOTE, WARN}
import coop.rchain.rholang.parser.log.{Diagnostic, DiagnosticListener}
import org.scalatest.Matchers

object LexerAssertUtils extends Matchers {

  def tokenize(content: String, listener: DiagnosticListener) =
    new RhoLexer(content, listener).readAll

  def verify(diagnostics: util.List[Diagnostic]) =
    EqTo(diagnostics)

  case class EqTo(diagnostics: util.List[Diagnostic]) {
    def eqTo(builders: DiagnosticBuilder*): Unit = {

      diagnostics should have size builders.length.toLong

      for (k <- 0 until builders.length) {
        val actual = diagnostics.get(k)
        val expected = builders(k)

        // kind
        if (expected._kind != null) {
          actual.kind shouldBe expected._kind
        }

        // code
        if (expected._code != null) {
          actual.code shouldBe expected._code
        }

        // msg
        if (expected._msg != null) {
          actual.msg shouldBe expected._msg
        }

        // msgArgs
        if (expected._msgArgs != null) {
          actual.msgArgs shouldBe expected._msgArgs
        }

        // line
        if (expected._line != null) {
          actual.line shouldBe expected._line
        }

        // row
        if (expected._row != -1) {
          actual.row shouldBe expected._row
        }

        // len
        if (expected._len != -1) {
          actual.len shouldBe expected._len
        }

        // col
        if (expected._col != -1) {
          actual.col shouldBe expected._col
        }

        // offset
        if (expected._offset != -1) {
          actual.offset shouldBe expected._offset
        }
      }
    }
  }

  def note(code: String) =
    new DiagnosticBuilder().kind(NOTE).code(code)

  def warn(code: String) =
    new DiagnosticBuilder().kind(WARN).code(code)

  def error(code: String) =
    new DiagnosticBuilder().kind(ERROR).code(code)

  class DiagnosticBuilder(var _kind: Diagnostic.Kind = null,
                          var _code: String = null,
                          var _msg: String = null,
                          var _msgArgs: util.List[String] = null,
                          //
                          var _line: String = null,
                          var _col: Int = -1,
                          var _len: Int = -1,
                          //
                          var _row: Int = -1,
                          var _offset: Int = -1,
                         ) {

    def kind(kind: Diagnostic.Kind) = {
      this._kind = kind
      this
    }

    def code(code: String) = {
      this._code = code
      this
    }

    def msg(msg: String) = {
      this._msg = msg
      this
    }

    def msgArgs(msgArgs: String*) = {
      import collection.JavaConverters._
      this._msgArgs = msgArgs.toList.asJava
      this
    }

    def line(line: String) = {
      this._line = line
      this
    }

    def col(col: Int) = {
      this._col = col
      this
    }

    def len(len: Int): DiagnosticBuilder = {
      this._len = len
      this
    }

    def len(str: String): DiagnosticBuilder = len(str.length)

    def row(row: Int) = {
      this._row = row
      this
    }

    def offset(offset: Int) = {
      this._offset = offset
      this
    }
  }

}
