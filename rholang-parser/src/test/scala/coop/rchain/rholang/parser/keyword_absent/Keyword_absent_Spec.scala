package coop.rchain.rholang.parser.keyword_absent

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils.{note => _note, _}
import coop.rchain.rholang.parser.RhoTokenType.{EOF, IDENT}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class Keyword_absent_Spec extends FlatSpec with Matchers with PropertyChecks {

  val x: Tuple1[String] = Tuple1("a")

  val corruptedKeywords =
    Table(
      "corrupted Keyword",
      //=====================
      "as", "of", "do", "eq",
      "As", "Of", "Do", "Eq",
      "AS", "OF", "DO", "EQ",
      //
      "url", "div", "mul", "add", "sub", "pow", "mod", "neg", "xml", "def", "let", "end", "map",
      "Url", "Div", "Mul", "Add", "Sub", "Pow", "Mod", "Neg", "Xml", "Def", "Let", "End", "Map",
      "URL", "DIV", "MUL", "ADD", "SUB", "POW", "MOD", "NEG", "XML", "DEF", "let", "END", "MAP",
      //
      "this", "self", "json", "type", "byte", "char", "case", "loop", "send", "list", "long",
      "This", "Self", "Json", "Type", "Byte", "Char", "Case", "Loop", "Send", "List", "Long",
      "THIS", "SELF", "JSON", "TYPE", "BYTE", "CHAR", "CASE", "LOOP", "SEND", "LIST", "LONG",
      //
      "class", "throw", "super", "until", "yield", "where", "begin", "break", "short", "float", "array", "while",
      "Class", "Throw", "Super", "Until", "Yield", "Where", "Begin", "Break", "Short", "Float", "Array", "While",
      "CLASS", "THROW", "SUPER", "UNTIL", "YIELD", "WHERE", "BEGIN", "BREAK", "SHORT", "FLOAT", "ARRAY", "WHILE",
      //
      "object", "public", "throws", "module", "scheme", "import", "double", "return", "forall",
      "Object", "Public", "Throws", "Module", "Scheme", "Import", "double", "Return", "Forall",
      "OBJECT", "PUBLIC", "THROWS", "MODULE", "SCHEME", "IMPORT", "double", "RETURN", "FORALL",
      //
      "private", "default", "integer", "defined", "newtype", "receive", "extends", "boolean",
      "Private", "Default", "Integer", "Defined", "Newtype", "Receive", "Extends", "Boolean",
      "PRIVATE", "DEFAULT", "INTEGER", "DEFINED", "NEWTYPE", "RECEIVE", "EXTENDS", "BOOLEAN",
      //
      "function", "property", "continue",
      "Function", "Property", "Continue",
      "FUNCTION", "PROPERTY", "CONTINUE",
      //
      "protected", "character",
      "Protected", "Character",
      "PROTECTED", "CHARACTER",
      //
      "instanceof", "implements",
      "Instanceof", "Implements",
      "INSTANCEOF", "IMPLEMENTS"
    )

  forAll (corruptedKeywords) { actualInput =>
    val collector = new DiagnosticCollector
    val tokens = tokenize(actualInput, collector)

    tokens shouldBe asList(IDENT.T(actualInput), EOF.T)
    verify(collector.getDiagnostics) eqTo
      _note("lexer.note.identifier.like-absent-keyword")
        .row(1).col(1).len(actualInput).offset(0)
        .msgArgs(actualInput)
  }
}
