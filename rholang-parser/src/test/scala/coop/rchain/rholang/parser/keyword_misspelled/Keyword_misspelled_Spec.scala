package coop.rchain.rholang.parser.keyword_misspelled

import java.util.Arrays.asList

import coop.rchain.rholang.parser.LexerAssertUtils._
import coop.rchain.rholang.parser.RhoTokenType.{EOF, IDENT}
import coop.rchain.rholang.parser.log.impl.DiagnosticCollector
import org.scalatest.prop.PropertyChecks
import org.scalatest.{FlatSpec, Matchers}

class Keyword_misspelled_Spec extends FlatSpec with Matchers with PropertyChecks {

  val misspelledKeywords =
    Table(
      ("actualInput", "correctedInput"),
      ("nil", "Nil"),
      ("NIL", "Nil"),
      ("nill", "Nil"),
      ("Nill", "Nil"),
      ("NILL", "Nil"),
      ("null", "Nil"),
      ("Null", "Nil"),
      ("NULL", "Nil"),
      ("nul", "Nil"),
      ("Nul", "Nil"),
      ("NUL", "Nil"),
      //
      ("For", "for"),
      ("FOR", "FOR"),
      //
      ("New", "new"),
      ("NEW", "new"),
      //
      ("In", "in"),
      ("IN", "in"),
      //
      ("Contract", "contract"),
      ("CONTRACT", "contract"),
      //
      ("If", "if"),
      ("IF", "if"),
      //
      ("Else", "else"),
      ("ELSE", "else"),
      //
      ("True", "true"),
      ("TRUE", "true"),
      //
      ("False", "false"),
      ("FALSE", "false"),
      //
      ("Not", "not"),
      ("NOT", "not"),
      //
      ("And", "and"),
      ("AND", "and"),
      //
      ("Or", "or"),
      ("OR", "or"),
      //
      ("Match", "match"),
      ("MATCH", "match"),
      //
      ("Matches", "matches"),
      ("MATCHES", "matches"),
      //
      ("Select", "select"),
      ("SELECT", "select"),
      //
      ("bool", "Bool"),
      ("BOOL", "Bool"),
      //
      ("int", "Int"),
      ("INT", "Int"),
      //
      ("string", "String"),
      ("STRING", "String"),
      //
      ("uri", "Uri"),
      ("URI", "Uri"),
      //
      ("byteArray", "ByteArray"),
      ("Bytearray", "ByteArray"),
      ("bytearray", "ByteArray"),
      ("BYTEARRAY", "ByteArray"),
      //
      ("Byte_Array", "ByteArray"),
      ("byte_Array", "ByteArray"),
      ("Byte_array", "ByteArray"),
      ("byte_array", "ByteArray"),
      ("BYTE_ARRAY", "ByteArray"),
      //
      ("set", "Set"),
      ("SET", "Set"),
      //
      ("Bundle", "bundle"),
      ("BUNDLE", "bundle"),
      ("Bundle0", "bundle0"),
      ("BUNDLE0", "bundle0"),
    )

  forAll (misspelledKeywords) { (actualInput, correctedInput) =>
    val collector = new DiagnosticCollector
    val tokens = tokenize(actualInput, collector)

    tokens shouldBe asList(IDENT.T(actualInput), EOF.T)
    verify(collector.getDiagnostics) eqTo
      warn("lexer.warn.identifier.like-existing-keyword")
        .row(1).col(1).len(actualInput).offset(0)
        .msgArgs(actualInput, correctedInput)
  }
}
