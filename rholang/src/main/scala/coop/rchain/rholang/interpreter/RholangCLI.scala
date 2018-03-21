package coop.rchain.rholang.interpreter

import java.io._

import scala.io.Source
import coop.rchain.models.Par
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import org.rogach.scallop.ScallopConf

object RholangCLI {
  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    version("Rholang Mercury 0.2")
    banner("""
             |Takes in a rholang source file and
             |outputs a normalized case class serialization for now.
             |
             |Options:
             |""".stripMargin)
    footer("\nWill add more options soon.")

    val binary = opt[Boolean](descr = "outputs binary protobuf serialization")
    val file   = trailArg[String](required = false, descr = "Rholang source file")
    verify()
  }

  def reader(fileName: String): FileReader = new FileReader(fileName)
  def lexer(fileReader: Reader): Yylex     = new Yylex(fileReader)
  def parser(lexer: Yylex): parser         = new parser(lexer, lexer.getSymbolFactory())

  def main(args: Array[String]): Unit = {
    val conf = new Conf(args)
    if (conf.file.supplied) {
      val fileName: String        = conf.file()
      val source                  = reader(fileName)
      val sortedTerm: Option[Par] = buildNormalizedTerm(source)
      if (conf.binary()) {
        writeBinary(fileName, sortedTerm.get)
      } else {
        writeHumanReadable(fileName, sortedTerm.get)
      }
    } else {
      print("> ")
      repl
    }
  }

  private def repl =
    for (ln <- Source.stdin.getLines) {
      print(buildNormalizedTerm(new StringReader(ln)).get.toString)
      print("> ")
    }

  private def buildNormalizedTerm(source: Reader) = {
    val term = buildAST(source)
    val inputs =
      ProcVisitInputs(Par(), DebruijnIndexMap[VarSort](), DebruijnLevelMap[VarSort]())
    val normalizedTerm: ProcVisitOutputs = normalizeTerm(term, inputs)
    ParSortMatcher.sortMatch(Some(normalizedTerm.par)).term
  }

  private def buildAST(source: Reader): Proc = {
    val lxr = lexer(source)
    val ast = parser(lxr)
    ast.pProc()
  }

  private def writeHumanReadable(fileName: String, sortedTerm: Par): Unit = {
    val compiledFileName = fileName.replaceAll(".rho$", "") + ".rhoc"
    new java.io.PrintWriter(compiledFileName) {
      write(sortedTerm.toString)
      close()
    }
    println(s"Compiled $fileName to $compiledFileName")
  }

  private def writeBinary(fileName: String, sortedTerm: Par): Unit = {
    val binaryFileName = fileName.replaceAll(".rho$", "") + ".bin"
    val output         = new BufferedOutputStream(new FileOutputStream(binaryFileName))
    output.write(sortedTerm.toByteString.toByteArray)
    output.close()
    println(s"Compiled $fileName to $binaryFileName")
  }

  private def normalizeTerm(term: Proc, inputs: ProcVisitInputs) = {
    val normalizedTerm = ProcNormalizeMatcher.normalizeMatch(term, inputs)
    if (normalizedTerm.par.freeCount > 0)
      throw new Error("Top-level free variables are not allowed.")
    normalizedTerm
  }
}
