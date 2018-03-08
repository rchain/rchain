package coop.rchain.rholang.interpreter

import java.io.{FileNotFoundException, FileReader}

import coop.rchain.models.Par
import coop.rchain.rholang.syntax.rholang_mercury.{parser, Yylex}
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import org.rogach.scallop.ScallopConf

object RholangCLI {
  class Conf(arguments: Seq[String]) extends ScallopConf(arguments) {
    val file = trailArg[String]()
    verify()
  }

  def reader(fileName: String): FileReader = new FileReader(fileName)
  def lexer(fileReader: FileReader): Yylex = new Yylex(fileReader)
  def parser(lexer: Yylex): parser         = new parser(lexer, lexer.getSymbolFactory())

  def buildAST(fileName: String): Option[Proc] =
    try {
      val rdr  = reader(fileName)
      val lxr  = lexer(rdr)
      val prsr = parser(lxr)
      Some(prsr.pProc())
    } catch {
      case e: FileNotFoundException => {
        System.err.println(s"""Error: File not found: ${fileName}""")
        None
      }
      case t: Throwable => {
        System.err.println(s"""Error while compiling: ${fileName}\n${t}""")
        None
      }
    }

  def main(args: Array[String]): Unit = {
    val conf             = new Conf(args)
    val fileName: String = conf.file()
    buildAST(fileName) match {
      case Some(term) => {
        val inputs =
          ProcVisitInputs(Par(), DebruijnLevelMap[VarSort](), DebruijnLevelMap[VarSort]())
        val normalizedTerm   = ProcNormalizeMatcher.normalizeMatch(term, inputs)
        val sortedTerm       = ParSortMatcher.sortMatch(Some(normalizedTerm.par)).term.get
        val compiledFileName = fileName.replaceAll(".rho$", "") + ".rhoc"
        new java.io.PrintWriter(compiledFileName) {
          write(sortedTerm.toString); close
        }
        println(s"Compiled $fileName to $compiledFileName")
      }
      case None => System.exit(1)
    }
  }
}
