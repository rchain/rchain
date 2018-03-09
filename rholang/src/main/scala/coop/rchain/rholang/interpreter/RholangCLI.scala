package coop.rchain.rholang.interpreter

import java.io.{BufferedOutputStream, FileNotFoundException, FileOutputStream, FileReader}

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
    val file   = trailArg[String](descr = "Rholang source file")
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
      case e: FileNotFoundException =>
        System.err.println(s"""Error: File not found: ${fileName}\n${e}""")
        None
      case t: Throwable =>
        System.err.println(s"""Error while compiling: ${fileName}\n${t}""")
        None
    }

  def main(args: Array[String]): Unit = {
    val conf             = new Conf(args)
    val fileName: String = conf.file()
    buildAST(fileName) match {
      case Some(term) => {
        val inputs =
          ProcVisitInputs(Par(), DebruijnLevelMap[VarSort](), DebruijnLevelMap[VarSort]())
        val normalizedTerm: ProcVisitOutputs = normalizeTerm(term, inputs)
        val sortedTerm                       = ParSortMatcher.sortMatch(Some(normalizedTerm.par)).term.get
        if (conf.binary()) {
          writeBinary(fileName, sortedTerm)
        } else {
          writeHumanReadable(fileName, sortedTerm)
        }
      }
      case None => System.exit(1)
    }
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
