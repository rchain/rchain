package coop.rchain.rholang.build

import coop.rchain.rholang.interpreter.Interpreter
import coop.rchain.models.Par

import java.io.{File, FileInputStream, FileReader}

trait CompiledRholangSource {
  val term: Par
  val code: String
}

object CompiledRholangSource {
  def fromSourceFile(file: File): Either[Throwable, Par] = {
    val reader = new FileReader(file)

    Interpreter.buildNormalizedTerm(reader).runAttempt
  }

  def fromProtoFile(path: String): Par = {
    val input = new FileInputStream(path)

    Par.parseFrom(input)
  }
}
