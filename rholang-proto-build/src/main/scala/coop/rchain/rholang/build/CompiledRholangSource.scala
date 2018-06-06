package coop.rchain.rholang.build

import coop.rchain.rholang.interpreter.{errors, RholangCLI}
import coop.rchain.models.Par

import java.io.{File, FileInputStream, FileReader}

trait CompiledRholangSource {
  val term: Par
}

object CompiledRholangSource {
  def fromSourceFile(file: File): Either[Throwable, Par] = {
    val reader = new FileReader(file)

    try {
      RholangCLI.buildNormalizedTerm(reader).runAttempt
    } catch {
      case errors.UnrecognizedInterpreterError(err) => Left(err)
      case other: Throwable                         => Left(other)
    }
  }

  def fromProtoFile(path: String): Par = {
    val input = new FileInputStream(path)

    Par.parseFrom(input)
  }
}
