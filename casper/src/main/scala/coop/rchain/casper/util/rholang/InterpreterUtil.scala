package coop.rchain.casper.util.rholang

import coop.rchain.models.Par

import coop.rchain.rholang.interpreter.RholangCLI

import java.io.StringReader
import java.nio.file.{Files, Path, Paths, StandardCopyOption}

object InterpreterUtil {

  def mkTerm(s: String): Either[Throwable, Par] =
    RholangCLI.buildNormalizedTerm(new StringReader(s))

  def copyDB(source: Path, dest: Path): Unit = {
    val srcLock = Paths.get(source.toString + "/lock.mdb")
    val srcData = Paths.get(source.toString + "/data.mdb")

    val destLock = Paths.get(dest.toString + "/lock.mdb")
    val destData = Paths.get(dest.toString + "/data.mdb")

    Files.copy(srcLock, destLock, StandardCopyOption.REPLACE_EXISTING)
    Files.copy(srcData, destData, StandardCopyOption.REPLACE_EXISTING)
  }

}
