package rholang.rosette

import java.nio.file.{Files, Path, Paths}

import coop.rchain.rho2rose.Rholang2RosetteCompiler
import org.scalatest.FunSuite

import scala.collection.JavaConverters._

class CompilerTests extends FunSuite {

  val testFiles: Iterable[Path] =
    Files.newDirectoryStream(Paths.get("tests")).asScala

  val failureTestFiles: Iterable[Path] =
    Files.newDirectoryStream(Paths.get("failure_tests")).asScala

  for (file <- testFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString) {
      assert(
        Rholang2RosetteCompiler
          .compile(file.toString)
          .map(Rholang2RosetteCompiler.serialize)
          .isDefined)
    }
  }

  for (file <- failureTestFiles if file.getFileName.toString.endsWith(".rho")) {
    test(file.toString) {
      assert(
        Rholang2RosetteCompiler
          .compile(file.toString)
          .map(Rholang2RosetteCompiler.serialize)
          .isEmpty)
    }
  }
}
