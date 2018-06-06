package coop.rchain.rholang.build

import coop.rchain.rholang.util.Files

import java.io.FileOutputStream
import java.nio.file.{Path, Paths}

import scala.io.Source

object RholangProtoBuild {
  val packageAnnotation: String = "scalapackage"

  def rhoFiles(base: Path): Seq[Path] = Files.walk(base, Files.ofType("rho"))

  def getPackageComment(source: Path): Option[String] =
    Source
      .fromFile(source.toFile)
      .getLines()
      .find(l => l.startsWith("//") && l.contains(packageAnnotation))
      .map(_.replace("//", "").replace(packageAnnotation, "").trim)

  def escape(s: String): String = "\"" + s.replace("\"", "\\") + "\""

  def createProtoArtifact(input: Path, output: Path): Unit =
    CompiledRholangSource.fromSourceFile(input.toFile) match {
      case Left(ex) => throw ex

      case Right(term) => term.writeTo(new FileOutputStream(output.toFile))
    }

  def createScalaArtifact(pkg: Option[String], name: String, proto: Path): String = {
    val pkgDeclaration = pkg.map(p => s"package $p").getOrElse("")
    val imports = List(
      "coop.rchain.rholang.build.CompiledRholangSource",
      "coop.rchain.models.Par"
    ).map(i => s"import $i")
    val definition = s"final case object $name extends CompiledRholangSource"
    val body =
      s"override val term: Par = CompiledRholangSource.fromProtoFile(${escape(proto.toFile.getAbsolutePath())})"

    s"""$pkgDeclaration
     |
     |${imports.mkString("\n")}
     |
     |$definition {
     |  $body
     |}
     |""".stripMargin
  }

  def rhoArtifacts(inputBase: Path, outputBase: Path): Seq[Path] = {
    val sources = rhoFiles(inputBase)

    val artifactsDetails = sources.map(source => {
      val name      = source.toFile.getName().replace(".rho", "")
      val pkg       = getPackageComment(source)
      val protoFile = outputBase.resolve(s"$name.proto")

      createProtoArtifact(source, protoFile)

      (pkg, name, protoFile)
    })

    val outputs = artifactsDetails.map {
      case (pkg, name, source) =>
        val output   = outputBase.resolve(s"$name.scala")
        val artifact = createScalaArtifact(pkg, name, source)

        Files.write(output, artifact)
        output
    }
    outputs
  }

  def cliUsage: String =
    """Usage:
    |  java -jar RholangProtoBuild.jar <baseDirectory> <sourceManaged>
    |  
    |  <baseDirectory> - directory to search (recursively) for rholang sources
    |  <sourceManaged> - directory to write generated artifacts to
    |""".stripMargin

  def main(args: Array[String]): Unit =
    if (args.length != 2) {
      println(cliUsage)
      throw new Exception("Bad arguments! See usage above.")
    } else {
      val Array(baseDirectory, sourceManaged) = args.map(Paths.get(_))
      val sourceManagedFile                   = sourceManaged.toFile
      if (!sourceManagedFile.exists) sourceManagedFile.mkdirs

      rhoArtifacts(baseDirectory, sourceManaged)
    }
}
