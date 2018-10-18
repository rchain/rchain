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
      case Left(ex) =>
        println(s"Rholang compilation failed for file $input")
        throw ex

      case Right(term) => term.writeTo(new FileOutputStream(output.toFile))
    }

  def escapeSourceCode(source: Path): String =
    Source.fromFile(source.toFile).getLines().mkString("\"\"\"", "\n|", "\"\"\".stripMargin")

  def createScalaArtifact(pkg: Option[String], name: String, proto: Path, source: Path): String = {
    val pkgDeclaration = pkg.map(p => s"package $p").getOrElse("")
    val imports = List(
      "coop.rchain.casper.util.rholang.InterpreterUtil",
      "coop.rchain.rholang.build.CompiledRholangSource",
      "coop.rchain.models.Par"
    ).map(i => s"import $i")
    val definition = s"final case object $name extends CompiledRholangSource"
    val protoName  = "/" + proto.toFile.getName

    val body =
      s"""val resource = getClass.getResourceAsStream(${escape(protoName)})
         |  override val term: Par = InterpreterUtil.mkTerm(${escapeSourceCode(source)}).right.get
         |  override val code: String = ${escapeSourceCode(source)}""".stripMargin

    s"""$pkgDeclaration
     |
     |${imports.mkString("\n")}
     |
     |$definition {
     |  $body
     |}
     |""".stripMargin
  }

  def rhoArtifacts(inputBase: Path, srcManaged: Path, resources: Path): Seq[Path] = {
    val sources = rhoFiles(inputBase)

    val artifactsDetails = sources.map(source => {
      val name      = source.toFile.getName().replace(".rho", "")
      val pkg       = getPackageComment(source)
      val protoFile = resources.resolve(s"$name.proto")

      createProtoArtifact(source, protoFile)

      (pkg, name, protoFile, source)
    })

    val outputs = artifactsDetails.map {
      case (pkg, name, protoFile, sourceFile) =>
        val output   = srcManaged.resolve(s"$name.scala")
        val artifact = createScalaArtifact(pkg, name, protoFile, sourceFile)

        Files.write(output, artifact)
        output
    }
    outputs
  }

  def cliUsage: String =
    """Usage:
    |  java -jar RholangProtoBuild.jar <baseDirectory> <sourceManaged> <resourceManaged>
    |  
    |  <baseDirectory> - directory to search (recursively) for rholang sources
    |  <sourceManaged> - directory to write generated scala code to
    |  <resourceManaged>     - directory to write protobuf outputs to
    |""".stripMargin

  def main(args: Array[String]): Unit =
    if (args.length != 3) {
      println(cliUsage)
      throw new Exception("Bad arguments! See usage above.")
    } else {
      val Array(baseDirectory, sourceManaged, resources) = args.map(Paths.get(_))
      val sourceManagedFile                              = sourceManaged.toFile
      val resourcesFile                                  = resources.toFile
      if (!sourceManagedFile.exists) sourceManagedFile.mkdirs
      if (!resourcesFile.exists) resourcesFile.mkdirs

      rhoArtifacts(baseDirectory, sourceManaged, resources)
    }
}
