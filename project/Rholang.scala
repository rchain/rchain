import sbt._
import Keys._
import sbt.io.Path.relativeTo

/*
 * There doesn't seem to be a good way to have a plugin defined in
 * one sub-project be used in another sub-project. So we need to
 * call an assembled jar instead, building it if necessary.
 */
object Rholang {

  def jarOutDated(jar: File, jarSources: File): Boolean = {
    val scalaFiles   = (jarSources ** "*.scala").get
    val latestSource = scalaFiles.maxBy(_.lastModified())

    (!jar.exists()) || (jar.lastModified() < latestSource.lastModified())
  }

  def constructArtifacts(
      jar: File,
      rhoSourceDir: File,
      srcManaged: File,
      resources: File
  ): Seq[File] = {
    import scala.sys.process._

    val command =
      s"java -jar ${jar.getAbsolutePath()} ${rhoSourceDir.getAbsolutePath()} ${srcManaged
        .getAbsolutePath()} ${resources.getAbsolutePath()}"

    val status = command.!

    if (status == 0) {
      (srcManaged ** "*.scala").get
    } else {
      throw new Exception(s"Non-zero exit status during rholang artifact creation: $status")
    }
  }

  val rholangSource             = settingKey[File]("Default Rholang source directory.")
  val rholangProtoBuildAssembly = taskKey[File]("Rholang proto build Jar")
  val rholangScalaProto =
    taskKey[Seq[File]]("Generates managed Scala sources and proto from Rholang.")

  lazy val rholangSettings = Seq(
    exportJars := true,
    rholangSource in Compile := (sourceDirectory in Compile).value / "rholang",
    rholangSource in Test := (sourceDirectory in Test).value / "rholang",
    sourceGenerators in Compile += (rholangScalaProto in Compile).taskValue,
    sourceGenerators in Test += (rholangScalaProto in Test).taskValue,
    rholangScalaProto in Compile := {
      constructArtifacts(
        rholangProtoBuildAssembly.value,
        (rholangSource in Compile).value,
        (sourceManaged in Compile).value,
        (resourceManaged in Compile).value
      )
    },
    rholangScalaProto in Test := {
      constructArtifacts(
        rholangProtoBuildAssembly.value,
        (rholangSource in Test).value,
        (sourceManaged in Test).value,
        (resourceManaged in Test).value
      )
    },
    mappings in (Compile, packageBin) ++= {
      val generatedProtos = (resourceManaged in Compile).value ** "*.proto"
      generatedProtos pair relativeTo((resourceManaged in Compile).value)
    },
    mappings in (Test, packageBin) ++= {
      val generatedProtos = (resourceManaged in Test).value ** "*.proto"
      generatedProtos pair relativeTo((resourceManaged in Test).value)
    }
  )
}
