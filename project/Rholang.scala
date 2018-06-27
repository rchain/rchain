import sbt._
import Keys._

/*
 * There doesn't seem to be a good way to have a plugin defined in 
 * one sub-project be used in another sub-project. So we need to
 * call an assembled jar instead, building it if necessary.
 */
object Rholang {

  def jarOutDated(jar: File, jarSources: File): Boolean = {
    val scalaFiles = (jarSources ** "*.scala").get
    val latestSource = scalaFiles.maxBy(_.lastModified())
    
    (!jar.exists()) || (jar.lastModified() < latestSource.lastModified())
  }

  def constructArtifacts(jar: File, rhoSourceDir: File, outputDir: File): Seq[File] = {
    import scala.sys.process._

    val command = 
      s"java -jar ${jar.getAbsolutePath()} ${rhoSourceDir.getAbsolutePath()} ${outputDir.getAbsolutePath()}"
      
    val status = command.!
    
    if (status == 0) {
      (outputDir ** "*.scala").get
    } else {
      throw new Exception(s"Non-zero exit status during rholang artifact creation: $status")
    }
  }

  val rholangSource = settingKey[File]("Default Rholang source directory.")
  val rholangProtoBuildAssembly = taskKey[File]("Rholang proto build Jar")
  val rholangScalaProto = taskKey[Seq[File]]("Generates managed Scala sources and proto from Rholang.")

  lazy val rholangSettings = Seq(
    rholangSource in Compile := (sourceDirectory in Compile).value / "rholang",
    rholangSource in Test := (sourceDirectory in Test).value / "rholang",
    sourceGenerators in Compile += (rholangScalaProto in Compile).taskValue,
    sourceGenerators in Test += (rholangScalaProto in Test).taskValue,
    rholangScalaProto in Compile := {
      constructArtifacts(
        rholangProtoBuildAssembly.value,
        (rholangSource in Compile).value,
        (sourceManaged in Compile).value
      )
    },
    rholangScalaProto in Test := {
      constructArtifacts(
        rholangProtoBuildAssembly.value,
        (rholangSource in Test).value,
        (sourceManaged in Test).value
      )
    }

  )
}
