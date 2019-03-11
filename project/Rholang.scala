import sbt._
import Keys._
import sbt.io.Path.relativeTo

/*
 * There doesn't seem to be a good way to have a plugin defined in
 * one sub-project be used in another sub-project. So we need to
 * call an assembled jar instead, building it if necessary.
 */
object Rholang {

  val rholangSource             = settingKey[File]("Default Rholang source directory.")

  lazy val rholangSettings = Seq(
    exportJars := true,
    rholangSource in Compile := (sourceDirectory in Compile).value / "rholang",
    rholangSource in Test := (sourceDirectory in Test).value / "rholang",

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
