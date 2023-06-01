import sbt._
import Keys._
import sbt.io.Path.relativeTo

/*
 * There doesn't seem to be a good way to have a plugin defined in
 * one sub-project be used in another sub-project. So we need to
 * call an assembled jar instead, building it if necessary.
 */
object Rholang {

  val rholangSource = settingKey[File]("Default Rholang source directory.")

  lazy val rholangSettings = Seq(
    exportJars := true,
    Compile / rholangSource := (Compile / sourceDirectory).value / "rholang",
    Test / rholangSource := (sourceDirectory in Test).value / "rholang",
    Compile / packageBin / mappings ++= {
      val generatedProtos = (resourceManaged in Compile).value ** "*.proto"
      generatedProtos pair relativeTo((resourceManaged in Compile).value)
    },
    Test / packageBin / mappings ++= {
      val generatedProtos = (Test / resourceManaged).value ** "*.proto"
      generatedProtos pair relativeTo((Test / resourceManaged).value)
    }
  )
}
