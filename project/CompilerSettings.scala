import java.lang.Runtime.getRuntime

import sbt._
import sbt.Keys._

object CompilerSettings {

  /*
   * In the future, let's add:
   *
   *   "-Xfatal-warnings",
   *   "-Xlint:adapted-args",
   *   "-Xlint:inaccessible",
   *   "-Ywarn-value-discard",
   */

  private lazy val commonOptions =
    // format: off
    Seq(
      // To prevent "dead code following this construct" error when using `*` from mockito-scala library
      // https://github.com/mockito/mockito-scala/#dead-code-warning
      // This warning should be disabled only for Tests but in that case IntelliJ cannot compile project.
      // Related issues https://youtrack.jetbrains.com/issue/SCL-11824, https://youtrack.jetbrains.com/issue/IDEA-232043
      // "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-language:_",
      "-unchecked",
      "-Xfatal-warnings",
      //With > 16: [error] invalid setting for -Ybackend-parallelism must be between 1 and 16
      //https://github.com/scala/scala/blob/v2.12.6/src/compiler/scala/tools/nsc/settings/ScalaSettings.scala#L240
      "-Ybackend-parallelism", getRuntime.availableProcessors().min(16).toString,
      // Added with migration to Scala 2.13
      // needed for Cats-tagless
      "-Ymacro-annotations",
      // disable exhaustivity checking of unsealed types
      "-Xlint:-strict-unsealed-patmat",
      "-Xnon-strict-patmat-analysis",
      // Error: Block result was adapted via implicit conversion (method apply) taking a by-name parameter
      "-Xlint:-byname-implicit",
      "-Xlint:adapted-args"
    )
  // format: on

  lazy val options = Seq(
    javacOptions ++= Seq("-encoding", "UTF-8"),
    scalacOptions ++= commonOptions ++ {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, major)) if major >= 12 =>
          // We disable warnings about unused imports for Scala 2.12+
          // because certain imports (ex: cat.syntax.either) provide
          // compatibility for Scala versions < 2.12
          Seq(
            "-Xlint:-unused,-adapted-args,-inaccessible,_",
            "-Ywarn-unused:implicits",
            "-Ywarn-macros:after",
            "-Ywarn-unused:locals",
            "-Ywarn-unused:patvars",
            "-Ywarn-unused:privates"
          )
        case _ =>
          Seq(
            "-Xlint:-missing-interpolator,-adapted-args,-inaccessible,_",
            "-Ywarn-unused",
            "-Ywarn-unused:imports"
          )
      }
    },
    Compile / console / scalacOptions ~= {
      _.filterNot(
        Set(
          "-Xfatal-warnings",
          "-Ywarn-unused:imports"
        )
      )
    },
    Test / console / scalacOptions := (Compile / console / scalacOptions).value
  )
}
