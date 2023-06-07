import sbt.*
import sbt.Keys.*

import java.lang.Runtime.getRuntime

object CompilerSettings {

  /** Common scalac compiler options */
  private lazy val commonOptions = {
    // https://docs.scala-lang.org/overviews/compiler-options/index.html
    // format: off
    Seq(
      // Scala language features (standard settings)
      "-language:implicitConversions",
      "-language:postfixOps",
      "-language:experimental.macros",
      "-deprecation",
      "-encoding", "UTF-8",
      "-feature",
      "-unchecked",
      "-Xfatal-warnings", // Warning as compile errors

      // Scala linter options
      "-Xlint:_",                // Enables all linter warnings (is this by default?)
      "-Xlint:adapted-args",
      "-Xlint:inaccessible",
      "-Ywarn-unused:-imports",  // Disables warning for unused imports
      "-Ywarn-unused:-privates", // Disables warning for unused private vars
      "-Ywarn-unused:-locals",   // Disables warning for unused local vars
      // Scala bug false positive warnings - https://github.com/scala/bug/issues/12072
      "-Xlint:-byname-implicit",

      // Disable exhaustivity checking of unsealed types (after migration to Scala 2.13)
      "-Xlint:-strict-unsealed-patmat",
      "-Xnon-strict-patmat-analysis",

      // Warn when numerics are widened
      "-Ywarn-numeric-widen",

      // To prevent "dead code following this construct" error when using `*` from mockito-scala library
      // https://github.com/mockito/mockito-scala/#dead-code-warning
      // This warning should be disabled only for Tests but in that case IntelliJ cannot compile project.
      // Related issues https://youtrack.jetbrains.com/issue/SCL-11824, https://youtrack.jetbrains.com/issue/IDEA-232043
      // "-Ywarn-dead-code",

      // TODO: Needed or duplicated with wartremover warnings?
      // Warn when non-Unit expression results are unused
      // "-Ywarn-value-discard",

      // With > 16: [error] invalid setting for -Ybackend-parallelism must be between 1 and 16
      // https://github.com/scala/scala/blob/v2.12.6/src/compiler/scala/tools/nsc/settings/ScalaSettings.scala#L240
      "-Ybackend-parallelism", getRuntime.availableProcessors().min(16).toString
    )
  }
  // format: on

  lazy val options = Seq(
    javacOptions ++= Seq("-encoding", "UTF-8"),
    scalacOptions ++= commonOptions,
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
