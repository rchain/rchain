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
      "-Xfuture",
      "-Ypartial-unification",
      // To prevent "dead code following this construct" error when using * mockito-scala library
      // https://github.com/mockito/mockito-scala/issues/29
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
      "-Ybackend-parallelism", getRuntime.availableProcessors().min(16).toString
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
            "-Ywarn-unused-import"
          )
      }
    },
    scalacOptions in (Compile, console) ~= {
      _.filterNot(
        Set(
          "-Xfatal-warnings",
          "-Ywarn-unused-import",
          "-Ywarn-unused:imports"
        )
      )
    },
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
  )
}
