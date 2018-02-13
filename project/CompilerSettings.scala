import sbt._
import sbt.Keys._

object CompilerSettings {

  /*
   * In the future, let's add:
   *
   *   "-Xlint:adapted-args",
   *   "-Xlint:inaccessible",
   *   "-Ywarn-value-discard",
   */

  private lazy val commonOptions =
    // format: off
    Seq(
      // "-Xfatal-warnings",
      "-Xfuture",
      "-Ypartial-unification",
      "-Ywarn-dead-code",
      "-Ywarn-numeric-widen",
      // "-deprecation", // rholang's Term.scala uses some deprecated shit.  Kill it with fire.
      "-encoding", "UTF-8",
      "-feature",
      "-language:_",
      "-unchecked"
    )
    // format: on

  lazy val options = Seq(
    scalacOptions ++= commonOptions ++ {
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((2, major)) if major >= 12 =>
          // We disable warnings about unused imports for Scala 2.12+
          // because certain imports (ex: cat.syntax.either) provide
          // compatibility for Scala versions < 2.12
          Seq(
            "-Xlint:-unused,-adapted-args,-inaccessible,_",
            "-Ywarn-unused:implicits",
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
          // "-Xfatal-warnings",
          "-Ywarn-unused-import",
          "-Ywarn-unused:imports"
        ))
    },
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value
  )
}
