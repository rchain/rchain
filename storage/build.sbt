// import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "coop.rchain",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "storage",
    // http://assets.maven-repository.com/artifact/org.lmdbjava/lmdbjava/0.0.2
    // Last modified: 2016-07-29 02:24:11 UTC
    libraryDependencies += "org.lmdbjava" % "lmdbjava" % "0.0.2",
    // https://mvnrepository.com/artifact/junit/junit/4.12
    // Date: Dec 04, 2014
    libraryDependencies += "junit" % "junit" % "4.12" % "test",
    // https://mvnrepository.com/artifact/org.deephacks.lmdbjni/lmdbjni-linux64
    // Date: Jan 19, 2017
    libraryDependencies += "org.deephacks.lmdbjni" % "lmdbjni-linux64" % "0.4.7",
    // https://mvnrepository.com/artifact/org.agrona/Agrona
    // Date: Dec 20, 2016
    libraryDependencies += "org.agrona" % "Agrona" % "0.9.1",
    // https://mvnrepository.com/artifact/org.hamcrest/hamcrest-all
    // Date: Jul 09, 2012
    libraryDependencies += "org.hamcrest" % "hamcrest-all" % "1.3" % "test",
    // https://mvnrepository.com/artifact/com.google.guava/guava
    // Date: Aug 04, 2017
    libraryDependencies += "com.google.guava" % "guava" % "23.0"
    // http://www.scalatest.org/install
    // Date: Oct 10, 2017
    // libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"
    // libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

    // http://www.scalatest.org/install
    // http://www.scalatest.org/supersafe
    // Date: 0ct 10, 2017
    // addSbtPlugin("com.artima.supersafe" % "sbtplugin" % "1.1.2")
  )

connectInput in run := true

