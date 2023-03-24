addSbtPlugin("com.thesamet" % "sbt-protoc" % "1.0.6")
// Yes it's weird to do the following, but it's what is mandated by the scalapb documentation
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.11.3"

addSbtPlugin("com.typesafe.sbt"       % "sbt-license-report"   % "1.2.0")
addSbtPlugin("org.wartremover"        % "sbt-wartremover"      % "2.4.10")
addSbtPlugin("org.scalameta"          % "sbt-scalafmt"         % "2.4.0")
addSbtPlugin("com.eed3si9n"           % "sbt-assembly"         % "0.14.10")
addSbtPlugin("org.scoverage"          % "sbt-scoverage"        % "1.6.1")
addSbtPlugin("com.github.tkawachi"    % "sbt-repeat"           % "0.1.0")
addSbtPlugin("com.eed3si9n"           % "sbt-buildinfo"        % "0.11.0")
addSbtPlugin("com.typesafe.sbt"       % "sbt-native-packager"  % "1.3.12")
addSbtPlugin("pl.project13.scala"     % "sbt-jmh"              % "0.4.0")
addSbtPlugin("com.typesafe.sbt"       % "sbt-site"             % "1.4.1")
addSbtPlugin("com.typesafe.sbt"       % "sbt-ghpages"          % "0.6.3")
addSbtPlugin("com.jsuereth"           % "sbt-pgp"              % "2.0.1")
addSbtPlugin("org.xerial.sbt"         % "sbt-sonatype"         % "2.6")
addSbtPlugin("net.virtual-void"       % "sbt-dependency-graph" % "0.9.2")
addSbtPlugin("io.spray"               % "sbt-revolver"         % "0.9.1")
addSbtPlugin("com.sksamuel.scapegoat" %% "sbt-scapegoat"       % "1.1.1")
// TODO replace with addSbtPlugin("org.typelevel" % "sbt-fs2-grpc" % "<latest-version>")
//   when migrated top CE3 since latest fs2-grpc is not available for CE2
addSbtPlugin("org.lyranthe.fs2-grpc" % "sbt-java-gen" % "0.11.2")
