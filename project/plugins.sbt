addSbtPlugin("com.thesamet" % "sbt-protoc" % "0.99.18")
// Yes it's weird to do the following, but it's what is mandated by the scalapb documentation
libraryDependencies += "com.thesamet.scalapb" %% "compilerplugin" % "0.7.1"

addSbtPlugin("com.geirsson"        % "sbt-scalafmt"        % "1.4.0")
addSbtPlugin("com.eed3si9n"        % "sbt-assembly"        % "0.14.5")
addSbtPlugin("org.scoverage"       % "sbt-scoverage"       % "1.5.1")
addSbtPlugin("com.github.tkawachi" % "sbt-doctest"         % "0.7.1")
addSbtPlugin("com.eed3si9n"        % "sbt-buildinfo"       % "0.7.0")
addSbtPlugin("com.typesafe.sbt"    % "sbt-native-packager" % "1.3.3")
addSbtPlugin("pl.project13.scala"  % "sbt-jmh"             % "0.3.3")
addSbtPlugin("com.typesafe.sbt"    % "sbt-site"            % "1.3.2")
addSbtPlugin("com.typesafe.sbt"    % "sbt-ghpages"         % "0.6.2")
addSbtPlugin("com.jsuereth"        % "sbt-pgp"             % "1.1.1")
addSbtPlugin("org.xerial.sbt"      % "sbt-sonatype"        % "2.0")
addSbtPlugin("org.tpolecat"        % "tut-plugin"          % "0.6.3")
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.0")
