from ubuntu:17.04

run apt update && apt install -y dirmngr apt-transport-https openjdk-8-jdk bc
run echo "deb https://dl.bintray.com/sbt/debian /" > /etc/apt/sources.list.d/sbt.list && apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv 2EE0EA64E40A89B84B2DF73499E82A75642AC823 
run apt update && apt dist-upgrade -y && apt install -y sbt

copy . /src
workdir /src
run rm ensime.sbt

env JAVA_TOOL_OPTIONS -Dfile.encoding=UTF8

run sbt sbtVersion
run sbt compile

entrypoint [ "sbt", "console" ]
