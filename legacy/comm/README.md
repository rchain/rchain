# RChain Communication Subsystem

Network related operations for RChain. 

## Build from the source

The only up-front build-time requirements are the Java Development Toolkit (we've been using [OpenJDK version
1.8.0_151](http://openjdk.java.net/install/)) and [sbt](http://www.scala-sbt.org/download.html), both of which should be installed
according to your platform.

Simply run `sbt comm/compile` to compile the project, `sbt comm/test` to run all the tests suite.

## Exposed programming API for other modules

`TransportLayer` - responsible for node2node communicaton
`NodeDiscovery`  - responsible for node discovery within p2p network
`Metrcis`        - responsible for storing metrics witin the running node
