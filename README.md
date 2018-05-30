# RChain

[![Build Status](https://travis-ci.org/rchain/rchain.svg?branch=dev)](https://travis-ci.org/rchain/rchain)
[![codecov](https://codecov.io/gh/rchain/rchain/branch/master/graph/badge.svg)](https://codecov.io/gh/rchain/rchain)

The open-source RChain project is building a decentralized, economic, censorship-resistant, public compute infrastructure and blockchain. It will host and execute programs popularly referred to as “smart contracts”. It will be trustworthy, scalable, concurrent, with proof-of-stake consensus and content delivery.

[RChain Developer](https://developer.rchain.coop/) features project-related tutorials and documentation, project planning information, events calendar, and information for how to engage with this project.

## Building and running

Pre-release versions of the RChain software are now available. We plan to launch the full platform in Q4 of 2018.

__Note__ Successfully building from source requires attending to all of the prerequisites shown below. When users experience errors, it is typically related to failure to assure all prerequisites are met. Work is in progress to improve this experience.

### Running from Docker
Assuming you have Docker running on your system, use the following pull command to run the latest version of RNode in Docker
'docker pull rchain/rnode:latest`

### Building and running from source
#### Prerequisites
* Java Development Kit (JDK), version 8. We recommend using the OpenJDK
* [sbt](https://www.scala-sbt.org/download.html)
* For crypto, the [Sodium crypto library](https://github.com/jedisct1/libsodium)
* For Rholang
    - [CUP](http://www2.cs.tum.edu/projects/cup/install.php) 0.11b-2014-06-11 or later. See [Rholang README](https://github.com/rchain/rchain/blob/master/rholang/README.md) for notes on installation requirements.
     - [jflex](http://jflex.de/)
     - Build [BNFC](http://bnfc.digitalgrammars.com/) from the following commit or later: [BNFC/bnfc@7c9e859](https://github.com/BNFC/bnfc/commit/7c9e859). Use the installation command `cabal install bnfc --global`.
     
#### Building and running
Building some of the subprojects is just a matter of `sbt compile`, however some (like `rholang` or `crypto`) require extra steps to build. See README.md of each subproject for details.

Once you can build each subproject individually, run `sbt node/assembly` to build an executable. The assembled jar will be available under `./node/target/scala-2.12/rnode-assembly-x.y.z.jar`

Example
```scala
sbt:rchain> node/assembly
[info] Including: JLex.jar
[info] Including: log4s_2.12-1.4.0.jar
[info] Including: java-cup-11b-runtime.jar

(...)

[info] SHA-1: bd4471642bb340c8b1fc0571fc614902c5bafbb2
[info] Packaging /Users/rabbit/projects/rchain/node/target/scala-2.12/rnode-assembly-0.1.3.jar ...
[info] Done packaging.
[success] Total time: 25 s, completed Mar 26, 2018 3:36:09 PM
```

## Information for developers
Assure prerequisites shown above are met.

### If cross-developing for Linux (e.g. Ubuntu) on a Mac
You will need a virtual machine running the appropriate version of Linux.
1. Install [VirtualBox]( https://www.virtualbox.org/wiki/Downloads)
2. Install the Linux distribution you need (e.g. [Ubuntu](http://releases.ubuntu.com/16.04/ubuntu-16.04.3-server-amd64.iso))
3. Start VirtualBox and create a new virtual machine in the manager
4. Boot your virtual machine using the Linux distribution ISO installed in step 2.
5. Configure your Linux VM as desired. You may need to install additional tools sucah as g++, g++-multilib, make, git, etc.

For a more convenient experience, you can share a folder on your Mac with the virtual machine. To do this you will need to install the VirtualBox Guest Additions. Unfortunately there are some gotchas with this. You may need to utilize one of these [solutions](https://askubuntu.com/questions/573596/unable-to-install-guest-additions-cd-image-on-virtual-box).

### Developer Quick-Start

When working in a single project, scope all `sbt` commands to that project. The most effective way is to maintain a running `sbt` instance, invoked from the project root:
```
$ sbt
[info] Loading settings from plugins.sbt ...
[info] Loading global plugins from /home/kirkwood/.sbt/1.0/plugins
[info] Loading settings from plugins.sbt,protoc.sbt ...
[info] Loading project definition from /home/kirkwood/src/rchain/project
[info] Loading settings from build.sbt ...
[info] Set current project to rchain (in build file:/home/kirkwood/src/rchain/)
[info] sbt server started at local:///home/kirkwood/.sbt/1.0/server/e6a65c30ec6e52272d3a/sock
sbt:rchain> project rspace
[info] Set current project to rspace (in build file:/home/kirkwood/src/rchain/)
sbt:rspace> compile
[... compiling rspace ...]
```
but single-line commands work, too:
```
$ sbt "project rspace" clean compile test
```
or
```
$ sbt rspace/clean rspace/compile rspace/test
```

### Building

The build is organized into several, mostly autonomous projects. These projects may be built (and used!) on their own, or they may be combined together to form the full node package. The build process in any case is contained in and controlled by a single, top-level `build.sbt` file. This process is able to produce several different kinds of artifacts, including JAR files (for Scala) and Docker images.

The most up-to-date code is found in the `dev` branch. This brilliant, cutting-edge source is periodically merged into `master`, which branch should represent a more stable, tested version.

#### Subprojects

The available subprojects may be listed by `sbt`:
```
sbt:rchain> projects
[info] In file:/home/kirkwood/src/rchain/
[info] 	   comm
[info] 	   node
[info] 	 * rchain
[info] 	   rholang
[info] 	   roscala
[info] 	   roscala_macros
[info] 	   rspace
```

In most cases, simply building the project you care about is enough:

```
sbt:rchain> rspace/compile
[info] Updating rspace...
[info] Done updating.
[info] Compiling 3 protobuf files to /home/kirkwood/src/rchain/rspace/target/scala-2.12/src_managed/main
[info] Compiling schema /home/kirkwood/src/rchain/rspace/src/main/protobuf/Block.proto
[info] Compiling schema /home/kirkwood/src/rchain/rspace/src/main/protobuf/SystemContract.proto
[info] Compiling schema /home/kirkwood/src/rchain/rspace/src/main/protobuf/Contract.proto
protoc-jar: protoc version: 340, detected platform: linux/amd64
protoc-jar: executing: [/tmp/protocjar6602275461596807284/bin/protoc.exe, --plugin=protoc-gen-scala=/tmp/protocbridge191927131554276550, --scala_out=flat_package,grpc:/home/kirkwood/src/rchain/rspace/target/scala-2.12/src_managed/main, -I/home/kirkwood/src/rchain/rspace/src/main/protobuf, -I/home/kirkwood/src/rchain/rspace/target/protobuf_external, /home/kirkwood/src/rchain/rspace/src/main/protobuf/Block.proto, /home/kirkwood/src/rchain/rspace/src/main/protobuf/SystemContract.proto, /home/kirkwood/src/rchain/rspace/src/main/protobuf/Contract.proto]
[info] Compiling protobuf
[info] Protoc target directory: /home/kirkwood/src/rchain/rspace/target/scala-2.12/src_managed/main
[info] Compiling 18 Scala sources to /home/kirkwood/src/rchain/rspace/target/scala-2.12/classes ...
[info] Done compiling.
[success] Total time: 12 s, completed Feb 13, 2018 3:13:28 PM
```

To work entirely within one of the subprojects, it's easy to keep a running `sbt` console up, and switch into the project. All subsequent commands are scoped to that subproject:
```
sbt:rchain> project rspace
[info] Set current project to rspace (in build file:/home/kirkwood/src/rchain/)
sbt:rspace> compile
[info] Updating ...
[info] Done updating.
[info] Compiling 3 protobuf files to /home/kirkwood/src/rchain/rspace/target/scala-2.12/src_managed/main
[info] Compiling schema /home/kirkwood/src/rchain/rspace/src/main/protobuf/Block.proto
[info] Compiling schema /home/kirkwood/src/rchain/rspace/src/main/protobuf/SystemContract.proto
[info] Compiling schema /home/kirkwood/src/rchain/rspace/src/main/protobuf/Contract.proto
protoc-jar: protoc version: 340, detected platform: linux/amd64
protoc-jar: executing: [/tmp/protocjar9042295252462121263/bin/protoc.exe, --plugin=protoc-gen-scala=/tmp/protocbridge4033649966837455863, --scala_out=flat_package,grpc:/home/kirkwood/src/rchain/rspace/target/scala-2.12/src_managed/main, -I/home/kirkwood/src/rchain/rspace/src/main/protobuf, -I/home/kirkwood/src/rchain/rspace/target/protobuf_external, /home/kirkwood/src/rchain/rspace/src/main/protobuf/Block.proto, /home/kirkwood/src/rchain/rspace/src/main/protobuf/SystemContract.proto, /home/kirkwood/src/rchain/rspace/src/main/protobuf/Contract.proto]
[info] Compiling protobuf
[info] Protoc target directory: /home/kirkwood/src/rchain/rspace/target/scala-2.12/src_managed/main
[info] Compiling 18 Scala sources to /home/kirkwood/src/rchain/rspace/target/scala-2.12/classes ...
[info] Done compiling.
[success] Total time: 9 s, completed Feb 13, 2018 4:35:05 PM
sbt:rspace> test
[info] Packaging /home/kirkwood/src/rchain/rspace/target/scala-2.12/rspace_2.12-0.1.0-SNAPSHOT.jar ...
[info] Done packaging.
[info] Compiling 6 Scala sources to /home/kirkwood/src/rchain/rspace/target/scala-2.12/test-classes ...
[info] Done compiling.
[info] Packaging /home/kirkwood/src/rchain/rspace/target/scala-2.12/rspace_2.12-0.1.0-SNAPSHOT-tests.jar ...
[info] Done packaging.
[info] MultiplierUnitTests:
[info] Multiplier tryParse
[info] - should work
[... eyerollingly thorough test output snipped...]
[success] Total time: 7 s, completed Feb 13, 2018 4:35:13 PM
sbt:rspace> 
```

#### Whole-project Build

Everything that can be stitched together can be done so with the `node` project. `sbt` will even make a Docker image you can run. A single-line build command that does the trick is
```
<computer:~/src/rchain (dev)> sbt clean bnfc:clean bnfc:generate compile docker
[... tons of output snipped ...]
[info] Step 8/8 : ENTRYPOINT ["\/bin\/main.sh"]
[info]  ---> Running in 2ac7f835192d
[info] Removing intermediate container 2ac7f835192d
[info]  ---> 5e79e6d92528
[info] Successfully built 5e79e6d92528
[info] Tagging image 5e79e6d92528 with name: coop.rchain/rnode
[success] Total time: 35 s, completed May 24, 2018 10:19:14 AM
```

```
<computer:~/src/rchain (dev)> docker images
REPOSITORY          TAG                 IMAGE ID            CREATED             SIZE
coop.rchain/rnode   latest              5e79e6d92528        7 minutes ago       143MB
<none>              <none>              e9b49f497dd7        47 hours ago        143MB
openjdk             8u151-jre-alpine    b1bd879ca9b3        4 months ago        82MB
```

### Running

Invoking the above Docker image is simple enough (exit with `C-c`):
```
<computer:~/src/rchain (dev)> docker run -v $HOME/tmp:/var/lib/rnode -ti coop.rchain/rnode:latest
08:30:30.894 [main] INFO  conf - uPnP: None -> None
08:30:32.599 [main] INFO  o.h.b.c.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:8080
08:30:32.600 [main] INFO  org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _
08:30:32.601 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
08:30:32.601 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
08:30:32.601 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
08:30:32.601 [main] INFO  org.http4s.server.blaze.BlazeBuilder -              |_|
08:30:32.647 [main] INFO  org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:8080/
08:30:32.710 [kamon.prometheus.PrometheusReporter] INFO  kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:9095
08:30:32.799 [main] INFO  logger - gRPC server started, listening on
08:30:32.827 [main] INFO  logger - Listening for traffic on rnode://3afa77d09eb24a6caa25c0cb6a3e969f@172.17.0.2:30304.
08:30:32.841 [main] INFO  logger - Bootstrapping from #{PeerNode acd0b05a971c243817a0cfd469f5d1a238c60294}.
08:30:32.857 [main] INFO  logger - Initialize first phase handshake (encryption handshake) to #{PeerNode acd0b05a971c243817a0cfd469f5d1a238c60294}
[...]
```

In order to use both the peer-to-peer network and REPL capabilities of the node, you must run more than one Docker Rnode on the same host, the containers need to be connected to one user-defined network bridge:

```bash
> docker network create rnode-net

> docker run -v $HOME/tmp:/var/lib/rnode -dit --name rnode0 --network rnode-net coop.rchain/rnode:latest -s

> docker ps
CONTAINER ID        IMAGE                      COMMAND             CREATED             STATUS              PORTS               NAMES
15aa78b45da4        coop.rchain/rnode:latest   "/bin/main.sh -s"   3 seconds ago       Up 2 seconds                            rnode0
```

In a new terminal:
```bash
> docker logs -f rnode0
08:38:09.738 [main] INFO  conf - uPnP: None -> None
08:38:11.252 [main] INFO  o.h.b.c.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:8080
08:38:11.253 [main] INFO  org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _
08:38:11.253 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
08:38:11.253 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
08:38:11.254 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
08:38:11.254 [main] INFO  org.http4s.server.blaze.BlazeBuilder -              |_|
08:38:11.298 [main] INFO  org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:8080/
08:38:11.358 [kamon.prometheus.PrometheusReporter] INFO  kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:9095
08:38:11.436 [main] INFO  logger - gRPC server started, listening on
08:38:11.460 [main] INFO  logger - Listening for traffic on rnode://ee00a5357f2f4cb58b08a8a4c949da1b@172.18.0.2:30304.
08:38:11.463 [main] INFO  logger - Starting stand-alone node.
```

Note this line (listening address):
```bash
Listening for traffic on rnode://ee00a5357f2f4cb58b08a8a4c949da1b@172.18.0.2:30304.
```

A repl instance can be invoked this way:
```bash
> docker run -v $HOME/tmp:/var/lib/rnode -it --name rnode-repl --network rnode-net coop.rchain/rnode:latest --grpc-host rnode0 -r

  ╦═╗┌─┐┬ ┬┌─┐┬┌┐┌  ╔╗╔┌─┐┌┬┐┌─┐  ╦═╗╔═╗╔═╗╦
  ╠╦╝│  ├─┤├─┤││││  ║║║│ │ ││├┤   ╠╦╝║╣ ╠═╝║
  ╩╚═└─┘┴ ┴┴ ┴┴┘└┘  ╝╚╝└─┘─┴┘└─┘  ╩╚═╚═╝╩  ╩═╝

rholang $ 5
Storage Contents:
 for( x0 <= @{"stdout"} ) { Nil } | for( x0, x1 <= @{"stderrAck"} ) { Nil } | for( x0 <= @{"stderr"} ) { Nil } | for( x0, x1 <= @{"stdoutAck"} ) { Nil }
```

The above command should result in (`rnode0` output):
```bash
[...]
08:38:11.463 [main] INFO  logger - Starting stand-alone node.

Evaluating:
5
```

A peer node can be started with the following command (note that `--bootstrap` takes the listening address of `rnode0`):
```bash
> docker run -v $HOME/tmp:/var/lib/rnode -it --name rnode-client --network rnode-net coop.rchain/rnode:latest --bootstrap rnode://ee00a5357f2f4cb58b08a8a4c949da1b@172.18.0.2:30304
08:58:34.595 [main] INFO  conf - uPnP: None -> None
08:58:36.053 [main] INFO  o.h.b.c.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:8080
08:58:36.054 [main] INFO  org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _
08:58:36.054 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
08:58:36.054 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
08:58:36.055 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
08:58:36.055 [main] INFO  org.http4s.server.blaze.BlazeBuilder -              |_|
08:58:36.098 [main] INFO  org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:8080/
08:58:36.139 [kamon.prometheus.PrometheusReporter] INFO  kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:9095
08:58:36.241 [main] INFO  logger - gRPC server started, listening on
08:58:36.267 [main] INFO  logger - Listening for traffic on rnode://29d77e8cfd924db49e715d4cf4eeb28d@172.18.0.4:30304.
08:58:36.279 [main] INFO  logger - Bootstrapping from #{PeerNode ee00a5357f2f4cb58b08a8a4c949da1b}.
08:58:36.294 [main] INFO  logger - Initialize first phase handshake (encryption handshake) to #{PeerNode ee00a5357f2f4cb58b08a8a4c949da1b}
08:58:36.816 [repl-io-29] INFO  logger - Initialize second phase handshake (protocol handshake) to #{PeerNode ee00a5357f2f4cb58b08a8a4c949da1b}
08:58:36.890 [repl-io-30] INFO  logger - Connected #{PeerNode ee00a5357f2f4cb58b08a8a4c949da1b}.
08:58:41.939 [repl-io-30] INFO  logger - Peers: 1.
```

The above command should result in (`rnode0` output):
```bash
08:58:36.769 [repl-io-29] INFO  logger - Responded to encryption handshake request from #{PeerNode 29d77e8cfd924db49e715d4cf4eeb28d}.
08:58:36.882 [repl-io-29] INFO  logger - Responded to protocol handshake request from #{PeerNode 29d77e8cfd924db49e715d4cf4eeb28d}
08:58:37.211 [repl-io-35] INFO  logger - Peers: 1.
```


Each scoped build is as similar to the original, per-project build process as possible, so assemblies should be produced in the same way as before:
```
<computer:~/src/rchain (dev)> sbt "project rholang" assembly
[info] Loading settings from plugins.sbt ...
[... compiling, testing, jarring ...]
[info] Packaging /home/kirkwood/src/rchain/rholang/target/scala-2.12/rholang-assembly-0.1.0-SNAPSHOT.jar ...
<computer:~/src/rchain (dev)> rholang/rho2rbl rholang/examples/hello_world_again.rho 
compiled rholang/examples/hello_world_again.rho to rholang/examples/hello_world_again.rbl
<computer:~/src/rchain (dev)> cat rholang/examples/hello_world_again.rbl; echo
(let [[helloworld (generateFresh "helloworld")]] (block ( (proc [] (run (compile (let [[[[Rholanga4716fe347be]] (consume t [helloworld] [['world]] #t)]] ( (proc [[world]] (let [[[Rholang5401f0efc120] (consume t [world] ['msg] #f)]] ( (proc [[[msg]]] ( display msg "
")) [Rholang5401f0efc120]))) [Rholanga4716fe347be])))))) (let [[world (generateFresh "world")] [world2 (generateFresh "world2")]] (block (block (block (produce t helloworld world) (produce t world "Hello World")) (produce t helloworld world2)) (produce t world2 "Hello World again")))))
<computer:~/src/rchain (dev)> 
```


## Description of subprojects

### Communication

The `comm` subproject contains code for network related operations for RChain.

### Rholang

The `rholang` subproject contains compiler related code for the Rholang language.

### Roscala

The `roscala` subproject contains a Scala translation of the Rosette VM.

### Rosette

The `rosette` subproject contains code for a low level virtual machine for RChain.

### Rspace

The `rspace` subproject contains code related to the key-value storage of the RChain blockchain.


## Caveats and filing issues

### Caveats
During this pre-release phase of the RChain software, there are some [known issues](https://rchain.atlassian.net/wiki/spaces/CORE/pages/428376244/RChain+software+unresolved+bugs+and+known+issues). 

### Filing Issues

File issues in our Public Jira Instance: [File a bug](https://rchain.atlassian.net/secure/CreateIssueDetails!init.jspa?pid=10105&issuetype=10103&versions=10012&components=10004&assignee=medha&summary=issue+created%20via+link)

