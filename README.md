# RChain

[![Build Status](https://travis-ci.org/rchain/rchain.svg?branch=dev)](https://travis-ci.org/rchain/rchain)
[![codecov](https://codecov.io/gh/rchain/rchain/branch/master/graph/badge.svg)](https://codecov.io/gh/rchain/rchain)

The open-source RChain project is building a decentralized, economic, censorship-resistant, public compute infrastructure and blockchain. It will host and execute programs popularly referred to as “smart contracts”. It will be trustworthy, scalable, concurrent, with proof-of-stake consensus and content delivery.

### Communication

The `comm` subproject contains code for network related operations for RChain.

The network layer is the lowest level component in the architecture and it
is featured in our **Node.Hello (v0.1) release**. The simplest way to get
started is with [docker][]: `docker run -ti rchain/rchain-comm`. For other options,
see [comm/README.md][cr].

[docker]: https://store.docker.com/community/images/rchain/rchain-comm
[cr]: https://github.com/rchain/rchain/tree/master/comm

### Rholang

The `rholang` subproject contains compiler related code for the Rholang language.

### Roscala

The `roscala` subproject contains a Scala translation of the Rosette VM.

### Rosette

The `rosette` subproject contains code for a low level virtual machine for RChain.

### Storage

The `storage` subproject contains code related to the key-value storage of the RChain blockchain.

### Filing Issues

File issues in our Public Jira Instance: [File a bug](https://rchain.atlassian.net/secure/CreateIssueDetails!init.jspa?pid=10105&issuetype=10103&versions=10012&components=10004&assignee=medha&summary=issue+created%20via+link)

## Building and Running

This document is for do-it-yourselfers, who want to try out _very_ alpha versions of the RChain software. For those lacking in masochistic tendencies, periodic releases are available via Docker at [our Docker storefront](https://store.docker.com/profiles/rchain).

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
sbt:rchain> project storage
[info] Set current project to storage (in build file:/home/kirkwood/src/rchain/)
sbt:storage> compile
[... compiling storage ...]
```
but single-line commands work, too:
```
$ sbt "project storage" clean compile test
```
or
```
$ sbt storage/clean storage/compile storage/test
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
[info] 	   storage
```

In most cases, simply building the project you care about is enough:

```
sbt:rchain> storage/compile
[info] Updating storage...
[info] Done updating.
[info] Compiling 3 protobuf files to /home/kirkwood/src/rchain/storage/target/scala-2.12/src_managed/main
[info] Compiling schema /home/kirkwood/src/rchain/storage/src/main/protobuf/Block.proto
[info] Compiling schema /home/kirkwood/src/rchain/storage/src/main/protobuf/SystemContract.proto
[info] Compiling schema /home/kirkwood/src/rchain/storage/src/main/protobuf/Contract.proto
protoc-jar: protoc version: 340, detected platform: linux/amd64
protoc-jar: executing: [/tmp/protocjar6602275461596807284/bin/protoc.exe, --plugin=protoc-gen-scala=/tmp/protocbridge191927131554276550, --scala_out=flat_package,grpc:/home/kirkwood/src/rchain/storage/target/scala-2.12/src_managed/main, -I/home/kirkwood/src/rchain/storage/src/main/protobuf, -I/home/kirkwood/src/rchain/storage/target/protobuf_external, /home/kirkwood/src/rchain/storage/src/main/protobuf/Block.proto, /home/kirkwood/src/rchain/storage/src/main/protobuf/SystemContract.proto, /home/kirkwood/src/rchain/storage/src/main/protobuf/Contract.proto]
[info] Compiling protobuf
[info] Protoc target directory: /home/kirkwood/src/rchain/storage/target/scala-2.12/src_managed/main
[info] Compiling 18 Scala sources to /home/kirkwood/src/rchain/storage/target/scala-2.12/classes ...
[info] Done compiling.
[success] Total time: 12 s, completed Feb 13, 2018 3:13:28 PM
```

To work entirely within one of the subprojects, it's easy to keep a running `sbt` console up, and switch into the project. All subsequent commands are scoped to that subproject:
```
sbt:rchain> project storage
[info] Set current project to storage (in build file:/home/kirkwood/src/rchain/)
sbt:storage> compile
[info] Updating ...
[info] Done updating.
[info] Compiling 3 protobuf files to /home/kirkwood/src/rchain/storage/target/scala-2.12/src_managed/main
[info] Compiling schema /home/kirkwood/src/rchain/storage/src/main/protobuf/Block.proto
[info] Compiling schema /home/kirkwood/src/rchain/storage/src/main/protobuf/SystemContract.proto
[info] Compiling schema /home/kirkwood/src/rchain/storage/src/main/protobuf/Contract.proto
protoc-jar: protoc version: 340, detected platform: linux/amd64
protoc-jar: executing: [/tmp/protocjar9042295252462121263/bin/protoc.exe, --plugin=protoc-gen-scala=/tmp/protocbridge4033649966837455863, --scala_out=flat_package,grpc:/home/kirkwood/src/rchain/storage/target/scala-2.12/src_managed/main, -I/home/kirkwood/src/rchain/storage/src/main/protobuf, -I/home/kirkwood/src/rchain/storage/target/protobuf_external, /home/kirkwood/src/rchain/storage/src/main/protobuf/Block.proto, /home/kirkwood/src/rchain/storage/src/main/protobuf/SystemContract.proto, /home/kirkwood/src/rchain/storage/src/main/protobuf/Contract.proto]
[info] Compiling protobuf
[info] Protoc target directory: /home/kirkwood/src/rchain/storage/target/scala-2.12/src_managed/main
[info] Compiling 18 Scala sources to /home/kirkwood/src/rchain/storage/target/scala-2.12/classes ...
[info] Done compiling.
[success] Total time: 9 s, completed Feb 13, 2018 4:35:05 PM
sbt:storage> test
[info] Packaging /home/kirkwood/src/rchain/storage/target/scala-2.12/storage_2.12-0.1.0-SNAPSHOT.jar ...
[info] Done packaging.
[info] Compiling 6 Scala sources to /home/kirkwood/src/rchain/storage/target/scala-2.12/test-classes ...
[info] Done compiling.
[info] Packaging /home/kirkwood/src/rchain/storage/target/scala-2.12/storage_2.12-0.1.0-SNAPSHOT-tests.jar ...
[info] Done packaging.
[info] MultiplierUnitTests:
[info] Multiplier tryParse
[info] - should work
[... eyerollingly thorough test output snipped...]
[success] Total time: 7 s, completed Feb 13, 2018 4:35:13 PM
sbt:storage> 
```

#### Whole-project Build

Everything that can be stitched together can be done so with the `node` project. `sbt` will even make a Docker image you can run. A single-line build command that does the trick is
```
<computer:~/src/rchain (dev)> sbt clean bnfc:clean bnfc:generate compile docker
[... tons of output snipped ...]
[info] Step 5/5 : ENTRYPOINT ["\/bin\/main.sh"]
[info]  ---> Running in 015ae98a7ea7
[info] Removing intermediate container 015ae98a7ea7
[info]  ---> 33341b27ac61
[info] Successfully built 33341b27ac61
[info] Tagging image 33341b27ac61 with name: coop.rchain/coop.rchain-node:latest
[info] Tagging image 33341b27ac61 with name: coop.rchain/coop.rchain-node:v0.1
[success] Total time: 9 s, completed Feb 14, 2018 7:47:59 AM
```

### Running

Invoking the above Docker image is simple enough:
```
<computer:~/src/rchain (dev)> docker run -ti coop.rchain/coop.rchain-node:latest
15:49:21.363 [main] INFO main - uPnP: Some(/10.0.0.9) -> Some(192.168.0.101)
15:49:21.497 [main] INFO main - Listening for traffic on #{Network rnode://facff6c005814a669b2063b38f8fc6c4@10.0.0.9:30304}.
15:49:21.500 [main] INFO main - Bootstrapping from #{PeerNode 0f365f1016a54747b384b386b8e85352}.
15:49:21.500 [main] DEBUG p2p - connect(): Connecting to #{PeerNode 0f365f1016a54747b384b386b8e85352}
15:49:21.826 [main] DEBUG p2p - connect(): Received encryption handshake response from #{PeerNode 0f365f1016a54747b384b386b8e85352}.
15:49:21.868 [main] DEBUG p2p - connect(): Received protocol handshake response from #{PeerNode 0f365f1016a54747b384b386b8e85352}.
15:49:25.715 [Thread-2] INFO p2p - Responded to encryption handshake request from #{PeerNode bfa3c0b9d9ce4f30adc2e17979f93285}.
15:49:25.882 [Thread-2] INFO p2p - Responded to protocol handshake request from #{PeerNode bfa3c0b9d9ce4f30adc2e17979f93285}.
15:49:26.974 [main] INFO main - Possibly new peer: #{PeerNode c12882b563fa47c9af297ce952ef7d94}.
15:49:26.974 [main] DEBUG p2p - connect(): Connecting to #{PeerNode c12882b563fa47c9af297ce952ef7d94}
[...]
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
