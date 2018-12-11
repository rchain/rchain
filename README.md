# RChain

[![Build Status](https://travis-ci.org/rchain/rchain.svg?branch=dev)](https://travis-ci.org/rchain/rchain)
[![codecov](https://codecov.io/gh/rchain/rchain/branch/master/graph/badge.svg)](https://codecov.io/gh/rchain/rchain)

The open-source RChain project is building a decentralized, economic, censorship-resistant, public compute infrastructure and blockchain. It will host and execute programs popularly referred to as “smart contracts”. It will be trustworthy, scalable, concurrent, with proof-of-stake consensus and content delivery.

[RChain Developer](https://developer.rchain.coop/) features project-related tutorials and documentation, project planning information, events calendar, and information for how to engage with this project.

## Running

### Running from Docker

Assuming you have Docker running on your system, use the following command to fetch the latest version of RNode from the remote Docker hub and run it (exit with `C-c`):
```
<computer:~/src/rchain (dev)> docker run -v $HOME/tmp:/var/lib/rnode -ti -p 40400:40400 rchain/rnode:latest --profile docker
08:30:30.894 [main] INFO  conf - uPnP: None -> None
08:30:32.599 [main] INFO  o.h.b.c.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:40402
08:30:32.600 [main] INFO  org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _
08:30:32.601 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
08:30:32.601 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
08:30:32.601 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
08:30:32.601 [main] INFO  org.http4s.server.blaze.BlazeBuilder -              |_|
08:30:32.647 [main] INFO  org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:40402/
08:30:32.710 [kamon.prometheus.PrometheusReporter] INFO  kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:40403
08:30:32.799 [main] INFO  logger - gRPC server started, listening on
08:30:32.827 [main] INFO  logger - Listening for traffic on rnode://3afa77d09eb24a6caa25c0cb6a3e969f@172.17.0.2:40400.
08:30:32.841 [main] INFO  logger - Bootstrapping from #{PeerNode de6eed5d00cf080fc587eeb412cb31a75fd10358}.
08:30:32.857 [main] INFO  logger - Initialize first phase handshake (encryption handshake) to #{PeerNode de6eed5d00cf080fc587eeb412cb31a75fd10358}
[...]
```

In order to use both the peer-to-peer network and REPL capabilities of the node, you must run more than one Docker Rnode on the same host, the containers need to be connected to one user-defined network bridge:

```bash
> docker network create rnode-net

> docker run -v $HOME/tmp:/var/lib/rnode -dit --name rnode0 --network rnode-net rchain/rnode:latest -s

> docker ps
CONTAINER ID        IMAGE                      COMMAND             CREATED             STATUS              PORTS               NAMES
15aa78b45da4        rchain/rnode:latest   "/bin/main.sh -s"   3 seconds ago       Up 2 seconds                            rnode0
```

In a new terminal:
```bash
> docker logs -f rnode0
08:38:09.738 [main] INFO  conf - uPnP: None -> None
08:38:11.252 [main] INFO  o.h.b.c.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:40402
08:38:11.253 [main] INFO  org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _
08:38:11.253 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
08:38:11.253 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
08:38:11.254 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
08:38:11.254 [main] INFO  org.http4s.server.blaze.BlazeBuilder -              |_|
08:38:11.298 [main] INFO  org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:40402/
08:38:11.358 [kamon.prometheus.PrometheusReporter] INFO  kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:40403
08:38:11.436 [main] INFO  logger - gRPC server started, listening on
08:38:11.460 [main] INFO  logger - Listening for traffic on rnode://ee00a5357f2f4cb58b08a8a4c949da1b@172.18.0.2:40400.
08:38:11.463 [main] INFO  logger - Starting stand-alone node.
```

Note this line (listening address):
```bash
Listening for traffic on rnode://ee00a5357f2f4cb58b08a8a4c949da1b@172.18.0.2:40400.
```

A repl instance can be invoked this way:
```bash
> docker run -v $HOME/tmp:/var/lib/rnode -it --name rnode-repl --network rnode-net rchain/rnode:latest --grpc-host rnode0 -r

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
> docker run -v $HOME/tmp:/var/lib/rnode -it --name rnode-client --network rnode-net rchain/rnode:latest --bootstrap rnode://ee00a5357f2f4cb58b08a8a4c949da1b@172.18.0.2:40400
08:58:34.595 [main] INFO  conf - uPnP: None -> None
08:58:36.053 [main] INFO  o.h.b.c.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:40402
08:58:36.054 [main] INFO  org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _
08:58:36.054 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
08:58:36.054 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
08:58:36.055 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
08:58:36.055 [main] INFO  org.http4s.server.blaze.BlazeBuilder -              |_|
08:58:36.098 [main] INFO  org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:40402/
08:58:36.139 [kamon.prometheus.PrometheusReporter] INFO  kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:40403
08:58:36.241 [main] INFO  logger - gRPC server started, listening on
08:58:36.267 [main] INFO  logger - Listening for traffic on rnode://29d77e8cfd924db49e715d4cf4eeb28d@172.18.0.4:40400.
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

### Configuration file
Most of the [command line options](node/README.md##2-modes) can be specified in a configuration file `rnode.toml`. The default location of the configuration file is the data dir. An alternative lococation can be specified with the command line option `--config-file  <path>`. The format of the configuration file is [TOML](https://github.com/toml-lang/toml). Example configuration file:
```toml
[server]
host = "localhost"
port = 40400
http-port = 40402
metrics-port = 40403
no-upnp = false
default-timeout = 2000
bootstrap = "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109:40400"
standalone = false
data-dir = "/var/lib/rnode"
map-size = 1073741824
casper-block-store-size = 1073741824
in-memory-store = false
max-num-of-connections = 500

[grpc-server]
host = "localhost"
port = 40401
port-internal = 40404

[tls]
certificate = "/var/lib/rnode/certificate.pem"
key = "/var/lib/rnode/key.pem"

[validators]
count = 5
shard-id = "rchain"
sig-algorithm = "ed25519"
# bonds-file = ""
# wallets-file = ""
# known = ""
# public-key = ""
# private-key = ""
```

### Running from the tar-ball
TBD

### Installing and running on Debian from DEB package
TBD

### Installing and running on RedHat and Fedora from RPM package
TBD

### Installing and running on macOS via Homebrew

#### Installing Homebrew - https://brew.sh
```bash
> /usr/bin/ruby -e "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install)"
```

#### Installing rnode
```bash
> brew install rchain/rchain/rnode
```
## Deverloper guide

For getting started with development of RChain please refer to the [Developer guide](DEVELOPER.md)

## Caveats and filing issues

### Caveats
During this pre-release phase of the RChain software, there are some [known issues](https://rchain.atlassian.net/wiki/spaces/CORE/pages/428376244/RChain+software+unresolved+bugs+and+known+issues).

### Filing Issues

File issues in our Public Jira Instance: [File a bug](https://rchain.atlassian.net/secure/CreateIssueDetails!init.jspa?pid=10105&issuetype=10103&versions=10012&components=10004&assignee=medha&summary=issue+created%20via+link)

## Acknowledgements
We use YourKit to profile rchain performance.
YourKit supports open source projects with its full-featured Java Profiler.
YourKit, LLC is the creator of <a href="https://www.yourkit.com/java/profiler/">YourKit Java Profiler</a>
and <a href="https://www.yourkit.com/.net/profiler/">YourKit .NET Profiler</a>,
innovative and intelligent tools for profiling Java and .NET applications.

