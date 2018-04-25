# RChain Node

## 1. Overview

At the moment, you can execute node in three separate modes: 

1. REPL
2. EVAL
3. P2P

In REPL mode users have the ability to execute Rho-lang commands in a REPL environment, which will be evaluated by the Rho-lang interpreter. Note that at the moment - in REPL mode - no node-to-node communication is possible.

EVAL mode allows users run Rholang that is stored in a plain text file (filename.rho). In this mode the node will make a directory on the local system available to the interpreter as a location where Rholang contracts can be executed.

In P2P mode, node will instantiate a peer-to-peer network. It will either connect to some already existing node in the network (called bootstrap node) or will create a new network (essentially acting as bootstrap node).

## 2. Building

### Building from source

#### Prerequisites
* Java Development Kit (JDK), version 8
    - We recommend using the OpenJDK 
    - Alternatively, the [Oracle JDK](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html) is an option
* [sbt](https://www.scala-sbt.org/download.html)
* For crypto, the [Sodium crypto library](https://github.com/jedisct1/libsodium)
* For Rholang
    - [CUP](http://www2.cs.tum.edu/projects/cup/install.php) 0.11b-2014-06-11 or later. See [Rholang README](https://github.com/rchain/rchain/blob/master/rholang/README.md) for notes on installation requirements.
     - [jflex](http://jflex.de/)
     - Build [BNFC](http://bnfc.digitalgrammars.com/) from the following commit or later: [BNFC/bnfc@7c9e859](https://github.com/BNFC/bnfc/commit/7c9e859)
      
#### Node depends on the following subprojects: 

1. comm
2. crypto
3. rholang
4. storage

Building some of them is just a matter of `sbt compile`, however some (like `rholang` or `crypto`) require extra steps to build. See README.md of each subproject for details.

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

### Building via Docker

The only Docker combination that has been extensively tested is

```
Client:
 Version:      17.09.1-ce
 API version:  1.32
 Go version:   go1.8.3
 Git commit:   19e2cf6
 Built:        Thu Dec  7 22:24:28 2017
 OS/Arch:      linux/amd64

Server:
 Version:      17.09.1-ce
 API version:  1.32 (minimum version 1.12)
 Go version:   go1.8.3
 Git commit:   19e2cf6
 Built:        Thu Dec  7 22:23:07 2017
 OS/Arch:      linux/amd64
 Experimental: false
```

though others are expected to work.

#### Docker by itself

If you have [docker](https://www.docker.com/) installed, you can build a docker image. Under the covers, the docker build process is the `sbt` build process, exactly as described above. The command

```
docker build . -t rchain-comm:latest
```

will build an image tagged "latest" containing the jar file and a suitable entry point.

#### Docker via `sbt`

If the requirement for building via `sbt` are _also_ met, you can request that `sbt` build and tag docker images:

```
sbt docker
```

We recommend this method, as it is much quicker and builds a much slimmer image.

#### Testing that image is built and available

To test if image is available to use, simply run `docker images`, `coop.rchain/rnode` should be on the list of available images.

```
hideout:rchain rabbit$ docker images | grep rchain
coop.rchain/rnode                               latest              b1af7024d9bf        6 minutes ago       131MB
```

## 3. Running a Node

### 3.1 REPL mode

In REPL mode users have the ability to execute Rho-lang commands in REPL environment, which will be evaluated by the Rho-lang interpreter. Note that at the moment - in REPL mode - no node-to-node communication is possible

In order to execute node in REPL mode you need to run it with `--repl` flag. Note that in REPL mode all other flags will be ignored.

#### Running via Docker

By far the simplest way to run this code is by using Docker and the official image at Docker Hub. You can also [build docker image yourself](#building-via-docker) and then run it.

```
$ docker run -ti coop.rchain/rnode --repl
```

#### Running via Java

This will run Node from JAR file that was built in [Building from source](#building-from-source)

```
$ java -jar ./node/target/scala-2.12/rnode-assembly-0.1.3.jar --repl
> 
```

### 3.2 EVAL mode
In EVAL mode, node runs in the interpreter mode and supports running Rholang code that is stored in a plain text file.

### Running via Docker
By far the simplest way to run this code is by using Docker and the official image at Docker Hub. You can also [build docker image yourself](#building-via-docker) and then run it.

To run Rholang that is stored in a plain text file (filename.rho), use

'''
docker run -it --mount type=bind,source="$(pwd)"/file_directory,target=/tmp rchain/rnode --eval /tmp/filename.rho
'''

This command will run the node in interpreter mode and will make a directory on the local system available to the interpreter as a location where Rholang contracts can be executed. When running your docker container, be aware of your current path - the 'pwd' command sticks you current path in the bind command.

### 3.3 P2P mode

In P2P mode, node will instantiate a peer-to-peer network. It will either connect to some already existing node in the network (called bootstrap node) or will create a new network (essentially acting as bootstrap node). Note that this release prints a great deal of diagnostic information.

An RChain node is addressed by an "rnode address", which has the following form

```
rnode://<address-key>@<host-or-ip>:<udp-port>
```

This version generates (non-cryptographically) random address keys of 128 bits, or 32 characters (UUIDs,
essentially). Future releases will generate full-length 256- or 512-bit rnode addresses, but for demonstration purposes,
128 bits is about the limit of manageability.

By default - when run without any flag - the communication system attempts to connect itself to the test RChain network by bootstrapping from a known node in that network. 

Using flags you can specify which bootstrapping node should be used or if the node should create its own network (become a bootstrap node).


#### Running via Docker

By far the simplest way to run this code is by using Docker and the official image at Docker Hub. You can also [build docker image yourself](#building-via-docker) and then run it.

Note that the port used has to be mapped to the proper host port for the node to be able to advertise itself to the network
properly. This may happen automatically, and it may not; it completely depends on how your computer and network are configured. Some
monkeying with `docker run` options may be required, and the `--host` and `--port` options to this system may also help.


```
$ docker run -ti coop.rchain/rnode
17:12:21.938 [main] INFO main - uPnP: Some(/192.168.1.123) -> Some(93.158.233.123)
17:12:22.450 [kamon.prometheus.PrometheusReporter] INFO kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:9095
17:12:22.850 [main] INFO org.http4s.blaze.channel.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:8080
17:12:22.851 [main] INFO org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _     
17:12:22.852 [main] INFO org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
17:12:22.852 [main] INFO org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
17:12:22.852 [main] INFO org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
17:12:22.852 [main] INFO org.http4s.server.blaze.BlazeBuilder -              |_|
17:12:22.889 [main] INFO org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:8080/
17:12:22.963 [main] INFO main - Listening for traffic on rnode://6403d6e9f0874d31940a654f3f52a830@192.168.1.123:30304.
17:12:22.970 [main] INFO main - Bootstrapping from #{PeerNode 0f365f1016a54747b384b386b8e85352}.
17:12:22.975 [main] DEBUG main - Connecting to #{PeerNode 0f365f1016a54747b384b386b8e85352}
(...)
```

#### Running via Java

This will run Node from JAR file that was built in [Building from source](#building-from-source)

```
$ java -jar ./node/target/scala-2.12/rnode-assembly-0.1.3.jar
17:12:21.938 [main] INFO main - uPnP: Some(/192.168.1.123) -> Some(93.158.233.123)
17:12:22.450 [kamon.prometheus.PrometheusReporter] INFO kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:9095
17:12:22.850 [main] INFO org.http4s.blaze.channel.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:8080
17:12:22.851 [main] INFO org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _     
17:12:22.852 [main] INFO org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
17:12:22.852 [main] INFO org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
17:12:22.852 [main] INFO org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
17:12:22.852 [main] INFO org.http4s.server.blaze.BlazeBuilder -              |_|
17:12:22.889 [main] INFO org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:8080/
17:12:22.963 [main] INFO main - Listening for traffic on rnode://6403d6e9f0874d31940a654f3f52a830@192.168.1.123:30304.
17:12:22.970 [main] INFO main - Bootstrapping from #{PeerNode 0f365f1016a54747b384b386b8e85352}.
17:12:22.975 [main] DEBUG main - Connecting to #{PeerNode 0f365f1016a54747b384b386b8e85352}
(...)
```
 
## 4. Command-line Arguments (available flags)

```
RChain Node 0.1.3
  -b, --bootstrap  <arg>   Bootstrap rnode address for initial seed.
  -h, --host  <arg>        Hostname or IP of this node.
  -x, --http-port  <arg>   HTTP port.
  -n, --name  <arg>        Node name or key.
  -p, --port  <arg>        Network port to use.
  -r, --repl               Start node with REPL (but without P2P network)
  -s, --standalone         Start a stand-alone node (no bootstrapping).
      --help               Show help message
      --version            Show version of this program
```

## 5. Miscellaneous

### 5.1. Host and Port

The system attempts to find a gateway device with Universal Plug-and-Play enabled. If that fails, the system tries to guess a good
IP address and a reasonable UDP port that other nodes can use to communicate with this one. If it does not guess a usable pair, they may be specified on the command line using the `--host` and `--port` options:

```
--host 1.2.3.4 --port 30304
```

By default it uses UDP port 30304. This is also how more than one node may be run on a single machine: just pick different
ports. Remember that if using Docker, ports may have to be properly mapped and forwarded. For example, if we want to connect on the
test net on UDP port 12345 and our machine's public IP address is 1.2.3.4, we could do it like so:

```
$ docker run -ti -p 12345:12345/udp rchain/rchain-comm:latest -p 12345 --host 1.2.3.4
```

or perhaps by causing docker to use the host network and not its own bridge:

```
$ docker run -ti --network=host rchain/rchain-comm:latest -p 12345
```

This may take some experimentation to find combinations of arguments that work for any given setup.

Read more than you want to know about Docker networking starting about
[here](https://docs.docker.com/engine/userguide/networking/work-with-networks/), but honestly, it's featureful and powerful enough
that you need a [cheatsheet](https://github.com/wsargent/docker-cheat-sheet#exposing-ports).

### 5.2 Bootstrapping a Private Network

It is possible to set up a private RChain network by running a standalone node and using it for bootstrapping other nodes. Here we
run one on port 4000:

```
$ java -Djava.net.preferIPv4Stack=true -jar /Users/rabbit/projects/rchain/node/target/scala-2.12/rnode-assembly-0.1.3.jar -s -p 4000
11:21:00.164 [main] INFO  main - uPnP: Some(/192.168.1.123) -> Some(93.158.233.123)
11:21:00.600 [kamon.prometheus.PrometheusReporter] INFO  kamon.prometheus.PrometheusReporter - Started the embedded HTTP server on http://0.0.0.0:9095
11:21:01.012 [main] INFO  o.h.b.c.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:8080
11:21:01.013 [main] INFO  org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _     
11:21:01.014 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
11:21:01.014 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
11:21:01.014 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
11:21:01.014 [main] INFO  org.http4s.server.blaze.BlazeBuilder -              |_|
11:21:01.051 [main] INFO  org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:8080/
11:21:01.121 [main] INFO  main - Listening for traffic on rnode://a96a6c152711416f869da7fe8c2ced61@192.168.1.123:4000.
11:21:01.122 [main] INFO  main - Starting stand-alone node.
```

Now bootstrapping the other node just means giving the argument

```
--bootstrap rnode://a96a6c152711416f869da7fe8c2ced61@192.168.1.123:4000
```

For example:

```
$ java -Djava.net.preferIPv4Stack=true -jar /Users/rabbit/projects/rchain/node/target/scala-2.12/rnode-assembly-0.1.3.jar --bootstrap rnode://a96a6c152711416f869da7fe8c2ced61@192.168.1.123:4000 -p 4001 -x 8081
11:24:09.885 [main] INFO  main - uPnP: Some(/192.168.1.123) -> Some(93.158.233.123)
11:24:10.183 [kamon.prometheus.PrometheusReporter] ERROR kamon.ReporterRegistry - Metric reporter [kamon.prometheus.PrometheusReporter] failed to start.
11:24:10.567 [main] INFO  o.h.b.c.nio1.NIO1SocketServerGroup - Service bound to address /127.0.0.1:8081
11:24:10.568 [main] INFO  org.http4s.server.blaze.BlazeBuilder -   _   _   _        _ _     
11:24:10.568 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | |_| |_| |_ _ __| | | ___
11:24:10.568 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  | ' \  _|  _| '_ \_  _(_-<
11:24:10.569 [main] INFO  org.http4s.server.blaze.BlazeBuilder -  |_||_\__|\__| .__/ |_|/__/
11:24:10.569 [main] INFO  org.http4s.server.blaze.BlazeBuilder -              |_|
11:24:10.606 [main] INFO  org.http4s.server.blaze.BlazeBuilder - http4s v0.18.0 on blaze v0.12.11 started at http://127.0.0.1:8081/
11:24:10.674 [main] INFO  main - Listening for traffic on rnode://5fb06848ea89457e82a9e37565df2d57@192.168.1.123:4001.
11:24:10.678 [main] INFO  main - Bootstrapping from #{PeerNode a96a6c152711416f869da7fe8c2ced61}.
11:24:10.694 [main] INFO  main - Initialize first phase handshake (encryption handshake) to #{PeerNode a96a6c152711416f869da7fe8c2ced61}
11:24:11.201 [scala-execution-context-global-28] INFO  main - Initialize second phase handshake (protocol handshake) to #{PeerNode a96a6c152711416f869da7fe8c2ced61}
11:24:11.253 [scala-execution-context-global-28] INFO  main - Connected #{PeerNode a96a6c152711416f869da7fe8c2ced61}.
11:24:16.301 [scala-execution-context-global-28] INFO  main - Peers: 1.
```

where bootstrapped node will log 

```
11:23:34.784 [scala-execution-context-global-26] INFO  main - Responded to encryption handshake request from #{PeerNode c17855314fc743c6ac8df001b70b2963}.
11:24:11.163 [scala-execution-context-global-26] INFO  main - Responded to encryption handshake request from #{PeerNode 5fb06848ea89457e82a9e37565df2d57}.
11:24:11.249 [scala-execution-context-global-26] INFO  main - Responded to protocol handshake request from #{PeerNode 5fb06848ea89457e82a9e37565df2d57}
11:24:11.334 [main] INFO  main - Peers: 1.
```

### 5.3 Metrics
The current version of the node produces metrics on some communications-related activities in Prometheus format. A local node and metrics visualizer may be started by following the instructions found [here](https://github.com/rchain/rchain/blob/master/docker/node/README.md).

### 5.4 Caveats

This is very much a work in progress. The networking overlay is only known to work when it can avail itself of visible IP addresses,
either public or all contained within the same network. It does not yet include any special code for getting around a home firewall
or a closed router, though it does contain some uPNP handling. Any port used must be open or mapped through the router. Depending on
your setup, it might be necessary to configure port-forwarding on your router. In some cases, it might even be necessary to specify
your router's public IP address as the node address if your router's port-forwarding requires it.


