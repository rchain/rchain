# RChain Node

Rchain Node is a module that gathers all other subprojects into final executable.

## 1. Building from source

### 1.1 Building JAR

#### 1.1.1 Prerequisites

In this pre-release version, successful building requires attention to several prerequisites. Prequisites are defined in [rchain/README.md](https://github.com/rchain/rchain/blob/master/README.md). 

__Note__ Failure to attend to all prerequisites will result in errors.
      
#### 1.1.2. Node depends on the following subprojects: 

1. comm
2. crypto
3. rholang
4. rspace

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

### 1.2 Building Docker image

Run:
```
sbt docker
```

To test if the image is available to use, simply run `docker images`, `coop.rchain/rnode` should be on the list of available images.

```
hideout:rchain rabbit$ docker images | grep rchain
coop.rchain/rnode                               latest              b1af7024d9bf        6 minutes ago       131MB
```


## 2. Modes

The node module comes with single executable (jar, docker image, debian or fedora package), that can run in a few modes. Which mode is running depends on the flag you use. 
You can run node with the following flags:

```
  -b, --bootstrap  <arg>   Bootstrap rnode address for initial seed.
      --data_dir  <arg>    Path to data directory. Defaults to /var/lib/rnode
      --deploy-demo        Demo sending some placeholder Deploy operations to
                           Casper at regular intervals
  -d, --diagnostics        Node diagnostics
  -e, --eval  <arg>        Starts a thin client that will evaluate rholang in
                           file on a existing running node. See grpcHost and
                           grpcPort.
      --grpc-host  <arg>   Hostname or IP of node on which gRPC service is
                           running.
  -g, --grpc-port  <arg>   Port used for gRPC API.
  -h, --host  <arg>        Hostname or IP of this node.
  -x, --http-port  <arg>   HTTP port (deprecated - all API features will be
                           ported to gRPC API).
  -m, --map_size  <arg>    Map size (in bytes)
  -n, --name  <arg>        Node name or key (deprecated, will be removed in next
                           release).
  -p, --port  <arg>        Network port to use. Currently UDP port, will become
                           TCP port in next release.
  -r, --repl               Starts a thin client, that will connect to existing
                           node. See grpcHost and grpcPort.
  -s, --standalone         Start a stand-alone node (no bootstrapping).
      --help               Show help message
      --version            Show version of this program	  
```

### 2.1 The Node
By default when you execute the program, it will fire up a running node instance. That instance will become either a bootstrap node (see `--standalone` flag) or will try to connect to existing bootstrap.

Node will instantiate a peer-to-peer network. It will either connect to some already existing node in the network (called bootstrap node) or will create a new network (essentially acting as bootstrap node). __Note__ This release prints a great deal of diagnostic information.

An RChain node is addressed by an "rnode address", which has the following form

```
rnode://<address-key>@<host-or-ip>:<udp-port>
```

This version generates (non-cryptographically) random address keys of 128 bits, or 32 characters (UUIDs,
essentially). Future releases will generate full-length 256- or 512-bit rnode addresses, but for demonstration purposes,
128 bits is about the limit of manageability.

By default - when run without any flag - the communication system attempts to connect itself to the test RChain network by bootstrapping from a known node in that network. 

Using flags you can specify which bootstrapping node should be used or if the node should create its own network (become a bootstrap node).

#### 2.1.1 gRPC API

Node exposes its API via gRPC services, which are exposed on `grpc-port`. To see the list of all available services, RPC calls, possible requests and responses, please see [node/src/main/protobuf/rnode.proto](https://github.com/rchain/rchain/blob/master/node/src/main/protobuf/rnode.proto)

#### 2.1.2 Data directory

Node needs to have read and write access to a folder called data directory. By default that folder is `/var/lib/rnode/`. User can control that value by providing `--data_dir` flag. 
Regardless of which path on the file system you choose for the data directory, please remember that node needs to have read and write access to that folder.

Below is an example of creating folder for default location and giving it ownership to the current user.

```
sudo mkdir -p /var/lib/rnode
sudo chown $USER /var/lib/rnode
```

#### 2.1.2 Running the Node

##### 2.1.2.1 via Docker

By far the simplest way to run this code is by using Docker. Use this pull command in Docker to get the current version of RNode

```docker pull rchain/rnode```

You can also [build a docker image yourself](#building-via-docker) and then run it.

__Note__ The port used has to be mapped to the proper host port for the node to be able to advertise itself to the network
properly. This may happen automatically, and it may not; it completely depends on how your computer and network are configured. Some monkeying with `docker run` options may be required, and the `--host` and `--port` options to this system may also help.


```
$ docker run -ti rchain/rnode
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

To use both the peer-to-peer and REPL capabilities of RNode, two containers running RNode need to be connected to one user-defined network bridge:

```bash
> docker network create rnode-net

> docker run -dit --name rnode0 --network rnode-net rchain/rnode:latest -s

> docker run -it --name rnode-repl --network rnode-net rchain/rnode:latest --grpc-host rnode0 -r
```

##### 2.1.2.2 via Java

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

### 2.2 REPL
When running the program with `--repl`, it will fire up a thin REPL client. This client will connect to running node instance via gRPC to provide simple REPL functionality. You can control to which node you will connect by providing `--grpc-host` and `--grpc-port`.
In REPL mode users have the ability to execute Rholang commands in REPL environment, which will be evaluated by the Rholang interpreter


#### 2.2.1 Running via Docker
Assuming you have a RNode running in Docker, use the command below to run the node in REPL mode.

```
$ docker run -ti rchain/rnode --repl
```

#### 2.2.2. Running via Java
This will run Node from JAR file that was built in [Building from source](#building-from-source)

```
$ java -jar ./node/target/scala-2.12/rnode-assembly-0.1.3.jar --repl
```

### 2.3 Eval
When running the program with `--eval`, it will fire up a thin program that will connect to running node instance via gRPC to evaluate Rholang code that is stored in a plain text file on the node itself.

### 2.3.1 Running via Docker
This assumes you have a RNode running in Docker. To run Rholang that is stored in a plain text file (filename.rho), use

'''
docker run -it --mount type=bind,source="$(pwd)"/file_directory,target=/tmp rchain/rnode --eval /tmp/filename.rho
'''

This command will run the node in interpreter mode and will make a directory on the local system available to the interpreter as a location where Rholang contracts can be executed. When running your docker container, be aware of your current path - the 'pwd' command sticks your current path in the bind command.

 
## 3. Miscellaneous

### 3.1. Host and Port

The system attempts to find a gateway device with Universal Plug-and-Play enabled. If that fails, the system tries to guess a good IP address and a reasonable UDP port that other nodes can use to communicate with this one. If it does not guess a usable pair, they may be specified on the command line using the `--host` and `--port` options:

```
--host 1.2.3.4 --port 30304
```

By default it uses UDP port 30304. This is also how more than one node may be run on a single machine: just pick different
ports. Remember that if using Docker, ports may have to be properly mapped and forwarded. For example, if we want to connect on the test net on UDP port 12345 and our machine's public IP address is 1.2.3.4, we could do it like so:

```
$ docker run -ti -p 12345:12345/udp rchain/rchain-comm:latest -p 12345 --host 1.2.3.4
```

or perhaps by causing docker to use the host network and not its own bridge:

```
$ docker run -ti --network=host rchain/rchain-comm:latest -p 12345
```

This may take some experimentation to find combinations of arguments that work for any given setup.

Read more than you want to know about Docker networking starting about
[here](https://docs.docker.com/engine/userguide/networking/work-with-networks/), but honestly, it's featureful and powerful enough that you need a [cheatsheet](https://github.com/wsargent/docker-cheat-sheet#exposing-ports).

### 3.2 Bootstrapping a Private Network

It is possible to set up a private RChain network by running a standalone node and using it for bootstrapping other nodes. Here we run one on port 4000:

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

### 3.3 Metrics
The current version of the node produces metrics on some communications-related activities in Prometheus format. A local node and metrics visualizer may be started by following the instructions found [here](https://github.com/rchain/rchain/blob/master/docker/node/README.md).

### 3.4 Caveats

This is very much a work in progress. The networking overlay is only known to work when it can avail itself of visible IP addresses, either public or all contained within the same network. It does not yet include any special code for getting around a home firewall or a closed router, though it does contain some uPNP handling. Any port used must be open or mapped through the router. Depending on your setup, it might be necessary to configure port-forwarding on your router. In some cases, it might even be necessary to specify your router's public IP address as the node address if your router's port-forwarding requires it.


