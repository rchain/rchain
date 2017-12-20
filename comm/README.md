# RChain Communication Subsystem

Network related operations for RChain.

## Addresses

An RChain node is addressed by an "rnode address", which has the following form

```
rnode://<address-key>@<host-or-ip>:<udp-port>
```

This version generates (non-cryptographically) random addresses of 128 bits, or 32 characters (UUIDs,
essentially). Future releases will generate full-length 256- or 512-bit rnode addresses, but for demonstration purposes,
128 bits is about the limit of manageability.

## Building from Source

Clone (or download and unpack) the master branch of the source.

### Building with `sbt`

The only up-front build-time requirements are the Java Development Toolkit (we've been using [OpenJDK version
1.8.0_151](http://openjdk.java.net/install/)) and [sbt](http://www.scala-sbt.org/download.html), both of which should be installed
according to your platform.

To build, simply issue from a terminal:

```
sbt assembly
```

This will download dependencies, if required, compile all the code, construct a single, large jar file, and run all the tests.

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

If you have [docker](https://www.docker.com/) installed, you can build a docker image. Under the covers, the docker build process is
the `sbt` build process, exactly as described above. The command

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

## Running a Node

By default, the communication system attempts to connect itself to the test RChain network by bootstrapping from a known node in
that network. Note that this release prints a great deal of diagnostic information.

### Running via Docker

By far the simplest way to run this code is by using Docker and the official image at Docker Hub:

```
$ docker run -ti rchain/rchain-comm
Unable to find image 'rchain/rchain-comm:latest' locally
latest: Pulling from rchain/rchain-comm
Status: Downloaded newer image for rchain/rchain-comm:latest
16:45:51.344 [main] INFO main - uPnP: None -> None
16:45:51.467 [main] INFO main - Listening for traffic on #{Network rnode://a94bd1f373ae4dd39dbb78f03a131075@172.17.0.2:30304}.
16:45:51.469 [main] INFO main - Bootstrapping from rnode://0f365f1016a54747b384b386b8e85352@216.83.154.106:30012.
16:45:51.470 [main] DEBUG p2p - connect(): Connecting to #{PeerNode 0f365f1016a54747b384b386b8e85352}
```

### Running with Java

The fat jar built above may be run with Java like so:

```
$ java -Djava.net.preferIPv4Stack=true -jar target/scala-2.12/comm-assembly-0.1.jar
07:57:16.465 [main] INFO main - uPnP: Some(/10.0.0.3) -> Some(192.168.1.1)
07:57:16.584 [main] INFO main - Listening for traffic on #{Network rnode://f869a0d0a21744e1807b4176b0217e56@10.0.0.3:30304}.
07:57:16.585 [main] INFO main - Bootstrapping from rnode://0f365f1016a54747b384b386b8e85352@216.83.154.106:30012.
07:57:16.586 [main] DEBUG p2p - connect(): Connecting to #{PeerNode 0f365f1016a54747b384b386b8e85352}
```

### Running Local Image via Docker

If you built a docker image called `rchain-comm:latest`, you can run that with

```
$ docker run -ti rchain-comm
16:42:48.310 [main] INFO main - uPnP: None -> None
16:42:48.414 [main] INFO main - Listening for traffic on #{Network rnode://0acfd966f69a412ebd8c0b267c9817dc@172.17.0.2:30304}.
16:42:48.415 [main] INFO main - Bootstrapping from rnode://0f365f1016a54747b384b386b8e85352@216.83.154.106:30012.
16:42:48.416 [main] DEBUG p2p - connect(): Connecting to #{PeerNode 0f365f1016a54747b384b386b8e85352}
```

Note that the port used has to be mapped to the proper host port for the node to be able to advertise itself to the network
properly. This may happen automatically, and it may not; it completely depends on how your computer and network are configured. Some
monkeying with `docker run` options may be required, and the `--host` and `--port` options to this system may also help.

### Command-line Arguments

However it gets run, it responds to the following arguments:

```
  -b, --bootstrap  <arg>   Bootstrap rnode address for initial seed.
  -h, --host  <arg>        Hostname or IP of this node.
  -n, --name  <arg>        Node name or key.
  -p, --port  <arg>        Network port to use.
  -s, --standalone         Start a stand-alone node (no bootstrapping).
      --help               Show help message
      --version            Show version of this program
```

#### Bootstrapping a Private Network

It is possible to set up a private RChain network by running a standalone node and using it for bootstrapping other nodes. Here we
run one on port 40000:

```
$ java -Djava.net.preferIPv4Stack=true -jar target/scala-2.12/comm-assembly-0.1.jar --standalone --port 40000
08:51:46.823 [main] INFO main - uPnP: Some(/10.0.0.3) -> Some(192.168.1.1)
08:51:46.926 [main] INFO main - Listening for traffic on #{Network rnode://142acf0737fd4f2ab030150723ef5dea@10.0.0.3:40000}.
08:51:46.927 [main] INFO main - Starting stand-alone node.
```

Now bootstrapping the other node just means giving the argument

```
--bootstrap rnode://142acf0737fd4f2ab030150723ef5dea@10.0.0.3:40000
```

For example:

```
$ docker run -ti rchain-comm --bootstrap rnode://142acf0737fd4f2ab030150723ef5dea@10.0.0.3:40000 --port 40001            
16:52:37.517 [main] INFO main - uPnP: None -> None
16:52:37.624 [main] INFO main - Listening for traffic on #{Network rnode://5fb160c772ea40a89567ce83733a9d05@172.17.0.2:40001}.
16:52:37.625 [main] INFO main - Bootstrapping from rnode://142acf0737fd4f2ab030150723ef5dea@10.0.0.3:40000.
16:52:37.626 [main] DEBUG p2p - connect(): Connecting to #{PeerNode 142acf0737fd4f2ab030150723ef5dea}
16:52:38.035 [main] DEBUG p2p - connect(): Received encryption handshake response from #{PeerNode 142acf0737fd4f2ab030150723ef5dea}.
16:52:38.044 [main] DEBUG p2p - connect(): Received protocol handshake response from #{PeerNode 142acf0737fd4f2ab030150723ef5dea}.
16:52:43.100 [main] INFO main - Peers: 1.
```

#### Host and Port

The system attempts to find a gateway device with Universal Plug-and-Play enabled. If that fails, the system tries to guess a good
IP address and a reasonable UDP port that other nodes can use to communicate with this one. If it does not guess a usable pair, they
may be specified on the command line using the `--host` and `--port` options:

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

## Notes

### Caveats

This is very much a work in progress. The networking overlay is only known to work when it can avail itself of visible IP addresses,
either public or all contained within the same network. It does not yet include any special code for getting around a home firewall
or a closed router, though it does contain some uPNP handling. Any port used must be open or mapped through the router. Depending on
your setup, it might be necessary to configure port-forwarding on your router. In some cases, it might even be necessary to specify
your router's public IP address as the node address if your router's port-forwarding requires it.

### Dependency list

The list of dependencies that sbt downloads and packages with the system is currently
 * bcprov-jdk15on-1.58.jar
 * curve25519-java-0.4.1.jar
 * fastparse-utils_2.12-0.4.4.jar
 * fastparse_2.12-0.4.4.jar
 * guava-19.0.jar
 * lenses_2.12-0.4.12.jar
 * logback-classic-1.2.3.jar
 * logback-core-1.2.3.jar
 * macro-compat_2.12-1.1.1.jar
 * parboiled_2.12-2.1.4.jar
 * protobuf-java-3.4.0.jar
 * scala-library.jar
 * scala-logging_2.12-3.7.2.jar
 * scala-reflect.jar
 * scala-uri_2.12-0.5.0.jar
 * scalactic_2.12-3.0.1.jar
 * scalapb-runtime_2.12-0.6.6.jar
 * scallop_2.12-3.0.3.jar
 * scrypto_2.12-2.0.0.jar
 * shapeless_2.12-2.3.2.jar
 * slf4j-api-1.7.25.jar
 * sourcecode_2.12-0.1.4.jar
 * spray-json_2.12-1.3.2.jar
 * supertagged_2.12-1.3.jar
 * weupnp-0.1.4.jar
