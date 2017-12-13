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

The only up-front build-time requirement is [sbt](http://www.scala-sbt.org/download.html), which should be installed
according to your platform. It, in turn, will download and include all depedencies for the system.

To build once `sbt` is installed, issue:

```
sbt assembly
```

This will download dependencies, if required, compile all the code, construct a single, large jar file, and run all the
tests.

### Building via Docker

If you have [docker](https://www.docker.com/) installed, you can build a docker image. Under the covers, the docker
build process is the `sbt` build process, exactly as described above. The command

```
docker build . -t rchain-comm:latest
```

will build an image tagged "latest" containing the jar file and a suitable entry point.


## Running a Node

A simple, stand-alone node which does nothing but wait for another node to contact it may be invoked by running the
program with no arguments.

### Running with Java

The fat jar built above may be run with Java like so:

```
$ java -Dfile.encoding=UTF8 -Djava.net.preferIPv4Stack=true -jar target/scala-2.12/src-assembly-0.0.1.jar
17:34:52.110 [main] INFO main - Listening for traffic on #{Network rnode://ac4b1fdee5a947e8a511383e698df99d@63.224.55.75:30304}.
```

### Running via Docker

If you built a docker image called `rchain-comm:latest`, you can run that with

```
$ docker run -ti -p 30304:30304/udp rchain-comm:latest
Picked up JAVA_TOOL_OPTIONS: -Dfile.encoding=UTF8 -Djava.net.preferIPv4Stack=true
04:20:56.910 [main] INFO main - Listening for traffic on #{Network rnode://c75981a613b547d7bd7e127c543e4d31@172.17.0.6:30304}.
```

Note that the port used has to be mapped to the proper host port for the node to be able to advertise itself to the
network properly. The host might also have to be set to the docker host's IP (not the container's IP, which will
probably be the one chosen automatically), _and_ the port has to be open or forwarded from the immediate router.

### Command-line Arguments

However it gets run, it responds to the following arguments:

```
  -b, --bootstrap  <arg>   Bootstrap rnode address for initial seed.
  -h, --host  <arg>        Hostname or IP of this node.
  -p, --port  <arg>        Network port to use.
      --help               Show help message
      --version            Show version of this program
```

#### Bootstrapping

By default, the node will not attempt to bootstrap into any other network and so will create a brand new network with
only the one

```
--bootstrap rnode://ace40ebca0924eb797bb69dfda04f5d9@216.83.154.106:30012
```

#### Host and Port

The system attempts to guess an good IP address and UDP port that other nodes can use to communicate with this one. If
it does not guess a usable pair, they may be specified on the command line using the `--host` and `--port` options:

```
--host 1.2.3.4 --port 30304 --bootstrap rnode://ace40ebca0924eb797bb69dfda04f5d9@216.83.154.106:30012
```

By default it uses UDP port 30304. This is also how more than one node may be run on a single machine: just pick
different ports. Remember that if using Docker, ports have to be properly mapped and forwarded. For example, if we want
to connect on the test net on UDP port 12345 and our machine's public IP address is 63.224.55.75, we could do it like
so:

```
$ docker run -ti -p 12345:12345/udp rchain-comm:latest -p 12345 --host 63.224.55.75 --bootstrap rnode://ace40ebca0924eb797bb69dfda04f5d9@216.83.154.106:30012
```

Read more than you want to know about Docker networking starting about
[here](https://docs.docker.com/engine/userguide/networking/work-with-networks/), but honestly, it's featureful and
powerful enough that you need a [cheatsheet](https://github.com/wsargent/docker-cheat-sheet#exposing-ports).

## Notes

### Caveats

This is very much a work in progress. The networking overlay is only known to work when it can avail itself of visible
IP addresses, either public or all contained within the same network. It does not yet include any special code for
getting around a home firewall or a closed router, including uPNP handling. Any port used must be open. Additionally, it
might be necessary to configure port-forwarding on your router. In some cases, it might even be necessary to specify
your router's public IP address as the node address if your router's port-forwarding requires it.

### Dependency list

The list of dependencies that sbt downloads and packages with the system is currently
 * lenses_2.12-0.4.12.jar
 * fastparse-utils_2.12-0.4.4.jar
 * macro-compat_2.12-1.1.1.jar
 * slf4j-api-1.7.25.jar
 * logback-classic-1.2.3.jar
 * scala-logging_2.12-3.7.2.jar
 * scala-uri_2.12-0.5.0.jar
 * sourcecode_2.12-0.1.4.jar
 * fastparse_2.12-0.4.4.jar
 * protobuf-java-3.4.0.jar
 * parboiled_2.12-2.1.4.jar
 * curve25519-java-0.4.1.jar
 * spray-json_2.12-1.3.2.jar
 * scala-library.jar
 * scalactic_2.12-3.0.1.jar
 * logback-core-1.2.3.jar
 * shapeless_2.12-2.3.2.jar
 * scallop_2.12-3.0.3.jar
 * scrypto_2.12-2.0.0.jar
 * supertagged_2.12-1.3.jar
 * scalapb-runtime_2.12-0.6.6.jar
 * bcprov-jdk15on-1.58.jar
 * guava-19.0.jar

And for testing, add to that
 * scala-reflect.jar
