# RChain

[![Build Status](https://github.com/rchain/rchain/workflows/CI/badge.svg)](https://github.com/rchain/rchain/actions?query=workflow%3ACI+branch%3Astaging)
[![codecov](https://codecov.io/gh/rchain/rchain/branch/master/graph/badge.svg)](https://codecov.io/gh/rchain/rchain)

The open-source RChain project is building a decentralized, economic,
censorship-resistant, public compute infrastructure and blockchain. It will
host and execute programs popularly referred to as “smart contracts”. It will
be trustworthy, scalable, concurrent, with proof-of-stake consensus and
content delivery.

[RChain Developer](https://developer.rchain.coop/) features project-related
tutorials and documentation, project planning information, events calendar,
and information for how to engage with this project.

## Note on the use of this software
This code has not yet completed a security review. We strongly recommend that you do not use it in production or to transfer items of material value. We take no responsibility for any loss you may incur through the use of this code.

## Use the public testnet
The RChain Cooperative maintains a public testnet running the latest version of RNode. Learn more at [RChain public testnet information](https://rchain.atlassian.net/wiki/spaces/CORE/pages/678756429/RChain+public+testnet+information).

## Installation
### Docker

`$ docker pull rchain/rnode`

### Debian/Ubuntu

1. Download a `.deb` package from the [releases page](https://github.com/rchain/rchain/releases/)
2. `$ sudo apt install ./rnode_<VERSION>.deb`, where `<VERSION>` is something like `0.9.18`

### RedHat/Fedora

1. Download a `.rpm` package from the [releases page](https://github.com/rchain/rchain/releases/)
2. `$ sudo rpm -U ./rnode_<VERSION>.noarch.rpm`, where `<VERSION>` is something like `0.9.18`

### macOS

1. Install Homebrew by following steps at the [Homebrew homepage](https://brew.sh/)
2. `$ brew install rchain/rchain/rnode`

## Running

Docker will be used in the examples port portability reasons, but running the
node as a standalone process is very similar.

To fetch the latest version of RNode from the remote Docker hub and run it
(exit with `C-c`):

```
$ docker run -v $HOME/tmp:/var/lib/rnode -ti -p 40400:40400 rchain/rnode:latest
```

In order to use both the peer-to-peer network and REPL capabilities of the
node, you need to run more than one Docker Rnode on the same host, the
containers need to be connected to one user-defined network bridge:

```bash
$ docker network create rnode-net

$ docker run -v $HOME/tmp/peer0:/var/lib/rnode -dit --name rnode0 --network rnode-net rchain/rnode:latest run -s --network localnet

$ docker ps
CONTAINER ID        IMAGE                      COMMAND             CREATED             STATUS              PORTS               NAMES
15aa78b45da4        rchain/rnode:latest        "/bin/main.sh -s"   3 seconds ago       Up 2 seconds                            rnode0
```

To attach terminal to RNode logstream execute

```bash
$ docker logs -f rnode0
[...]
08:38:11.460 [main] INFO  logger - Listening for traffic on rnode://137200d47b8bb0fff54a753aabddf9ee2bfea089@172.18.0.2?protocol=40400&discovery=40404
[...]
```

A repl instance can be invoked in a separate terminal using the following command:

```bash
$ docker run -v $HOME/tmp/rnode-repl:/var/lib/rnode -it --name rnode-repl --network rnode-net rchain/rnode:latest --grpc-host rnode0 repl

  ╦═╗┌─┐┬ ┬┌─┐┬┌┐┌  ╔╗╔┌─┐┌┬┐┌─┐  ╦═╗╔═╗╔═╗╦
  ╠╦╝│  ├─┤├─┤││││  ║║║│ │ ││├┤   ╠╦╝║╣ ╠═╝║
  ╩╚═└─┘┴ ┴┴ ┴┴┘└┘  ╝╚╝└─┘─┴┘└─┘  ╩╚═╚═╝╩  ╩═╝

rholang $
```

Type `5` in REPL console. This command should result in (`rnode0` output):
```bash
[...]
Evaluating:
5
```

A peer node can be started with the following command (note that `--bootstrap` takes the listening address of `rnode0`):

```bash
$ docker run -v $HOME/tmp/peer1:/var/lib/rnode -it --name rnode1 --network rnode-net rchain/rnode:latest run --network localnet --bootstrap 'rnode://ee00a5357f2f4cb58b08a8a4c949da1b@172.18.0.2?protocol=40400&discovery=40404'
[...]
08:58:36.267 [main] INFO  logger - Listening for traffic on rnode://d04b133a3a9a0209d8278713a0235b9fc0ec34f3@172.18.0.2?protocol=40400&discovery=40404.
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

To get a full list of options rnode accepts, use the `--help` option: `$ docker
run -it rchain/rnode --help`.

### Configuration file

Most of the command line options can be specified in a configuration file
`rnode.conf`.

The default location of the configuration file is the data directory. An
alternative location can be specified with the command line option
`--config-file <path>`.

The format of the configuration file is [HOCON](https://github.com/lightbend/config/blob/master/HOCON.md).

The [reference.conf](node/src/main/resources/reference.conf) configuration
file shows all options and default values.

Example configuration file:

```hocon
rnode {
  server {
    host = localhost
    upnp = false
    port = 40400
    port-http = 40403
    port-kademlia = 40404
    port-admin-http = 40405
    send-timeout = 2 seconds
    standalone = false
    bootstrap = "rnode://de6eed5d00cf080fc587eeb412cb31a75fd10358@52.119.8.109?protocol=40400&discovery=40404"
    data-dir = "/var/lib/rnode"
    store-size = 1G
    map-size = 1G
    max-connections = 500
    max-message-size = 256K

    tls {
      certificate = /etc/ssl/node.certificate.pem"
      key = /etc/ssl/node.key.pem
    }

    metrics {
      prometheus = false
      influxdb = true
      zipkin = false
      sigar = false
    }
  }

  grpc {
    host = localhost
    port-external = 40401
    port-internal = 40402
  }

  casper {
    # validator-public-key =
    # validator-private-key-path =
    # bonds-file =
    # known-validators-file =
    validators = 5
   }
}
```

## Development

Compile the project with:

```bash
$ sbt clean rholang/bnfc:clean rholang/bnfc:generate compile node/docker:publishLocal
```

Run the resulting binary with:

```bash
$ ./node/target/universal/stage/bin/rnode
```

For more detailed instructions, see the [developer guide](DEVELOPER.md).

## Caveats and filing issues

### Caveats

During this pre-release phase of the RChain software, there are some [known
issues](https://rchain.atlassian.net/wiki/spaces/CORE/pages/428376244/RChain+software+unresolved+bugs+and+known+issues).

### Filing Issues

File issues in our Public Jira Instance: [File a
bug](https://rchain.atlassian.net/secure/CreateIssueDetails!init.jspa?pid=10105&issuetype=10103&versions=10012&components=10004&assignee=medha&summary=issue+created%20via+link)

## Acknowledgements

We use YourKit to profile rchain performance.  YourKit supports open source
projects with its full-featured Java Profiler.  YourKit, LLC is the creator of
<a href="https://www.yourkit.com/java/profiler/">YourKit Java Profiler</a> and
<a href="https://www.yourkit.com/.net/profiler/">YourKit .NET Profiler</a>,
innovative and intelligent tools for profiling Java and .NET applications.

## Licence information

To get summary of licenses being used by the RChain's dependencies, simply run
`sbt node/dumpLicenseReport`. The report will be available under
`node/target/license-reports/rnode-licenses.html`
