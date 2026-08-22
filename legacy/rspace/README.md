# rspace

The **R**Chain Tuple **Space**

### Overview

`rspace` is a Scala library that has been designed to provide a disk-backed [tuple space](https://en.wikipedia.org/wiki/Tuple_space) to the [Rholang](http://rchain-architecture.readthedocs.io/en/latest/contracts/contract-design.html#rholang-a-concurrent-language) interpreter.  It is based on an earlier, in-memory implementation written in the Rosette Base Language (RBL) found [here](https://github.com/rchain/rchain/blob/05e3969cc999853ab57fb8c792e732beb92948d3/rosette/rbl/rosette/namespace.rbl).

Traditionally, a key-value store provides two main functions for interacting with a given store: *put*, which persists a piece of data at a given key, and *get*, which retrieves the data stored at a particular key.

`rspace` departs from this model and provides a novel approach to storing and retrieving data.

Here are some key differences:

* Rather than associating a piece of data with a particular key, in `rspace`, a piece of data is associated with a *channel*.  Roughly speaking, in `rspace`, a *channel* can be any Scala type that can be serialized to and deserialized from an `Array[Byte]`.

* In addition to storing data, `rspace` can also store *continuations*, which represent actions to carry out once the data is retrieved.  Roughly speaking, in `rspace`, a *continuation* can be any Scala type that can be serialized to and deserialized from an`Array[Byte]`.

* A continuation is associated with a list of channels and a list of *patterns*.  Like channels and continuations, patterns can be any Scala type that can be serialized to and deserialized from an `Array[Byte]`.  Users of `rspace` provide their own custom match function for matching patterns against data.

* The two main functions for interacting with a given store are:

  * *consume*, which searches the store for data matching a given list of patterns at a given list channels; and

  * *produce*, which, given a piece data at a given channel, searches the store for matching continuation, using existing data in the store to help satisfy the match.

For more information and detailed instructions on using `rspace`, see the tutorial [here](../docs/rspace/Tutorial.md).

### Prerequisites

* Java Development Kit (JDK), version 8
  * We recommend using the OpenJDK which can be installed using most common package managers.
  * Alternatively, the Oracle JDK is available [here](http://www.oracle.com/technetwork/java/javase/downloads/jdk8-downloads-2133151.html).
* sbt
  * Available [here](http://www.scala-sbt.org/download.html).

### Usage

To use `rspace` in a Scala project, add the following coordinates your build:

```
coop.rchain %% rspace % "0.1.1"
```

### Issues

Bug reports are welcome!  Please report any issues you have with `rspace` [here](https://rchain.atlassian.net/secure/CreateIssueDetails!init.jspa?pid=10105&issuetype=10103&versions=10005&assignee=henry&summary=issue+created%20via+link).

### License

This project is licensed under [The Apache 2.0 License](https://www.apache.org/licenses/LICENSE-2.0).

