[![Build Status](https://travis-ci.org/rchain/rchain.svg?branch=dev)](https://travis-ci.org/rchain/rchain)

# RChain

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

