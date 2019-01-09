# Mainnet Feature Requirements

## Nodes
### As a Node Operator, I want to install software from binary artifacts or a Docker image
#### Install rnode using tarball
##### documentation: https://github.com/rchain/rchain/#installing-and-running-on-debian-from-deb-package
#### Install rnode using docker
##### documentation: https://github.com/rchain/rchain/#installing-and-running-on-docker
### As a Node Operator, I want to run software on Linux, macOS, and in Docker
#### Run rnode on Linux
##### documentation: https://github.com/rchain/rchain/#installing-and-running-on-debian-from-deb-package
#### Run rnode on macOS
##### documentation: https://github.com/rchain/rchain/#installing-and-running-on-macos-via-homebrew
#### Run rnode on docker
##### documentation: https://github.com/rchain/rchain/#installing-and-running-on-docker
### As a dApp Developer, I want to interface with the Rholang interpreter and evaluate smart contracts independently from the blockchain
#### A contract being run using rnode eval
##### test: test/test_eval.py::test_eval
##### steps:

* given that `rnode` is running
* user executes all contracts from `/rholang/examples/*.rho` using `rnode eval`
* program exists with 0 for each of the contract

#### A contract being run using rnode repl
##### test: test/test_repl.py::test_repl
##### steps:

* given that `rnode` is running
* user executes all contracts from `/rholang/examples/*.rho` using `rnode repl`
* program exists with 0 for each of the contract

#### REPL detects invalid rholang
##### test: test/test_repl.py::test_repl_detects_invalid_rholang
##### steps:

* given that `rnode` is running
* user executes rholang code that is "foo"
* program exists with 1 and prints out coop.rchain.rholang.interpreter.errorsTopLevelFreeVariablesNotAllowedError

### As a Node Operator, I want to have a default configuration and the ability to customize the configuration on the command line
#### Configure rnode using rnode.toml
##### documentation: https://github.com/rchain/rchain/#configuration-file
#### Configure rnode using command line flags
##### documentation: https://github.com/rchain/rchain/#configuration-flags
### As a Node Operator, I want to monitor the performance, resource consumption, and status of my node
#### Monitor resource consumption and status
##### documentation: https://github.com/rchain/rchain/#monitor-resource-consumption

#### Monitor performance
##### documentation: https://github.com/rchain/rchain/#monitor-performance

### As a Node Operator, when I start rnode, I want to provide my validator key and wallet key
#### Start node with validator key and wallet key
##### documentation: https://github.com/rchain/rchain/#starting-node-as-a-validator
## Peer to Peer Network
### As a Node operator, I want to be able to bootstrap to the network by connecting to any known node
#### connecting to existing node
##### test: test/test_p2p.py::test_connecting_to_existing_node
##### steps:

* given that `standalone` is a running node in a standalone mode
* start new node with `--bootstrap` pointing to `standalone`
* node should succesfully start and connect to `standalone` via protocol handshake

#### connecting to non-existing node
##### test: test/test_p2p.py::test_connecting_to_non_existing_node
##### steps:

* start new node with `--bootstrap` pointing to some non-existing address
* node should exit
* exit code should be 1
* node should log that bootstrap could not been found

### As a Node operator, once connected via a bootstrap node, I want to discover and connect to peers
#### discover other nodes
##### test: test/test_p2p.py::test_discover_other_nodes
##### steps:

* create a p2p network with 3 nodes `nodaA`, `nodeB` and `nodeC`
* join p2p network as described in "As a Node operator, I want to be able to bootstrap to the network by connecting to any known node" bootstaping from `nodeA`
* after a period of time new node discovers `nodeB` and `nodeC`
* after a period of time new node connects (via protocol handshake) with `nodeB` and `nodeC`

### As a Node operator, I want to know how many peers I am connected to
#### number of protocol peers
##### test: test/test_p2p.py::test_number_of_protocol_peers
##### steps:

* create a p2p network with 3 nodes `nodaA`, `nodeB` and `nodeC`
* access `nodeA` http endpoint under `/info` should print `connected_peers = 2`
* access `nodeA` http endpoint under `/peers` to list `nodeB` and `nodeC` in JSON format

#### number of protocol peers
##### test: test/test_p2p.py::test_number_of_discovery_peers
##### steps:

* create a p2p network with 3 nodes `nodaA`, `nodeB` and `nodeC`
* access `nodeA` http endpoint under `/info` should print `discovered_peers = 2`
* access `nodeA` http endpoint under `/discovered-peers` to list `nodeB` and `nodeC` in JSON format

## Network Launch
### As a platform stakeholder, I want a Coop-governed, community-driven, and independently verifiable validation of the genesis block used to launch a network
### As a platform stakeholder, I want a Coop-goverend, community-driven, and independently verifiable successful genesis ceremony
#### A successful genesis ceremony 
##### test: test/test_genesis_ceremony.py::test_successful_genesis_ceremony 
##### steps:

* `ceremonyMaster` is instatantied with flags `--required-sigs 2 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB`.
* `validatorA` and `validatorB` joins p2p, both pointing to `ceremonyMaster` as bootstrap
* `ceremonyMaster` sends `UnapprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` receives `UnapprovedBlock`
* `validatorA` and `validatorB` send back `BlockApproval`
* `ceremonyMaster` transitions to `ApprovedBlockReceivedHandler`
* `ceremonyMaster` sends `ApprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` transition to ApprovedBlockReceivedHandler
* `ceremonyMaster`, `validatorA` and `validatorB` tip points to block (genesis) where it has no parent and Bonds holds `validatorA` and `validatorB`

#### A successful genesis ceremony with read-only nodes joining 
##### test: test/test_genesis_ceremony.py::test_successful_genesis_ceremony_with_read_only
##### steps:

* `ceremonyMaster` is instatantied with flags `--required-sigs 2 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB`.
* `validatorA` and `validatorB` joins p2p, both pointing to `ceremonyMaster` as bootstrap
* `readOnlyA`(read-only) joins p2p
* `ceremonyMaster` sends `UnapprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` receives `UnapprovedBlock`
* `validatorA` and `validatorB` send back `BlockApproval`
* `ceremonyMaster` transitions to `ApprovedBlockReceivedHandler`
* `ceremonyMaster` sends `ApprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` transition to ApprovedBlockReceivedHandler
* `ceremonyMaster`, `validatorA` and `validatorB` tip points to block (genesis) where it has no parent and Bonds holds `validatorA` and `validatorB`
* `readOnlyA` **never** transitions to `ApprovedBlockReceivedHandler`


#### A NOT successful genesis ceremony (not enough sigs)
##### test: test/test_genesis_ceremony.py::test_not_successful_genesis_ceremony
##### steps:

* `ceremonyMaster` is instatantied with flags `--required-sigs 3 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB`.
* `validatorA` and `validatorB` joins p2p, both pointing to `ceremonyMaster` as bootstrap
* `ceremonyMaster` sends `UnapprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` receives `UnapprovedBlock`
* `validatorA` and `validatorB` send back `BlockApproval`
* `ceremonyMaster` logs an error about not getting enough signatures on time (`duration`)

### As a Node operator, I want to join the network after genesis ceremony, receive the genesis block, and know the state of the blockchain
#### A validator catching up after genesis ceremony
##### test: test/test_genesis_ceremony.py::test_validator_catching_up
##### steps:

* genesis reached as described in "A successful genesis ceremony"
* `validatorC` joins p2p, pointing on `ceremonyMaster` as bootstrap
* `validatorC` sends `ApprovedBlockRequest` to `ceremonyMaster`
* `ceremonyMaster` sends `ApprovedBlock` to `validatorC`
* `validatorC` transitions to `ApprovedBlockReceivedHandler`
* `validatorC` tip points to block (genesis) where it has no parent and Bonds holds `validatorA` and `validatorB`

## Contract deployment
### As a dApp developer I want to be able to deploy my rholang contract to a validator
#### A correct contract gets deployed successfully
##### test: test/test_deployment.py::test_simple_deploy
##### steps:

* instantiate p2p network with single `ceremonyMaster` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with `rholang/examples/tut-philosophers.rho` on `ceremonyMaster`
* assert a success on std out
* `rnode deploy` exit code should be 0

#### An incorrect contract does not get deployed 
##### test: test/test_deployment.py::test_incorrect_contract_does_not_deploy
##### steps:

* instantiate p2p network with single `ceremonyMaster` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with invalid contract on `ceremonyMaster`
* assert a error logs on std out
* `rnode deploy` exit code should be 1

### As a user I want to be able to store data using a rholang contract in the tuplespace. 
#### A contract pointing to some data gets deployed, the data gets fetched and asserted.
##### test: test/test_storage.py::test_data_is_stored_and_served_by_node
##### steps:

* instantiate p2p network with single `ceremonyMaster` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` & `rnode propose` with `integration-tests/features/contracts/storage/store-data.rho` on `ceremonyMaster`
* assert success on std out
* call `rnode deploy` & `rnode propose` with `integration-tests/features/contracts/storage/read-data.rho` on `ceremonyMaster`
* assert success on std out
* compare data sent and restored

### As a user I want to know how much REV my deployment will cost

### As a dApp developer, I want a contract to be able to call another contract so that I can take advantage of another contract's functionality
### As a dApp developer, I want contract definition inside another contract (ex WIDE, persisting contracts).
### As a dApp developer, I want support for including binary files as part of deployments.
### As a dApp developer, I want STDOUT to go back to gRPC response and not the log.
### As a dApp developer, when I make one or more deployments, I want a receipt of the transaction.

## REV
### As a platform stakeholder, I want REV to be the currency token for the RChain platform
### As a REV holder, assuming I maintain control of my keys and properly use the wallet where I store REV, I expect my REV to never be lost
## Wallets
### As a user, I want to be able to configure a coop-supplied wallet so that I can store REV in it
### As a user, I want to be able to interface with the coop-supplied wallet at the command line.
### As a user, I want to be able to add REV to my coop-supplied wallet so that I have available REV to pay for goods/services
### As a user, I want to be able to remove REV from my coop-supplied wallet so that I can pay for goods/services
### As a user, I want to be able to receive REV from another user by providing that user with the public key for my coop-supplied-wallet.
### As a user, I want to be able to send REV to the coop-supplied wallet of another user by specifying the public key to the coop-supplied wallet of that user.
### As a user of a coop-supplied wallet, I want to query a wallet contract (or the blocks) for my public address to get the history of all REV transfers to and/or from it
### As a recipient of REV (other than REV at mainnet launch 'genesis'), I can use a co-op supplied dApp to view my REV balance
### As a recipient of REV at mainnet launch 'genesis', I can use a co-op supplied wallet to view my REV balance after launch.
### As an organization holding REV, I need to have multiple approviesr for any REV transaction.
### As a validator, I can move Rev to/from the key-pair for one validator node to the key-pair for another validator node or that of the co-op supplied wallet dApp
### As a wallet dApp developer, I want to use Ethereum-style addresses for send transactions to specify the recipient, so that a) I can reuse available Ethereum utility libraries; b) so the QR code is smaller and thus faster to scan than it would be for a full public key; c) it is easier for users to verbally confirm their address versus public key; and d) so RChain is more palatable for the Ethereum community
### As a wallet dApp developer, I want to discover and connect to rNodes that support a particular version (release number and hash) and have a minimum number of active connections, so that user risks due to interoperability issues and sybil actors are minimized
### As a wallet user, I need a command line interface for interacting with wallets.
### As a dApp organization, I need to have multiple approvers for any send transaction.
## Validation
### As a RChain validator, I want my validator identity to be different from the identity of my node and from the identity of my wallet
#### Documentation of identities
##### documentation: https://github.com/rchain/rchain/#identities
### As a RChain validator, I want to know when a block I propose is finalized and with what degree of confidence
#### Check fault tolerance of block
##### test: test/test_dag_correctness.py::test_fault_tolerance
##### steps:

* (this needs better set of steps, not relying on CliqueOracleTest)
* run p2p network
* duplicate steps in CliqueOracleTest to get get a DAG of known shape
* run show-block on each block and assert known fault tolerance

### As a RChain validator, I want to expose to the internet only those methods needed for production dApps
#### external and internal api documentation
##### documentation: https://github.com/rchain/rchain/#external-and-internal-api
## Bonding/Unbonding
### As a Node Validator, I want to be able to add my stake to the network and be recognized as a validator so I can participate in proof of stake consensus and be eligible to earn rewards (validating)
#### Bonding a validator
##### test: test/test_heterogenous_validators.py::test_heterogenous_validators
##### steps:

* TBD

## Validator rewards
### As a RChain validator, I want to earn by validating on the RChain network.
### As a RChain validator, I want to receive the rewards I earn by validating on the RChain network.
### As a RChain validator, I want to retrieve the rewards I earn by validating on the RChain network.

## Validator slashing and ejection
### As a RChain validator and as a platform stakeholder, I want to know that other validators who do not validate according to the slashing API will be slashed (removed as a validator and lose stake) in a process observable to other validators.
### As a RChain validator, I want to know that if I am slashed then the Coop will hold my bond amount and not distribute it to other validators for a specified period of reconcilliation in the event that my slashing was unjustified.
### As a platform stakeholder, I want to see the stake of a slashed validator distributed to other validators after the time specified for a hold for reconciliation.
### As a platform stakeholder, I want to know that a validator that has been slashed is no longer able to validate.
### As a RChain validator, I want to update my RNode software without being slashed.
### As a RChain validator, I want to know that I will not be slashed and that I will be ejected if I meet the criteria as an underperforming validator and not the criteria for slashing.

## Consensus
### As a Node Validator, who joins existing network, my node can catch up and finalize the same set of blocks that other nodes are finalizing
#### Catch up triggerd by next round from other validator
##### test: test/test_dag_correctness.py::test_catch_up_next_round
##### steps:

* initiate p2p with 3 validators `validatorA`, `validatorB`, `validatorC`
* each validator runs 100 rounds of deploy and propose
* wait graceful period of 30 seconds
* validator `validatorD` joins the network
* `validatorA` runs deploy and propose
* wait graceful period of 30 seconds
* each validator should have a DAG with same set of finalized blocks

#### Catch up automatically
##### test: test/test_dag_correctness.py::test_catch_up
##### steps:

* initiate p2p with 3 validators `validatorA`, `validatorB`, `validatorC`
* each validator runs 10 rounds of deploy and propose
* wait graceful period of 30 seconds
* validator `validatorD` joins the network
* wait graceful period of 10 seconds
* each validator should have a DAG with same set of finalized blocks

### As a Node Validator I want consensus protocol to converge
#### 5 validators deploying 200 blocks end up with the same DAG
##### test: test/test_dag_correctness.py::test_5val_200blocks
##### steps:

* initiate p2p with 5 validators `validatorA`, `validatorB`, `validatorC`, `validatorD` and `validatorE`
* each validator runs 200 rounds of deploy and propose
* wait graceful period of 30 seconds
* each validator should output exactly same DAG

# Cost accounting
### As a node operator, I want to get the list of deploys and their costs when I run `show-blocks`.
### As a node operator, I want to be compensated for storing, running, and processing transactions against smart contracts based on the computational intensity required.
### As a node operator, I want to be compensated 
### As a validator, I want to receive interest in phlo on my bond amount as defined by the schedule in the mint.
### As a validator, having an absolute minimum phlo cost to deploy a contract helps support my resources and mitigate against DOS attacks.
### As a validator, to support management of my resources I want to receive as a fee either a minimum charge for a deployment or a charge determined as a percentage of the size of the contract. The fee will be whichever is greater: the absolute minimum charge or the calculated minimum.
### As a dApp developer, I use phlo to pay the cost to deploy smart contracts on the RChain platform.
### As a dApp developer, I need to know the minimum cost it will take to deploy my contract. The cost will be whichever is greater: the absolute minimum charge or the calculated minimum.
### As a dApp developer, I need to know how much it will cost to execute my contract.
### As a dApp developer, I want the phlo cost to execute a contract a contract once it's on chain to be consistent given the asm REV to phlo price and accounting for non-determinism.
### As a client of a contract already deployed on the blockchain, I need to know how much it will cost to execute the contract.
### As a validator, I will yield 0.01% of all transaction fees received to accounts controlled by the Coop.
### As the RChain coop, I want to receive 0.01% of all transaction fees to accounts controlled by the Coop.

## Name registry
### As a dApp developer, I want to predict registry names using readily available crypto libraries.

## Performance
### As a user of the platform, I want to know the current performance of the network in terms of COMM events per second.

## Platform administration
### As the RChain coop, I need to verify how much total phlo over a period of time was paid out of a sending address.
## Documentation
### As a any type of platform stakeholder, I want a single-source of indexed documenation.
### As a any type of platform stakeholder, I want to know what I can expect for the performance and reliability of the RChain platform im the form of a production engineering plan.
### As a node operator, I want to learn how to install, run, operate, and monitor my node.
### As a validator, I want to learn the minimum hardware, infrastructure, and networking requirements to participate as a validator.
### As a validator, I want to learn about the slashing API, the conditions and process for slashing, and the conditions and process for ejection.
### As a dApp developer, I want to learn how to use and write smart contracts in Rholang.
### As a dApp developer, I want to formally verify my Rholang contracts.
### As a dApp developer, I need to learn about COMM events (ex what are they? how are they measured? how are they part of the platform performance benchmark metric?) and how to optimize them in my smart contracts.
### As a dApp developer, I need to learn how unforgeable names are generated, how to determine unforgeable names, and how to retrieve the UUID of the unforgeable names.
### As a dApp developer, I need a reference for gRPC calls and a description of what they do.
### As a dApp developer, want to know how to calculate the costs for the execution of my contracts, including accounting for non-determinism.
### As an Ehtereum developer familiar with the Infura API, I want to learn how to use the RChain platform.

## Not_Grouped
### Counting blocks?
#### Count from show blocks
##### test: test/test_internal.py::test_blocks_count_from_show_blocks
##### steps:

* TBD

#### Count from show blocks
##### test: test/test_internal.py::test_extract_block_hash_from_propose_output
##### steps:

* TBD



