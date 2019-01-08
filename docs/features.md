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

## Deployment
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

### As a Node Validator, I want to be able to retrieve my stake from the network and no longer be recognized a as validator
### As an incoming Node Validator, I need confirmation of my request to bond
### As a platform stakeholder, I want to know only bonded validators are able to propose
#### TBD
##### documentation
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



