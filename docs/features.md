# Mainnet Feature Requirements

## Node
## As a node operator, I want to install software from binary artifacts or a Docker image
### test: not available 
### steps: TBD
## As a node operator, I want to run software on Linux, MacOS, and in Docker
### test: not available 
### steps: TBD
## As a dApp developer, I want to interface with the Rholang interpreter and evaluate smart contracts independently from the blockchain
### A contract being run using rnode eval
### test: test/test_eval.py::test_eval
### steps:

* given that `rnode` is running
* user executes all contracts from `/rholang/examples/*.rho` using `rnode eval`
* program exists with 0 for each of the contract

### A contract being run using rnode repl
### test: test/test_repl.py::test_repl
### steps:

* given that `rnode` is running
* user executes all contracts from `/rholang/examples/*.rho` using `rnode repl`
* program exists with 0 for each of the contract

### REPL detects invalid rholang
### test: test/test_repl.py::test_repl_detects_invalid_rholang
### steps:

* given that `rnode` is running
* user executes rholang code that is "foo"
* program exists with 1 and prints out coop.rchain.rholang.interpreter.errorsTopLevelFreeVariablesNotAllowedError

## As a node operator, I want to have a default configuration and the ability to customize the configuration on the command line
### test: not available 
### steps: TBD
## As a node operator, I want to monitor the performance, resource consumption, and status of my node
### test: not available 
### steps: TBD
## As a node operator, I do not want to expose my private wallet key on the command line
### test: not available 
### steps: TBD

## Peer to Peer Network
## As a node operator, I want to be able to bootstrap to the network by connecting to any known node
### test: not available 
### steps: TBD
## As a node operator, once connected via a bootstrap node, I want to discover and connect to peers
### test: not available 
### steps: TBD
## As a node operator, I want to know how many peers I am connected to
### test: not available 
### steps: TBD

## Network Launch
## As a platform stakeholder, I want a Coop-governed, community-driven, and independently verifiable validation of the genesis block used to launch a network.
### test: not available 
### steps: TBD
## As a platform stakeholder, I want a Coop-goverend, community-driven, and independently verifiable successful genesis ceremony. 
### test: test/test_genesis_ceremony.py::test_successful_genesis_ceremony 
### steps:

* `ceremonyMaster` is instatantied with flags `--required-sigs 2 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB`.
* `validatorA` and `validatorB` joins p2p, both pointing to `ceremonyMaster` as bootstrap
* `ceremonyMaster` sends `UnapprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` receives `UnapprovedBlock`
* `validatorA` and `validatorB` send back `BlockApproval`
* `ceremonyMaster` transitions to `ApprovedBlockReceivedHandler`
* `ceremonyMaster` sends `ApprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` transition to ApprovedBlockReceivedHandler
* `ceremonyMaster`, `validatorA` and `validatorB` tip points to block (genesis) where it has no parent and Bonds holds `validatorA` and `validatorB`

## A successful genesis ceremony with read-only nodes joining 
### test: not available
### steps:

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


## A NOT successful genesis ceremony (not enough sigs)
### test: not available
### steps:

* `ceremonyMaster` is instatantied with flags `--required-sigs 3 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB`.
* `validatorA` and `validatorB` joins p2p, both pointing to `ceremonyMaster` as bootstrap
* `ceremonyMaster` sends `UnapprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` receives `UnapprovedBlock`
* `validatorA` and `validatorB` send back `BlockApproval`
* `ceremonyMaster` logs an error about not getting enough signatures on time (`duration`)

## As a node operator, I want to join the network after genesis ceremony, receive the genesis block, and know the state of the blockchain.
## A validator catching up after genesis ceremony
### test: not available
### steps:

* genesis reached as described in "A successful genesis ceremony"
* `validatorC` joins p2p, pointing on `ceremonyMaster` as bootstrap
* `validatorC` sends `ApprovedBlockRequest` to `ceremonyMaster`
* `ceremonyMaster` sends `ApprovedBlock` to `validatorC`
* `validatorC` transitions to `ApprovedBlockReceivedHandler`
* `validatorC` tip points to block (genesis) where it has no parent and Bonds holds `validatorA` and `validatorB`

## REV
### As a platform stakeholder, I want REV to be the currency token for the RChain platform.
### test: not available
### steps: TBD
## As a REV holder, assuming I maintain control of my keys and properly use the wallet where I store REV, I expect my REV to never be lost.
### test: not available 
### steps: TBD

## Wallets
## As a user, I want to be able to configure a coop-supplied wallet so that I can store REV in it
### test: not available 
### steps: TBD
## As a user, I want to be able to interface with the coop-supplied wallet at the command line.
### test: not available 
### steps: TBD
## As a user, I want to be able to add REV to my coop-supplied wallet so that I have available REV to pay for goods/services
### test: not available 
### steps: TBD
## As a user, I want to be able to remove REV from my coop-supplied wallet so that I can pay for goods/services
### test: not available 
### steps: TBD
## As a user, I want to be able to receive REV from another user by providing that user with the public key for my coop-supplied-wallet.
### test: not available
### steps: TBD
## As a user, I want to be able to send REV to the coop-supplied wallet of another user by specifying the public key to the coop-supplied wallet of that user.
### test: not available
### steps: TBD
## As a user of a coop-supplied wallet, I want to query a wallet contract (or the blocks) for my public address to get the history of all REV transfers to and/or from it
### test: not available 
### steps: TBD
## As a recipient of REV (other than REV at mainnet launch 'genesis'), I can use a co-op supplied dApp to view my REV balance
### test: not available 
### steps: TBD
## As a recipient of REV at mainnet launch 'genesis', I can use a co-op supplied wallet to view my REV balance after launch.
### test: not available 
### steps: TBD
## As an organization holding REV, I need to have multiple approviesr for any REV transaction.
### test: not available 
### steps: TBD
## As a validator, I can move Rev to/from the key-pair for one validator node to the key-pair for another validator node or that of the co-op supplied wallet dApp
### test: not available 
### steps: TBD
## As a wallet dApp developer, I want to use Ethereum-style addresses for send transactions to specify the recipient, so that a) I can reuse available Ethereum utility libraries; b) so the QR code is smaller and thus faster to scan than it would be for a full public key; c) it is easier for users to verbally confirm their address versus public key; and d) so RChain is more palatable for the Ethereum community
### test: not available 
### steps: TBD
## As a wallet dApp developer, I want to discover and connect to rNodes that support a particular version (release number and hash) and have a minimum number of active connections, so that user risks due to interoperability issues and sybil actors are minimized
### test: not available 
### steps: TBD
## As a wallet user, I need a command line interface for interacting with wallets.
### test: not available 
### steps: TBD
## As a dApp organization, I need to have multiple approvers for any send transaction.
### test: not available 
### steps: TBD
## Storage
## As a user, I want to be able to store transaction data on the blockchain so that it is available and accessible to users
### test: not available 
### steps: TBD
## As a user, I want to be able to store non-transaction data on the blockchain so that it is available and accessible to users
### test: not available 
### steps: TBD

## Validation
## As a RChain validator, I want my validator identity to be different from the identity of my node and from the identity of my wallet.
### test: not available
### steps: TBD
## As a RChain validator, I want to know when a block I propose is finalized and with what degree of confidence.
### test: not available
### steps: TBD
## As a RChain validator, I want to expose to the internet only those methods needed for production dApps.
### test: not available
### steps: TBD

## Validator bonding and unbonding
## As an RChain validator, I want to be able to add my stake to the network and be recognized as a validator so I can participate in proof of stake consensus and be eligible to earn rewards (validating)
### test: not available 
### steps: TBD
## As an incoming RChain validator, I need confirmation of my request to bond
### test: not available 
### steps: TBD
## As an RChain validator, I want to be able to retrieve my stake from the network and no longer be recognized as validator
### test: not available 
### steps: TBD
## As a platform stakeholder, I want to know only bonded validators are able to validate.
### test: not available
### steps: TBD

## Validator rewards
## As a RChain validator, I want to earn by validating on the RChain network.
### test: not available
### steps: TBD
## As a RChain validator, I want to receive the rewards I earn by validating on the RChain network.
### test: not available
### steps: TBD
## As a RChain validator, I want to retrieve the rewards I earn by validating on the RChain network.
### test: not available
### steps: TBD

## Validator slashing and ejection
## As a RChain validator and as a platform stakeholder, I want to know that other validators who do not validate according to the slashing API will be slashed (removed as a validator and lose stake) in a process observable to other validators.
### test: not available
### steps: TBD
## As a RChain validator, I want to know that if I am slashed then the Coop will hold my bond amount and not distribute it to other validators for a specified period of reconcilliation in the event that my slashing was unjustified.
### test: not available
### steps: TBD
## As a platform stakeholder, I want to see the stake of a slashed validator distributed to other validators after the time specified for a hold for reconciliation.
## As a platform stakeholder, I want to know that a validator that has been slashed is no longer able to validate.
### test: not available
### steps: TBD
## As a RChain validator, I want to update my RNode software without being slashed.
### test: not available
### steps: TBD
## As a RChain validator, I want to know that I will not be slashed and that I will be ejected if I meet the criteria as an underperforming validator and not the criteria for slashing.
### test: not available
### steps: TBD

## Contract deployment
## As a dApp developer, I want to be able to deploy my rholang contract to a validator
### test: A correct contract gets deployed successfully 
### steps: 
* instantiate p2p network with single `ceremonyMaster` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with `rholang/examples/tut-philosophers.rho` on `ceremonyMaster`
* assert a success on std out
* `rnode deploy` exit code should be 0

## As a dApp developer, I want to receive error messages if there are syntax errors to a contract I want to deploy, and I want to know an incorrect contract will not be deployed.
### test: not available
### steps:

* instantiate p2p network with single `ceremonyMaster` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with invalid contract on `ceremonyMaster`
* assert a error logs on std out
* `rnode deploy` exit code should be 1

## As a dApp developer, I want want a smart contract to be able to call another smart contract so that I can take advantage of another contract's functionality.
### test: not available
### steps: TBD

## As a dApp developer, I want contract definition inside another contract (ex WIDE, persisting contracts).
### test: not available
### steps: TBD

## As a dApp developer, I want support for binary files as part of deployments.
### test: not available
### steps: TBD

## As a dApp developer, I want STDOUT to go back to gRPC response and not the log.
### test: not available
### steps: TBD

## As a dApp developer, when I make one or more deployments, I want a receipt of the transaction.
### test: not available
### steps: TBD

## Name registry
## As a dApp developer, I want to predict registry names using readily available crypto libraries.
### test: not available
### steps: TBD

## Cost accounting
## As a node operator, I want to get the list of deploys and their costs when I run `show-blocks`.
### test: not avaialble 
### steps: TBD
## As a node operator, I want to be compensated for storing, running, and processing transactions against smart contracts based on the computational intensity required.
### test: not avaialble 
### steps: TBD
### test: not avaialble 
### steps: TBD
## As a node operator, I want to be compensated 
## As a validator, I want to receive interest in phlo on my bond amount as defined by the schedule in the mint.
### test: not avaialble 
### steps: TBD
## As a validator, having an absolute minimum phlo cost to deploy a contract helps support my resources and mitigate against DOS attacks.
### test: not avaialble 
### steps: TBD
## As a validator, to support management of my resources I want to receive as a fee either a minimum charge for a deployment or a charge determined as a percentage of the size of the contract. The fee will be whichever is greater: the absolute minimum charge or the calculated minimum.
### test: not avaialble 
### steps: TBD
## As a dApp developer, I use phlo to pay the cost to deploy smart contracts on the RChain platform.
### test: not avaialble 
### steps: TBD
## As a dApp developer, I need to know the minimum cost it will take to deploy my contract. The cost will be whichever is greater: the absolute minimum charge or the calculated minimum.
## As a dApp developer, I need to know how much it will cost to execute my contract.
### test: not avaialble 
### steps: TBD
## As a dApp developer, I want the phlo cost to execute a contract a contract once it's on chain to be consistent given the asm REV to phlo price and accounting for non-determinism.
### test: not avaialble 
### steps: TBD
## As a client of a contract already deployed on the blockchain, I need to know how much it will cost to execute the contract.
### test: not avaialble 
### steps: TBD
## As a validator, I will yield 0.01% of all transaction fees received to accounts controlled by the Coop.
### test: not avaialble 
### steps: TBD
## As the RChain coop, I want to receive 0.01% of all transaction fees to accounts controlled by the Coop.
### test: not avaialble 
### steps: TBD

## Storage
## As a platform user, I want to store transaction data on the blockchain so it is available and accessible to other users.
### test: not available 
### steps: TBD

## Performance
## As a user of the platform, I want to know the current performance of the network in terms of COMM events per second.
### test: not available
### steps: TBD

## Platform administration
## As the RChain coop, I need to verify how much total phlo over a period of time was paid out of a sending address.
### test: not available
### steps: TBD

## Documentation
## As a any type of platform stakeholder, I want a single-source of indexed documenation.
### documentation location: not available
## As a any type of platform stakeholder, I want to know what I can expect for the performance and reliability of the RChain platform im the form of a production engineering plan.
### documentation location: not available
## As a node operator, I want to learn how to install, run, operate, and monitor my node.
### documentation location: not available
## As a validator, I want to learn the minimum hardware, infrastructure, and networking requirements to participate as a validator.
### documentation location: not available
## As a validator, I want to learn about the slashing API, the conditions and process for slashing, and the conditions and process for ejection.
### documentation location: not available
## As a dApp developer, I want to learn how to use and write smart contracts in Rholang.
### documentation location: not available
## As a dApp developer, I want to formally verify my Rholang contracts.
### documentation location: not available
## As a dApp developer, I need to learn about COMM events (ex what are they? how are they measured? how are they part of the platform performance benchmark metric?) and how to optimize them in my smart contracts.
### documentation location: not available
## As a dApp developer, I need to learn how unforgeable names are generated, how to determine unforgeable names, and how to retrieve the UUID of the unforgeable names.
### documentation location: not available
## As a dApp developer, I need a reference for gRPC calls and a description of what they do.
### documentation location: not available
## As a dApp developer, want to know how to calculate the costs for the execution of my contracts, including accounting for non-determinism.
## As an Ehtereum developer familiar with the Infura API, I want to learn how to use the RChain platform.
### documentation location: not available

## Not_Grouped
## All existing tests that need proper user story
## Heterogeneous validators
### test: test/test_heterogenous_validators.py::test_heterogenous_validators
### steps:TBD

## Count from show blocks
### test: test/test_internal.py::test_blocks_count_from_show_blocks
### steps:TBD

## Count from show blocks
### test: test/test_internal.py::test_extract_block_hash_from_propose_output
### steps:TBD
