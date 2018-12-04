# Mainnet Feature Requirements

## Nodes
### As a Node Operator, I want to install software from binary artifacts or a Docker image
### As a Node Operator, I want to run software on Linux, MacOS, and in Docker
### As a dApp Developer, I want to interface with the Rholang interpreter and evaluate smart contracts independently from the blockchain
### As a Node Operator, I want to have a default configuration and the ability to customize the configuration on the command line
### As a Node Operator, I want to monitor the performance, resource consumption, and status of my node
### As a Node Operator, I do not want to expose my private wallet key on the command line
## Peer to Peer Network
### As a Node operator, I want to be able to bootstrap to the network by connecting to any known node
### As a Node operator, once connected via a bootstrap node, I want to discover and connect to peers
### As a Node operator, I want to know how many peers I am connected to
## Network Launch
### As a Coop SRE I want to launch a network
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
##### test: not available
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
##### test: not available
##### steps:

* `ceremonyMaster` is instatantied with flags `--required-sigs 3 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB`.
* `validatorA` and `validatorB` joins p2p, both pointing to `ceremonyMaster` as bootstrap
* `ceremonyMaster` sends `UnapprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` receives `UnapprovedBlock`
* `validatorA` and `validatorB` send back `BlockApproval`
* `ceremonyMaster` logs an error about not getting enough signatures on time (`duration`)

### As a Node operator I want to join the network after genesis ceremony
#### A validator catching up after genesis ceremony
##### test: not available
##### steps:

* genesis reached as described in "A successful genesis ceremony"
* `validatorC` joins p2p, pointing on `ceremonyMaster` as bootstrap
* `validatorC` sends `ApprovedBlockRequest` to `ceremonyMaster`
* `ceremonyMaster` sends `ApprovedBlock` to `validatorC`
* `validatorC` transitions to `ApprovedBlockReceivedHandler`
* `validatorC` tip points to block (genesis) where it has no parent and Bonds holds `validatorA` and `validatorB`

## Wallets
### As a user, I want to be able to create a wallet so that I can store REV in it
### As a user, I want to be able to add REV to my wallet so that I have available REV to pay for goods/services
### As a user, I want to be able to remove REV from my wallet so that I can pay for goods/services
### Expose purses inside a wallet
### As a wallet application, I want to query a wallet contract (or the blocks) for the history of all Rev transfers to/from it
### As a REV holder, I can move some of my REV to the control another user’s public key (or address) via a co-op supplied dApp wallet
### As a recipient of REV (other than Genesis REV), I can use a co-op supplied dApp to view my REV balance
### As a validator, I can move Rev to/from the key-pair for one validator node to the key-pair for another validator node or that of the co-op supplied wallet dApp
### As a wallet dApp developer, I want to use Ethereum-style addresses for send transactions to specify the recipient, so that a) I can reuse available Ethereum utility libraries; b) so the QR code is smaller and thus faster to scan than it would be for a full public key; c) it is easier for users to verbally confirm their address versus public key; and d) so RChain is more palatable for the Ethereum community
### As a wallet dApp developer, I want to discover and connect to rNodes that support a particular version (release number and hash) and have a minimum number of active connections, so that user risks due to interoperability issues and sybil actors are minimized
### As a wallet user, I need a command line interface for interacting with wallets.
### As a dApp organization, I need to have multiple approvers for any send transaction.
## Storage
### As a user, I want to be able to store transaction data on the blockchain so that it is available and accessible to users
### As a user, I want to be able to store non-transaction data on the blockchain so that it is available and accessible to users
## Bonding/Unbonding
### As a Node Validator, I want to be able to add my stake to the network and be recognized as a validator so I can participate in proof of stake consensus and be eligible to earn rewards (validating)
### As a Node Validator, I want to be able to retrieve my stake from the network and no longer be recognized a as validator
### As an incoming Node Validator, I need confirmation of my request to bond
## Consensus
### As a dApp developer I want to be able to deploy my rholang contract to a validator
#### A correct contract gets deployed successfully
##### test: not available
##### steps:

* instantiate p2p network with single `ceremonyMaster` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with `rholang/examples/tut-philosophers.rho` on `ceremonyMaster`
* assert a success on std out
* `rnode deploy` exit code should be 0

#### An incorrect  contract does not get deployed 
##### test: not available
##### steps:

* instantiate p2p network with single `ceremonyMaster` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with invalid contract on `ceremonyMaster`
* assert a error logs on std out
* `rnode deploy` exit code should be 1

## External tooling
### As a dApp developer I can run arbitrary rholang contracts on a node
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

## Not_Grouped
### All existing tests that need proper user story
#### Heterogeneous validators
##### test: test/test_heterogenous_validators.py::test_heterogenous_validators
##### steps:

* TBD

#### Count from show blocks
##### test: test/test_internal.py::test_blocks_count_from_show_blocks
##### steps:

* TBD

#### Count from show blocks
##### test: test/test_internal.py::test_extract_block_hash_from_propose_output
##### steps:

* TBD
