# Language

* Ceremony master - TBD

# Mainnet Feature Requirements

## Network Launch

### UC: As a Coop SRE I want to launch a network

#### AC: A succesful genesis ceremony 

##### test: [todo: requires integration test]
##### steps:

* `ceremonyMaster` is instatantied with flags `--required-sigs 2 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB`.
* `validatorA` and `validatorB` joins p2p, both poining `ceremonyMaster` as bootstrap
* `ceremonyMaster` sends `UnapprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` receives `UnapprovedBlock`
* `validatorA` and `validatorB` send back `BlockApproval`
* `ceremonyMaster` transitions to `ApprovedBlockReceivedHandler`
* `ceremonyMaster` sends `ApprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` transition to ApprovedBlockReceivedHandler
* `ceremonyMaster`, `validatorA` and `validatorB` tip points to block (genesis) where it has no parent and Bonds holds `validatorA` and `validatorB`

#### AC: A succesful genesis ceremony with read-only nodes joining 

##### test: [todo: requires integration test]
##### steps:

* `ceremonyMaster` is instatantied with flags `--required-sigs 2 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB`.
* `validatorA` and `validatorB` joins p2p, both poining `ceremonyMaster` as bootstrap
* `readOnlyA`(read-only) joins p2p
* `ceremonyMaster` sends `UnapprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` receives `UnapprovedBlock`
* `validatorA` and `validatorB` send back `BlockApproval`
* `ceremonyMaster` transitions to `ApprovedBlockReceivedHandler`
* `ceremonyMaster` sends `ApprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` transition to ApprovedBlockReceivedHandler
* `ceremonyMaster`, `validatorA` and `validatorB` tip points to block (genesis) where it has no parent and Bonds holds `validatorA` and `validatorB`
* `readOnlyA` **never** transitions to `ApprovedBlockReceivedHandler`


#### AC: A NOT succesful genesis ceremony (not enough sigs)

##### test: [todo: requires integration test]
##### steps:

* `ceremonyMaster` is instatantied with flags `--required-sigs 3 --duration 5min --interval 10sec --bonds-file <holds two nodes validatorA and validatorB`.
* `validatorA` and `validatorB` joins p2p, both poining `ceremonyMaster` as bootstrap
* `ceremonyMaster` sends `UnapprovedBlock` to `validatorA` and `validatorB`
* `validatorA` and `validatorB` receives `UnapprovedBlock`
* `validatorA` and `validatorB` send back `BlockApproval`
* `ceremonyMaster` logs an error about not getting enough signatures on time (`duration`)


#### AC: A validator catching up after ceremony

##### test: [todo: requires integration test]
##### steps:

* genesis reach as described in [A succesful genesis ceremony](#user-content-ac-a-succesful-genesis-ceremony)
* `validatorC` joins p2p, pointing on `ceremonyMaster` as bootstrap
* `validatorC` sends `ApprovedBlockRequest` to `ceremonyMaster`
* `ceremonyMaster` sends `ApprovedBlock` to `validatorC`
* `validatorC` transitions to `ApprovedBlockReceivedHandler`
* `validatorC` tip points to block (genesis) where it has no parent and Bonds holds `validatorA` and `validatorB`

## Proof of stake consensus

### UC: As a dApp developer I want to be able to deploy my rholang contract to a validator

#### AC: A correct contract gets deployed successfully

##### test: [todo: requires integration test]

##### steps:

* instantiate p2p network with single `ceremonyMaster` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with `rholang/examples/tut-philosophers.rho` on `ceremonyMaster`
* assert a success on std out
* `rnode deploy` exit code should be 0

#### AC: An incorrect  contract does not get deployed 

##### test: [todo: requires integration test]

##### steps:

* instantiate p2p network with single `ceremonyMaster` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with invalid contract on `ceremonyMaster`
* assert a error logs on std out
* `rnode deploy` exit code should be 1
