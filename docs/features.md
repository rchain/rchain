# Language

* Ceremony master - TBD

# Mainnet Feature Requirements

## Network Launch

### UC: As a Coop SRE I want to launch a network

#### AC: A succesful genesis ceremony 

##### test: [todo: requires integration test]
##### steps:

* `nodeA`(ceremony-master) is instatantied with flags `--required-sigs 2 --duration 5min --interval 10sec --bonds-file <holds two nodes nodeB and nodeC`.
* `nodeB`(validator) and `nodeC`(validator) joins p2p, both poining `nodeA` as bootstrap
* `nodeA` sends `UnapprovedBlock` to `nodeB` and `nodeC`
* `nodeB` and `nodeC` receives `UnapprovedBlock`
* `nodeB` and `nodeC` send back `BlockApproval`
* `nodeA` transitions to `ApprovedBlockReceivedHandler`
* `nodeA` sends `ApprovedBlock` to `nodeB` and `nodeC`
* `nodeB` and `nodeC` transition to ApprovedBlockReceivedHandler
* `show-blocks` on `nodeA` shows single block (genesis) where Bonds holds `nodeB` and `nodeC`
* `show-blocks` on `nodeB` shows single block (genesis) where Bonds holds `nodeB` and `nodeC`
* `show-blocks` on `nodeC` shows single block (genesis) where Bonds holds `nodeB` and `nodeC`

#### AC: A succesful genesis ceremony with read-only nodes joining 

##### test: [todo: requires integration test]
##### steps:

* `nodeA`(ceremony-master) is instatantied with flags `--required-sigs 2 --duration 5min --interval 10sec --bonds-file <holds two nodes nodeB and nodeC`.
* `nodeB`(validator) and `nodeC`(validator) joins p2p, both poining `nodeA` as bootstrap
* `nodeD`(read-only) joins p2p
* `nodeA` sends `UnapprovedBlock` to `nodeB` and `nodeC`
* `nodeB` and `nodeC` receives `UnapprovedBlock`
* `nodeB` and `nodeC` send back `BlockApproval`
* `nodeA` transitions to `ApprovedBlockReceivedHandler`
* `nodeA` sends `ApprovedBlock` to `nodeB` and `nodeC`
* `nodeB` and `nodeC` transition to ApprovedBlockReceivedHandler
* `show-blocks` on `nodeA` shows single block (genesis) where Bonds holds `nodeB` and `nodeC`
* `show-blocks` on `nodeB` shows single block (genesis) where Bonds holds `nodeB` and `nodeC`
* `show-blocks` on `nodeC` shows single block (genesis) where Bonds holds `nodeB` and `nodeC`
* `nodeD` **never** transitions to `ApprovedBlockReceivedHandler`


#### AC: A NOT succesful genesis ceremony (not enough sigs)

##### test: [todo: requires integration test]
##### steps:

* `nodeA`(ceremony-master) is instatantied with flags `--required-sigs 3 --duration 5min --interval 10sec --bonds-file <holds two nodes nodeB and nodeC`.
* `nodeB`(validator) and `nodeC`(validator) joins p2p, both poining `nodeA` as bootstrap
* `nodeA` sends `UnapprovedBlock` to `nodeB` and `nodeC`
* `nodeB` and `nodeC` receives `UnapprovedBlock`
* `nodeB` and `nodeC` send back `BlockApproval`
* `nodeA` logs an error about not getting enough signatures on time (`duration`)


#### AC: A validator catching up after ceremony

##### test: [todo: requires integration test]
##### steps:

* genesis reach as described in [A succesful genesis ceremony](#user-content-ac-a-succesful-genesis-ceremony)
* `nodeD`(validator) joins p2p, pointing on `nodeA` as bootstrap
* `nodeD` sends `ApprobedBlockRequest` to `nodeA`
* `nodeA` sends `ApprovedBlock` to `nodeD`
* `nodeD` transitions to `ApprovedBlockReceivedHandler`
* `show-blocks` on `nodeD` shows single block (genesis) where Bonds holds `nodeB` and `nodeC`

## Proof of stake consensus

### UC: As a dApp developer I want to be able to deploy my rholang contract to a validator

#### AC: A correct contract gets deployed successfully

##### test: [todo: requires integration test]

##### steps:

* instantiate p2p network with single `nodeA` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with `rholang/examples/tut-philosophers.rho` on `nodeA`
* assert a success on std out
* `rnode deploy` exit code should be 0

#### AC: An incorrect  contract does not get deployed 

##### test: [todo: requires integration test]

##### steps:

* instantiate p2p network with single `nodeA` that transitions to `ApprovedBlockReceivedhandler` (`--required-sig 0`)
* call `rnode deploy` with invalid contract on `nodeA`
* assert a error logs on std out
* `rnode deploy` exit code should be 1
