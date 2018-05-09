
# Namespaces and Scaling in Mercury
[TOC]

## Structure

Broadly speaking, the Mercury RChain network is a directed acyclic graph ("dag") labeled by complete lattices.  The **organizational dag** is a rooted dag formed by superimposing a collection of trees.  Each **organizational tree** in the collection has a collection of vertices, called **locales**; locales may be shared between different organizational trees.  Each locale has a collection of **regions** that form the basis of a complete lattice of **regional namespaces**.  Each regional namespace has a countably infinite collection of **names** formed by quoting Rholang processes.

![](https://i.imgur.com/DniUajo.png)

### Locales

#### Regions and Regional Namespaces
Each locale has a set of **regions** that forms the basis for a complete lattice of **regional namespaces**.  For example, if a locale had 3 regions A, B, and C, it would have 2³=8 regional namespaces ⊥, A, B, C, A∨B, A∨C, B∨C, and A∨B∨C.

Regions set performance criteria that participating validators must meet or be ejected; they also set the staking requirements.

#### Names

Names are quoted Rholang processes.  The quotation is denoted syntactically by an at-sign `@`.  

Each name belongs to a regional namespace.  Ground terms belong to the bottom regional namespace ⊥; names declared using the `new` keyword belong to the namespace in which the code runs by default, but may have a regional namespace annotation to create a name in a different regional namespace; and all other quoted processes belong to the namespace of the join of all the names in the quoted process.

#### Structural and behavioral namespaces
A **structural namespace** is a collection of names that share a common portion of their abstract syntax tree.  Dynamic structural namespaces are supported as patterns in the `contract`, `for`, and `match` productions.  Static structural namespaces are under development and will be checked at deployment time.

A **behavioral namespace** is a collection of names whose evaluations all share some common use of the reduction rules of the language.  Behavioral namespaces will not be supported in the Mercury release.

Behavioral and structural namespaces provide a fine-grained, propositional approach to dividing up work within a single regional namespace, allowing blocks with updates to code running in disjoint behavioral or structural namespaces to be merged more easily.

#### Validators and Watchers
A **validator** is a piece of software responsible for checking the behavior of other validators.  To run a validator, a party must buy a stake in Rev in that locale and be added to the canonical list of validators for each region it belongs to in that locale.  A validator a) proposes new blocks, b) checks that other validators are following the [Casper protocol](https://link+here),  and c) only proposing updates to the Rholang state that obey the [operational semantics of the language](https://link+here).  A namespace that is the join of a set of regions is validated by the union of the sets of validators for each region.  

Validators mutually verify each other in each regional namespace. If they detect failure to perform at the required rate, they publish that information.  If enough others see the same failure, the failing validator may be ejected; ejection does not cause the party running the validator to lose its stake, but the validator is excluded form earning any more transaction fees.   If, on the other hand, others do not see the performance failure, the validator that "cries wolf" too often may be ejected.

**Watchers** are pieces of software that only check the validity of the Rholang updates, but form coalitions that watch all the regional namespaces in a locale.  To run a watcher, a party must buy stake in Rev in that locale and be added to the canonical list of watchers for that locale.

If watchers or validators see deviation from the protocol, they publish that information; it will have cryptographic evidence of deviation.  If enough others accept the evidence, the network slashes the validator and the party running the validator loses its stake.

### Organizational dag

The purpose of the organizational dag is to allow parts of the RChain network to act independently of each other.  This allows for parallelism at the cost of certain security invariants.  Unlike code running in different namespaces within a locale, where every validator trusts a coalition of watchers to check everything that goes on within all the other namespaces to make sure no one's cheating, the locales in the organizational dag do not necessarily watch one another.  They behave much like servers on the internet do today, where clients may provide malicious inputs.  It is the responsibility of code that accepts inputs from outside of the locale to perform input validation.

The organizational dag grows as organizations mount new locales as children of existing locales of an organizational tree.  The child locale publishes its genesis block in the parent, the validators bond in the parent locale, and optionally bond to validate the locales above the parent in the organizational tree.  It is in the interest of the validators in a locale L to validate the locales above it in an organizational tree so that they can be sure that the ancestor locales do not cheat by transferring or burning the Rev allocated to any subtree containing L.

By becoming validators in the parent locale, validators can propose blocks that deploy new code for interacting with contracts in the parent locale.  In the RChain organizational tree, there will be default contracts for publishing a genesis block and bonding, for Rev tracking in subtrees, and for routing data to other locales.

## Locales

### Regions
#### Bonding Validators
##### Bond amount
##### Fees
##### Ejection criteria
##### Payouts while bonded
##### IP addresses
The validators should run a contract that maps each validator to an IP address and allows the validator to send a signed message to update their IP address.  Nodes can then query the contract to get the information.
#### Unbonding Validators
##### Waiting period
#### Bonding Watchers
##### Bond amount
##### Fees
##### Ejection criteria
##### Slashing criteria
##### Payouts while bonded
#### Creating new regions

### Finalization
#### Block safety feed
#### Code mobility

### Rev
#### Mint
#### Coinpurse
#### Public facade
A public facade is a forwarding object that checks signature validity before forwarding.  It is how client-side wallet software interacts with purses in a locale.

### Registry
Each locale maintains a registry of URNs to unforgeable names.
#### Registration API
##### Proof of ownership
###### IANA domains
###### Public keys
###### Hash preimages
#### Lookup API

## Organizational dag
### Root locale
### Mounting API
### Unmounting API
### Token tracker API
### Routing API

