# Smart contracts

The node ships a set of **blessed contracts** — the genesis contracts that bootstrap the chain's
state — and they are the best demonstration of everything in this part of the book. Each one is built
out of unforgeable names, quoting, matching, and the capability patterns of the previous chapter. The
source is under [`legacy/casper/src/main/resources/`](../../../legacy/casper/src/main/resources/).

## The name registry

The **registry** is the chain's phone book: it maps a *name* (a quoted process) to a value. It is what
makes "look up code by name" work, and it is the substrate for upgradeable contracts.

```rho
@"rho:registry:lookup"!(*name, *ack)          -- look up `name`
@"rho:registry:insertArbitrary"!(*name, *v)   -- insert a value (unauthenticated)
@"rho:registry:insertSigned:secp256k1"!(...)  -- insert signed by a key (authenticated)
```

The registry is itself a rholang contract (a `TreeHashMap` behind `rho:registry:*`), and its keys are
names — which, because names are quoted processes, means *code is addressable*. The **self-update**
pattern (`UpdateRegistry`, `UpdatePos`, `UpdateAuthKey`) works by inserting a *new* contract into the
registry under the same name, so future lookups resolve to the new code. Upgradeable contracts are not
a special feature; they are a consequence of names being quoted processes.

## RevVault — accounts and transfer

`RevVault.rho` is the REV token vault. Its core operations are `findOrCreate` (get an account's vault,
creating it on first use), `balance` (how much it holds), and `transfer` (move REV between vaults):

```rho
@"rho:rchain:revVault"!("findOrCreate", *address, *ack)
@"rho:rchain:revVault"!("balance", *vault, *ack)
@"rho:rchain:revVault"!("transfer", *fromVault, *toVault, *amount, *ack)
```

A vault is identified by an **unforgeable name** derived from the account's public key (a `RevAddress`).
Only the holder of the vault's name can transfer out of it — access control is possession of an
unforgeable name, nothing more. `MultiSigRevVault.rho` extends this to a vault that requires
**k-of-n signatures** to move funds (the multisig pattern).

## MakeMint — unforgeable money with sealers/unsealers

`MakeMint.rho` implements money using the **sealer/unsealer** pattern from
[Object capabilities](object-capabilities.md). A mint is a pair `(purseFactory, brand)`:

- `purseFactory` creates a **purse** that can hold a balance and `deposit`/`withdraw`.
- `brand` is the unsealer that **authenticates** a purse: it proves a purse really was issued by this
  mint.

Depositing into a purse is *sprouting* — the mint seals the deposited value, and the purse holds the
sealed amount. Because only the mint holds the sealer, **no one can forge money**: a purse balance can
only come from a real deposit, verifiable by the brand. This is the rholang way to make a fungible
token whose authenticity is enforced by unforgeable names rather than by trusting a global ledger of
balances.

## NonNegativeNumber — monotone state via a mergeable tag

`NonNegativeNumber.rho` wraps a number so that it can only ever be **non-negative**, and so that
concurrent updates **merge deterministically**. It is the canonical example of the "mergeable tag"
pattern that makes rholang state behave under concurrency: instead of a single mutable cell (which
would have races), the number is a *monotone* value whose updates compose (`add`, `sub` with a floor of
zero, `peek`). This is the pattern behind mergeable channel state and deterministic contract merges
(Law 17).

## AuthKey — capability tokens

`AuthKey.rho` derives an **authorization key** from an unforgeable name or process. The auth key is
itself an unforgeable name that can be used as a capability token: hold it and you are authorized. The
`UpdateAuthKey` contract rotates this token — the capability pattern of revocation applied to a
contract's own authority.

## Either and ListOps — small combinators

`Either.rho` is an error-handling monad (`left`/`right`, `fromNillable`) for contracts that need to
return "value or error" cleanly. `ListOps.rho` provides list utilities (prepend/append/…) used by the
other blessed contracts. They are the ordinary functional-combinator layer on top of the process
model.

## The shape of a rholang contract

Every one of these contracts has the same shape, which is the lesson of Part I:

1. `new` names define the object's private authority.
2. `contract` channels are the object's public interface.
3. patterns match and dispatch on messages.
4. unforgeable names and bundles enforce who may do what.
5. `|` composes the parts, and the whole is correct because the parts are.

There is no state that is not a channel, no authority that is not a name, and no check that is not a
match. That uniformity — everything is a process over names — is what makes rholang contracts
composable and auditable.

> **Formal.** Merge determinism and non-negative numeric channels are **Law 17**; content addressing
> and the bonds cache are **Law 16**; the fringe/finality these contracts live under is **Law 14**.
> See [The 19 laws](../formal/the-19-laws.md) and [Consensus (Casper)](../node/consensus.md).
