# Unforgeable names

An **unforgeable name** is a name that cannot be guessed or reconstructed by anyone who does not
already hold it. It is rholang's unit of **authority**: possessing an unforgeable name is proof of the
right to use it, because no one else can forge it.

## `new` — fresh, private names

Unforgeable names are created by the `new` construct:

```rho
new a, b in { … }
```

`new a in { P }` binds `a` to a **fresh** unforgeable name whose scope is `P`. Each `a` is a *quoted
unforgeable process* — a name of the form `@Unforgeable` — generated from fresh randomness (a
Blake2-based derivation from the deploy's identity), so:

- **It is fresh.** No other `new a` anywhere else produces the same name.
- **It is unforgeable.** There is no language production that turns a string of bits back into the
  name. You cannot write it down; you can only *receive* it.

Because the name is private, two processes that share a `new` name have a private channel no one else
can send on or listen to. That is the foundation of secure, concurrent computation: private channels
mean no shared memory and no interference.

## Using the name

Inside its scope, `a` is used directly as a channel:

```rho
new a in {
  a!(42) |                      -- send on the private channel a
  for (x <- a) { … }            -- receive on the private channel a
}
```

The sender and receiver agree on `a` because they are both in its scope. No third party can forge `a`,
so no third party can interfere.

## Exporting the name: `*a`

The only way to give someone else access to an unforgeable name is to *send it* over a channel they
can already reach. That transfer uses `*` (evaluate):

```rho
new a in {
  new b in {
    a!(*b)                      -- send the *underlying process* of b out of b's scope
  } |
  for (x <- a) {
    -- here x is the name b: the receiver re-quotes the received process
    b!(1)                       -- x (== b) can now be used as a channel
  }
}
```

Here is the reflection dance, in detail. `b` is the name `@U` (a quoted unforgeable process). To export
it, you send `*b`, which evaluates to the underlying process `U`. The receiver binds that process to
the pattern variable `x`; because `x` is in *name* position, the received process is re-quoted, so `x`
is `@U` — the same name as `b`. The receiver has thus received `b` itself, without ever seeing bits
they could forge.

This is the rholang way to **grant a capability**: hand over the unforgeable name.

## Forgeable names vs unforgeable names

A *ground* name such as `"hello"` or `42` is **forgeable**: anyone can write it down, so possession of
it confers no authority — everyone already has it. Such names are fine for *public* data, but they
cannot be used to establish "you and only you may do X". Only unforgeable names can, because only the
holder *could* have them.

This is why rholang smart contracts build their security entirely out of `new` names: an account is a
`new` name, a mint's authority is a `new` name, a registry's write capability is a `new` name. See
[Object capabilities](object-capabilities.md) and [Smart contracts](smart-contracts.md).

## Unforgeable names in the system

The node pre-defines some unforgeable names to model authority on the chain itself. The most important
is the **deployer id** (`GDeployerId`): a name derived from the public key that signed a deploy, which
lets a contract identify *who* invoked it. Others include `GDeployId` (a unique id per deploy) and
`GSysAuthToken` (a system-internal authority token). These are all `Unforgeable` in the grammar
([Grammar and sorts](../formal/grammar-sorts.md)).

> **Formal.** `new` is restriction (ν); unforgeable names are `GUnforgeable` (Law 4's `new`-freshness,
> `reduce_freeVars_subset` in `spec/Rchain/Reduce.lean`; the K rule `new` in `processes-semantics.k`).
> The freshness that makes them unforgeable is the Blake2b derivation (Law 19, axiomatized). See
> [The 19 laws](../formal/the-19-laws.md).
