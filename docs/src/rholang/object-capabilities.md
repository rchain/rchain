# Object capabilities

Because possession of an unforgeable name *is* authority, rholang contracts are naturally
**object-capability** systems: you can only act on an object if you hold a name that denotes it. This
chapter shows the standard capability patterns — the same patterns Marc Stiegler's *A PictureBook of
Secure Cooperation* catalogued, expressed here as processes over unforgeable names.

## The capability model

- An **object** is a set of channels held inside a `new` scope.
- A **capability** is an unforgeable name that lets you reach one of those channels.
- **Granting** a capability is sending the name (via `*x`, [Unforgeable names](unforgeable-names.md)).
- **Revoking** is making the name unreachable (turning off the forwarder that held it).

There is no global authority to check against; there are only names. That is the whole model.

## Facets: read vs write

A single object often needs *different* capabilities for different users — one who may read a balance,
another who may move funds. Rholang expresses this with **bundles**, which wrap a process and restrict
its capability:

- `bundle+{P}` — **write-only**: you may send *to* the channels in `P`, but not receive from them.
- `bundle-{P}` — **read-only**: you may receive, but not send.
- `bundle0{P}` — **neither** read nor write (an inert capability).
- `bundle{P}` — both.

A `bundle-` name handed to a client gives them the ability to *read* a value but not to *change* it —
the capability equivalent of a read-only reference, enforced by the language, not by a check.

## Attenuating forwarders

An **attenuating forwarder** is a proxy that sits in front of an object and forwards only the messages
it is told to allow. The client holds the forwarder's name, not the object's name, so the client can do
only what the forwarder permits:

```rho
new full in {                       -- the full capability (held by the owner)
  new limited in {                  -- the attenuated capability (given to the client)
    contract limited(action) = {
      match action {
        ("allowed", data) => { full!("allowed", data) }
        _ => { Nil }               -- everything else is dropped
      }
    } |
    /* give *limited to the client; keep full private */
  }
}
```

The client can reach `limited`, but `limited` only forwards the `"allowed"` case. The client never
holds `full`.

## Revocation

Revocation is an attenuating forwarder that can be switched off. The forwarder holds a `live` flag; to
revoke, you flip it:

```rho
new full, revoked in {
  contract limited(action) = {
    for (_ <- revoked) { Nil }     -- once revoked, the forwarder stops
  } |
  /* revoke by sending: revoked!(true) */
}
```

After `revoked!(true)`, the forwarder can no longer be reached in a way that forwards — the capability
is dead.

## Sealer / unsealer

A **sealer/unsealer pair** is a pair of names such that one *seals* a value into an opaque, tamper-proof
box and only the other *unseals* it. It is rholang's way to make a value that can be authenticated
without being forged or inspected:

```rho
new sealer, unsealer in {
  contract sealer(value, ret) = {
    new sealed in {
      sealed!(*value) | ret!(*sealed)   -- box the value in a fresh name
    }
  } |
  contract unsealer(box, ret) = {
    for (value <- box) { ret!(value) }  -- only the unsealer opens the box
  }
}
```

Whoever holds `sealer` can box a value; only whoever holds `unsealer` can open it. The classic use is
**brands**: a mint seals a value with its private sealer, and anyone can *verify* a sealed value came
from the mint by asking the mint's unsealer — without being able to mint forgeries. This is exactly how
the system's `MakeMint` contract implements unforgeable money (see
[Smart contracts](smart-contracts.md)).

## Composition and the caveat

Capabilities **compose**: a system built by `|`-composing capability-safe parts is capability-safe.
But the one rule to respect is: **be careful what you attenuate and forward**. If you give a client an
attenuator that forwards a *granting* capability (the ability to hand out more capability), the
attenuation can be escaped by the client granting itself more. The Stiegler patterns exist precisely to
make such escape impossible by construction — by never handing out a capability that grants capability.

> **Formal.** Bundles are the `Bundle` name in the grammar (`writeFlag`/`readFlag`, `bundle+/-/0`);
> they restrict capability structurally, so a bundle name cannot be deconstructed through matching.
> The sealer/unsealer is the `MakeMint`/brand pattern. See
> [Grammar and sorts](../formal/grammar-sorts.md) and the blessed contracts in
> [`legacy/casper/src/main/resources/`](../../../legacy/casper/src/main/resources/).
