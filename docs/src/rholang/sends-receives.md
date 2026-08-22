# Sends and receives

The entire rholang language of computation reduces to two operations: **send** a message on a name,
and **receive** a message on a name. Everything else — state, iteration, contracts — is built from
these.

## Send

A send places a value on a channel and immediately continues:

```rho
name!(value)
```

`name` is any name (a quoted name `@"foo"`, a `new` name, a literal). `value` is any name — recall
that in rholang *everything you can name is a name*, including integers, booleans, strings, and quoted
processes. To send several values at once, list them:

```rho
name!(a, b, c)
```

## Receive

A receive waits for a message on a channel, binds it to a pattern, and runs the body:

```rho
for (pattern <- name) { body }
```

The `pattern` is a pattern (see [Patterns and matching](patterns-matching.md)); the simplest pattern is
a variable, which binds the whole message. A receive with several `<-` clauses waits for a message on
*each* channel simultaneously — a **join** (see [Joins and concurrency](joins-concurrency.md)):

```rho
for (x <- a; y <- b) { body }
```

## The COMM event

When a send and a receive meet on the same name, they **comm** (communicate): the message is delivered
to the receiver, and both are consumed. This single event — called **COMM** — is the whole engine of
computation (Law 4, [Structural congruence and reduction](../formal/congruence-reduction.md)).

```rho
new chan in {
  chan!(42) |
  for (x <- chan) { /* x = 42, runs once */ }
}
```

Because communication is asynchronous and the tuple space holds unmatched messages, the order in
which you *write* the send and the receive does not matter — whichever arrives first simply waits for
the other.

## Persistent channels

An ordinary send or receive is **linear**: it is consumed by exactly one comm. Rholang also has
**persistent** (replicated) forms:

- `name!!(value)` — a **persistent send**: the message is *not* consumed when it comms, so it can
  satisfy arbitrarily many receivers.
- `for (pattern <= name) { body }` — a **persistent receive** (a *peek*): the receiver is *not*
  consumed, so it can comm with arbitrarily many sends.

A persistent receive is exactly a service that is always available. The common way to write one is the
`contract` sugar:

```rho
contract @"double"(@x) = {
  @"double"!(2 * x)
}
```

`contract name(pattern) = { body }` is desugared to `for (pattern <= name) { body }`. Contracts are
how rholang expresses a callable service: an arbitrary number of clients can send to `@"double"`, and
each send comms with the one persistent receive.

## Sequencing sends with acks

Sends do not block, so two sends to the same channel may arrive in either order. To force a sequence —
for example, to write two lines to the console in order — rholang uses the standard **ack** pattern:
the system provides `rho:io:stdout` (print) together with `rho:io:stdoutAck` (a confirmation that the
print finished). Chain them:

```rho
new ack in {
  stdout!("first") |
  for (_ <- stdoutAck) {
    stdout!("second") |
    for (_ <- stdoutAck) { Nil }
  }
}
```

Each `stdout!(…)` is followed by a receive on `stdoutAck`; the second print only runs once the first
has acknowledged. This is the rholang idiom for ordered side effects in an otherwise asynchronous
language.

> **Formal.** Send/receive are the `Send`/`Receive` fields of the flat `Par`; `!!`/`<=` are the
> `persistent`/`peek` flags. COMM is the `Reduce` relation in `spec/Rchain/Rho.lean`, and the
> tuplespace pairing is `sending-receiving.k` + `persistent-sending-receiving.k`. See
> [The 19 laws](../formal/the-19-laws.md) (Law 4).
