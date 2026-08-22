# Processes and names

Rholang is a **process-oriented** language. Everything in a rholang program is a *process*, and the
program itself is many processes running **concurrently**, composed with the parallel operator `|`
(pronounced "par").

## Two kinds of thing: processes and names

The language has exactly two syntactic sorts:

- A **process** (*Proc*) is something that *runs*. It can send a message, wait to receive one, create a
  fresh name, branch on a pattern, or run several sub-processes in parallel.
- A **name** (*Name*) is something you *communicate over* — a channel. It appears in the channel
  position of a send, and in the source position of a receive.

The distinction is formal (see [Grammar and sorts](../formal/grammar-sorts.md)), but you already know
the intuition: a process is a *verb*, a name is a *noun*. In code they look different too — a name is
written with an `@` or as a literal, a process is everything else.

## Parallel composition

The simplest process is `Nil` — the stopped process that does nothing. The fundamental combinator is
`|`:

```rho
P | Q
```

This means "run `P` and `Q` at the same time." Order does not matter: `P | Q` and `Q | P` are the
*same* process (that is Law 2 — structural congruence). A rholang program is just a big parallel
composition of smaller processes, each of which is also a parallel composition, all the way down.

## Communication

Processes do not share memory. The *only* way two processes interact is by exchanging a message over a
name. There are two halves:

- **Send** — `name!(data)` puts `data` onto the channel `name` and continues. It does **not** wait for
  a receiver; communication is **asynchronous**.
- **Receive** — `for (pattern <- name) { body }` waits for a message on `name`, binds the message to
  `pattern`, and runs `body`.

Here is the smallest complete program — a "hello world" that sends the integer `1` over a name and
receives it:

```rho
new hello in {
  hello!(1) |
  for (x <- hello) { /* x is now 1 */ }
}
```

The `new hello in { … }` declares a fresh, private name `hello` (we will return to what "fresh" means
in [Unforgeable names](unforgeable-names.md)). Inside the braces, the two processes run concurrently:
the sender puts `1` on `hello`, and the receiver takes it off, binding `x` to `1`.

## The tuple space

Behind the scenes, communication happens through a **tuple space** (the RSpace layer, see
[The tuple space (RSpace)](../node/rspace.md)). Think of it as a shared blackboard:

- A **send** writes a *message* (its channel plus its data) onto the blackboard and leaves it there.
- A **receive** looks for a matching message; if one is present, the two are paired in a **COMM event**
  and both are consumed (or not, for persistent channels — see [Sends and receives](sends-receives.md)).
- If a send has no matching receive yet, the message simply *waits* on the blackboard. If a receive has
  no matching send, the receive waits.

This is a *join calculus* style of concurrency: the blackboard holds both outstanding messages and
outstanding receivers, and progress happens when a matching pair meets.

## Why processes instead of functions

A conventional program is a function that maps an input to an output. A rholang program is a set of
processes that react to messages forever. That is the right model for a **blockchain**: the node is
not "run" and "finished"; it is a persistent process that accepts deploys, produces blocks, and keeps
state — a network of senders and receivers that never stops.

> **Formal.** The process/name split is the `PSort` judgment (`HasSort`/`HasVarSort`); parallel
> composition and `Nil` are the `parMerge`/`nilPar` of the flat `Par`. See
> [Grammar and sorts](../formal/grammar-sorts.md) and Law 2 in [The 19 laws](../formal/the-19-laws.md).
