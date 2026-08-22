# Control flow and state

Rholang has no mutable variables and no loop statements. Control flow is **matching** and
**recursion**; state is a **channel holding a value**, updated by consuming and re-producing it. Both
are natural consequences of the process model.

## Conditionals

```rho
if (x > 0) { … } else { … }
```

`if` is sugar for a `match` on a boolean:

```rho
match x > 0 {
  true => { … }
  false => { … }
}
```

## `match` — dispatching on shape

```rho
match value {
  Nil => { … }
  [head, ...tail] => { … }
  ("tag", x) => { … }
  _ => { … }
}
```

Cases are tried in order; the first matching pattern runs, with its variables bound (see
[Patterns and matching](patterns-matching.md)).

## Recursion

Iteration is recursion. A contract can call itself:

```rho
contract @"countdown"(n) = {
  if (n == 0) { Nil }
  else { @"countdown"!(n - 1) }
}
```

A receive that is persistent (`contract`) re-arms itself after each comm, so it acts as a loop.

## State: a channel that holds one value

Mutable state is a `new` channel holding exactly one value, with **get** and **set** contracts that
consume and re-produce it. This is the canonical rholang "cell":

```rho
new cell in {
  cell!(0) |

  contract get(ack) = {
    for (value <- cell) {
      cell!(value) |            -- put it back
      ack!(value)               -- and report it
    }
  } |

  contract set(newValue, ack) = {
    for (_ <- cell) {
      cell!(newValue) |         -- replace it
      ack!(true)
    }
  }
}
```

To **read**, `get` consumes the current value, immediately re-produces it (so it isn't lost), and
sends it to the caller. To **write**, `set` consumes and discards the old value and produces the new
one. Because `cell` is a private unforgeable name, no one else can interfere with the state — this is
the rholang equivalent of a lock-protected variable, expressed entirely as processes.

This pattern — a `new` name + get/set contracts — is how rholang builds everything from counters to
vault balances to the name registry.

## Iteration over a collection

Iteration is a recursive contract that walks a list:

```rho
contract @"sum"(list, ack) = {
  match list {
    [] => { ack!(0) }
    [head, ...tail] => {
      new rest in {
        @"sum"!(tail, *rest) |
        for (r <- rest) { ack!(head + r) }
      }
    }
  }
}
```

Each step splits head/tail, recurses on the tail, and combines the result — the process-calculus form
of a `fold`.

> **Formal.** `match` and the first-match-wins ordering are Law 4's determinism; the "value in a
> channel" cell is a degenerate tuplespace (a single-slot produce/consume). See
> [Structural congruence and reduction](../formal/congruence-reduction.md) and
> [The tuple space (RSpace)](../node/rspace.md).
