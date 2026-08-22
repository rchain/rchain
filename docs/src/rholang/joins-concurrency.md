# Joins and concurrency

Concurrency in rholang is not something you "add" with threads and locks — it is the default. A program
is many processes composed with `|`, and they are already concurrent. The interesting construct is the
**join**, which lets a process wait for several messages *at once*.

## Joins: waiting for several messages together

A receive with multiple `<-` clauses waits for a message on **every** listed channel before it fires:

```rho
for (x <- a; y <- b) { … }
```

The body runs only once *both* `a` and `b` have produced a message. This is a **join**: an atomic
synchronization point across channels. Joins are what let rholang express "wait until all parties have
contributed".

```rho
new a, b in {
  a!(1) |
  b!(2) |
  for (x <- a; y <- b) {   -- fires once both sends are present
    /* x = 1, y = 2 */
  }
}
```

Because messages and receivers both wait in the tuple space, the join fires regardless of the order in
which `a!(1)` and `b!(2)` arrive.

## Deadlock

Because processes communicate only by messages, the only failure mode of concurrency is **deadlock**:
processes each waiting for a message the others will never send.

```rho
new a, b in {
  for (x <- a) { b!(x) } |      -- waits for a, then sends b
  for (y <- b) { a!(y) }        -- waits for b, then sends a
}
```

Neither side can move: each is waiting on the other. There is no shared memory to corrupt — deadlock is
the *only* concurrency hazard, and it is visible in the structure of the channels.

## The dining philosophers

The classic illustration is the dining philosophers: five philosophers sit around a table, each with a
fork to their left. To eat, a philosopher needs **both** adjacent forks. Each fork is a name; a
philosopher is a process that acquires two forks as a **join**:

```rho
new p1, p2, p3, p4, p5, f1, f2, f3, f4, f5 in {
  f1!(true) | f2!(true) | f3!(true) | f4!(true) | f5!(true) |

  contract philosopher(p, left, right) = {
    for (_ <- left; _ <- right) {
      /* eat, then put both forks back */
      left!(true) | right!(true) | philosopher!(p, left, right)
    }
  } |

  philosopher!(p1, f1, f2) |
  philosopher!(p2, f2, f3) |
  philosopher!(p3, f3, f4) |
  philosopher!(p4, f4, f5) |
  philosopher!(p5, f5, f1)
}
```

Each philosopher is a persistent process that atomically takes two forks via a join, then returns them.
The join is what makes fork acquisition *atomic* — a philosopher never holds one fork while waiting for
the other in a way that leaves the system half-acquired. (Whether this specific ordering deadlocks
depends on the fork assignment; the point is the pattern, not a particular fix.)

## Concurrency without shared state

The rholang answer to shared-memory races is: **don't share memory, share channels**. Each `new` name
is a private channel; a process only touches data it was explicitly handed. That is why
[Unforgeable names](unforgeable-names.md) and [Object capabilities](object-capabilities.md) are the
same story: private channels are both the concurrency mechanism and the security mechanism.

> **Formal.** Joins are multi-channel receives; join **commutativity** (the channel set is hashed in
> sorted order, so the join key is order-independent) is **Law 7** (`spec/Rchain/RSpace/Join.lean`).
> Deterministic pairing of sends/receives is **Law 8**. See [The 19 laws](../formal/the-19-laws.md)
> and [The tuple space (RSpace)](../node/rspace.md).
