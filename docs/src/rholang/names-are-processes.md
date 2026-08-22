# Names are quoted processes

The single idea that separates the ρ-calculus from the π-calculus is this: **a name is a quoted
process**. In rholang, the two sorts of the language are defined mutually:

```
Name  ::=  @Proc          -- quote a process into a name
Proc  ::=  *Name          -- evaluate a name back into a process
           |  …           -- sends, receives, new, match, par, replication
```

`@` (quote) turns a process into a name; `*` (evaluate) turns a name back into a process. Reflection —
a program naming and dereferencing its own code — is a *primitive*, not a library feature.

## `@` — quoting

To name something, quote it. The name `@"hello"` is the process `"hello"` (a ground string) quoted.
The name `@0` is the stopped process `0` quoted. Because *any* process can be quoted, you can name any
program:

```rho
@{ stdout!("hi") }           -- a name that denotes a printing process
@{"hello" | "world"}         -- a name that denotes a parallel composition
```

A name is therefore **data you can pass around**: the quoted process is carried as a value, and no
process inside it runs while it is quoted.

## `*` — evaluating

`*` is the inverse: given a name that is a quoted process, evaluate it to get the process back.

```rho
*@"hello"                    -- the process "hello" (a ground string), unquoted
```

The two are inverses: `*@P` is `P`, and `@*C` is `C` (up to equivalence, below). This is what lets a
process *run* code that it was handed as data.

## Name equivalence

Two names are **equivalent** when the processes they quote are equivalent. Concretely, a name is a
process *modulo* a short list of rules (the K rule `name-equivalence.k`, Law 2):

1. **Parallel order** — `@{P | Q} = @{Q | P}`.
2. **Identity** — `@{P | Nil} = @P`.
3. **Associativity** — `@{(P|Q)|R} = @{P|(Q|R)}`.
4. **Top-level arithmetic** — `@(10 + 2) = @(5 + 7)` (arithmetic is evaluated at the top level of a
   name).
5. **α-equivalence** — bound names may be consistently renamed.
6. **Added quotes/evals** — `@*@P = @P` (a name that quotes the evaluation of its own quote is the
   same name).

The consequence is that a name is **content-addressed**: the name of a process is determined by the
process's *meaning*, not by the text you typed. `@"hello"` and `@*@"hello"` and `@"hello" | Nil` are
all the same name.

## You can only look through the looking glass once

Name equivalence is applied **only at the top level**. When a *pattern* contains a quoted name, the
quoted name inside must match **exactly** (up to α), not up to full name equivalence. This is the
"looking glass once" rule: you may look through one layer of `@` to compare, but a name nested deeper
is matched literally.

This matters for patterns: `for (@{P | Q} <- chan) { … }` matches a message whose quoted process is
exactly a parallel composition, but `for (@{Q | P} <- chan) { … }` — where the two sub-processes are
swapped — also matches, because `P|Q` and `Q|P` are the same name. Patterns make this precise in
[Patterns and matching](patterns-matching.md).

## Why this matters

Because a name *is* a quoted process:

- **The registry is just a map from names to processes.** Looking up a name is looking up code.
- **Upgradeable contracts** are possible: you quote a new contract into a name, and the name now
  denotes the new code.
- **Unforgeable names** (next chapter) get their power from the same reflection: a `new` name is a
  quoted process you alone hold, and `*` is how you *use* it while keeping it secret.

> **Formal.** The reflective grammar is `spec/RHO-CALCULUS.md` §1; the flat `Par` *erases* `@`/`*` and
> the sort is recovered structurally (`classify`/`isPureName`). Name equivalence is Law 2
> (`StrCong` in `spec/Rchain/Rho.lean`; `name-equivalence.k`). See
> [Grammar and sorts](../formal/grammar-sorts.md).
