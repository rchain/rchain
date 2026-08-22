# Patterns and matching

Receives and `match` expressions don't just bind a whole message — they **match** a message against a
**pattern**, and run only if the message fits. Pattern matching is rholang's way of querying structured
data and of dispatching on a message's shape.

## Patterns are processes, plus a little more

A **pattern** is a process with a few extra productions added on top: variables, the wildcard `_`,
negation `~`, and the logical connectives `/\` (and) and `\/` (or). So every process is already a
pattern (the pattern that matches exactly that process), and patterns add the ability to say "anything
of this shape".

The simplest patterns are **variables**, which bind:

```rho
for (x <- chan) { … }        -- x binds to the whole message
for (@x <- chan) { … }       -- bind the process underlying a name; x is the process
```

A variable in *name* position (`x`) binds a name; a variable in *process* position (`@x`) binds the
process the name denotes.

## Matching is structural

Matching walks the structure of the message and the pattern together. A ground pattern matches only an
equal ground; a compound pattern matches a compound message whose parts match:

```rho
for (("hello", x) <- chan) { … }      -- a tuple whose first element is "hello"
for ([head, ...tail] <- chan) { … }   -- a non-empty list; head/tail split
for ({key : value} <- chan) { … }     -- a map containing key : value
```

## The wildcard and negation

- `_` — the **wildcard** — matches anything and binds nothing.
- `~P` — **negation** — matches anything that `P` does *not* match.

```rho
for (_ <- chan) { … }                 -- match any message, ignore it
for (~Nil <- chan) { … }              -- match any non-empty process
```

Negation is what makes rholang patterns able to express "everything except …".

## `/\` and `\/`

Two logical connectives compose patterns:

- `P /\ Q` — **and** — the message must match both `P` *and* `Q`; variables bind in both.
- `P \/ Q` — **or** — the message must match `P` *or* `Q`; **variables do not bind** in an `or`.

```rho
for (@{x} /\ @"tag"!(y) <- chan) { … }   -- a process that is x and also sends on "tag"
```

## Matching a parallel composition

A pattern may itself contain `|`:

```rho
for (@{P | Q} <- chan) { … }
```

This matches a message whose process is a parallel composition with a `P` part and a `Q` part. The
match is **greedy**: one pattern binds one variable to *all* of `P1 | … | Pn` (the whole parallel
bundle), rather than splitting it. This is the subtle "matching with `|`" rule (the K rule
`matching-with-par.k`).

## Exact matching with `=`

By default a pattern variable *binds* (and may shadow). The `=` prefix requests an **exact** match —
match the name literally, not as a binder:

```rho
for (=x <- chan) { … }        -- match exactly the message x (no binding/shadowing)
for (name =* x <- chan) { … } -- match the exact name x, bind it to `name`
```

This is the difference between "a variable" and "this specific name".

## Name equivalence applies once

Recall from [Names are quoted processes](names-are-processes.md): name equivalence (`@{P|Q} = @{Q|P}`,
and so on) is applied **only at the top level** of a name. When a pattern contains a quoted name
*nested inside*, that inner name must match exactly (up to α), not up to equivalence. You can "look
through the looking glass" once — at the outer name — and no further.

## `match` — dispatching on a value

The same patterns power the `match` expression:

```rho
match value {
  Nil => { … }
  [head, ...tail] => { … }
  _ => { … }
}
```

`match` tries each case **in order** and runs the body of the first pattern that matches (with the
matched variables in scope); an unmatched value falls through (and an implicit trailing `_ => Nil`
matches everything). This first-match-wins ordering is Law 4's determinism, and the guarantee that a
pattern binds each free variable **at most once** is Law 5.

> **Formal.** Matching is **spatial matching** (Law 5): `BindsAtMostOnce`, `spatialMatches`, and the
> decidable match in `spec/Rchain/Match.lean`; the K rules are `matching-function.k`,
> `specific-matching-rules.k`, `exact-matching-function.k`, and `matching-with-par.k`. See
> [Substitution and matching](../formal/substitution-matching.md).
