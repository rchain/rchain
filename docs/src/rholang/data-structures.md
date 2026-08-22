# Data structures

Rholang's data values are all **grounds** — terms that live in name position and are compared by
value. The collection types are **Lists**, **Tuples**, **Sets**, and **Maps**, each with a small set of
methods. (A complete method table is in the [Language reference](reference.md).)

## Grounds

| Kind | Syntax | Example |
|---|---|---|
| Integer | `42`, `-7` | arithmetic `+ - * / %`, comparisons `< <= > >= == !=` |
| Big integer | arbitrary precision | same operators |
| Boolean | `true`, `false` | `and`, `or`, `not` |
| String | `"hello"` | `++` concatenation, `${…}` interpolation |
| URI | `` `rho:io:stdout` `` | backtick-quoted system name |
| Byte array | `"deadbeef".hexToBytes()` | hex/utf8 conversions, `length`, `nth`, `slice` |

A **ground name** is forgeable (anyone can write `42` or `"hello"`), so grounds carry data, not
authority — authority comes from `new` names ([Unforgeable names](unforgeable-names.md)).

## Lists

```rho
[1, 2, 3]
```

| Method | Result |
|---|---|
| `lst.nth(i)` | the `i`-th element |
| `lst.length()` | the length |
| `lst.slice(from, to)` | a sub-list |
| `lst.getOrElse(i, default)` | element `i` or a default |
| `lst ++ lst2` | concatenation |
| `lst.contains(x)` | does it contain `x` |

Lists are matched with head/tail patterns: `[head, ...tail]`.

## Tuples

```rho
(1, "two", true)
```

A fixed-arity grouping. Tuples are matched positionally: `(x, y)` binds `x` to the first element.

## Sets

```rho
Set(1, 2, 3)
```

Unordered, duplicate-free. `contains`, `union`, `diff`, `delete`, `size`. Sets are **commutative** —
`Set(1,2)` and `Set(2,1)` are the same value (part of Law 1).

## Maps

```rho
{"name": "Alice", "age": 42}
```

| Method | Result |
|---|---|
| `m.get(key)` | the value, or `Nil` |
| `m.getOrElse(key, default)` | the value or a default |
| `m.set(key, value)` | a new map with the entry |
| `m.keys()` / `m.values()` | the keys / values |
| `m.contains(key)` | does it have `key` |
| `m.union(m2)` / `m.diff(m2)` | set-like union/difference |
| `m.delete(key)` / `m.size()` | removal / count |

Maps are also commutative in their entries (Law 1): the same map regardless of key order.

## Strings and byte arrays

Strings are matched literally and concatenated with `++`:

```rho
"hello " ++ "world"          -- "hello world"
```

Interpolation embeds values: `` "the answer is ${x}" ``.

Byte arrays are the bridge to hashing and signatures (used throughout the crypto contracts):

```rho
"deadbeef".hexToBytes()      -- a ByteArray from hex
"hello".toUtf8Bytes()        -- a ByteArray from a string
"deadbeef".hexToBytes().length()
```

## Collections are data, not channels

A collection is a *value* — you send it, receive it, match it, and pass it around like any other name.
It is not itself a channel; to build state out of collections you combine them with `new` names and
contracts, which is the subject of [Control flow and state](control-flow.md).

> **Formal.** Commutativity of `ESet`/`EMap` and the canonical order are **Law 1** (`sort` idempotent,
> `sort(p|q) = sort(q|p)`). See [The 19 laws](../formal/the-19-laws.md) and
> [Grammar and sorts](../formal/grammar-sorts.md).
