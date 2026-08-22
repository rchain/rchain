# Language reference

A compact reference for the rholang surface syntax. The precise abstract grammar and sorts are in
[Grammar and sorts](../formal/grammar-sorts.md); this page is the programmer-facing cheat sheet.

## Core grammar

```
Proc ::=  Name ! ( Name,* )                -- send
       |  Name !! ( Name,* )               -- persistent send
       |  for ( Pattern <- Name ;* ) { Proc }    -- receive (; = join)
       |  for ( Pattern <= Name ;* ) { Proc }    -- persistent receive (peek)
       |  contract Name ( Pattern,* ) = { Proc } -- sugar for a persistent receive
       |  new NameDec,* in { Proc }              -- fresh unforgeable names
       |  match Name { Pattern => Proc ,* }      -- first-match-wins dispatch
       |  if ( Bool ) Proc else Proc             -- sugar for match true/false
       |  Proc | Proc                            -- parallel composition
       |  ! Proc                                 -- replication
       |  Nil

Name ::=  @ Proc                            -- quote a process
       |  * Name                            -- evaluate a name
       |  Ground                            -- Int | BigInt | String | Bool | Uri | ByteArray
       |  Unforgeable                       -- new name | deployerId | deployId | sysAuthToken
       |  Bundle                            -- bundle{+|-|0|} Proc
       |  [ Name,* ] | ( Name,* ) | Set( Name,* ) | { Name : Name ,* }   -- collections
```

`;` in a receive is a **join** (simultaneous receive on every listed channel). `<=` is a peek; `!!` is a
persistent send.

## Patterns

```
Pattern ::=  Name                          -- an exact (literal) process
          |  Var                           -- binds
          |  _                             -- wildcard (matches anything, binds nothing)
          |  ~ Pattern                     -- negation (matches anything the pattern does not)
          |  Pattern /\ Pattern            -- and (binds in both)
          |  Pattern \/ Pattern            -- or  (binds in neither)
          |  = Name                        -- exact match (no binding/shadowing)
          |  Var =* Name                   -- match an exact name and bind it
          |  Pattern | Pattern             -- match a parallel composition (greedy)
```

Name equivalence is applied only at the top level of a name ("look through the looking glass once").

## Precedence

From tightest to loosest: postfix method calls and `*`/`@`; arithmetic (`* / %` then `+ -`);
comparisons; `not`; `and`; `or`; then `|` (parallel composition); then `new`/`for`/`match`/`if` bodies.
Parenthesize when in doubt — `( … )` always wins.

## Grounds and operators

| Kind | Literals | Operators / methods |
|---|---|---|
| Integer | `42`, `-7` | `+ - * / %`, `< <= > >= == !=` |
| Big integer | arbitrary precision | same |
| Boolean | `true` `false` | `and`, `or`, `not` |
| String | `"…"` | `++`, `${…}` interpolation, `length`, `slice`, `contains`, `toUtf8Bytes`, `hexToBytes` |
| URI | `` `rho:…` `` | — |
| Byte array | `"…".hexToBytes()` | `length`, `nth`, `slice`, `toUtf8Bytes` |

## Collections

| Type | Syntax | Methods |
|---|---|---|
| List | `[a, b, c]` | `nth`, `length`, `slice`, `getOrElse`, `contains`, `++` |
| Tuple | `(a, b, c)` | `nth`, `length` |
| Set | `Set(a, b)` | `contains`, `union`, `diff`, `delete`, `size` |
| Map | `{k: v, …}` | `get`, `getOrElse`, `set`, `keys`, `values`, `contains`, `union`, `diff`, `delete`, `size` |

## System channels

| Channel | Purpose |
|---|---|
| `rho:io:stdout` / `rho:io:stdoutAck` | print a line / its completion ack |
| `rho:io:stderr` / `rho:io:stderrAck` | print to stderr / its ack |
| `rho:registry:lookup` | resolve a name to a value |
| `rho:registry:insertArbitrary` | insert a value (unauthenticated) |
| `rho:registry:insertSigned:secp256k1` | insert a value, signed |
| `rho:rchain:deployerId` | the unforgeable id of the deployer (who invoked the contract) |
| `rho:rchain:revVault` | the REV vault contract |
| `rho:rchain:pos` | the proof-of-stake contract |
| `rho:crypto:blake2b256Hash` / `keccak256Hash` | hashing |
| `rho:crypto:secp256k1Verify` / `ed25519Verify` | signature verification |

## Known limitations

Some surface features are known incomplete in the reference semantics (see
[`legacy/rholang/README.md`](../../../legacy/rholang/README.md) "what's broken"): guarded patterns, and
certain 0-arity/match-case pre-evaluation edge cases. Treat the K semantics under
[`legacy/rholang/src/main/k/rholang/`](../../../legacy/rholang/src/main/k/rholang/) as the executable
reference, and the [19 laws](../formal/the-19-laws.md) as the authoritative invariants.
