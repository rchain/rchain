# Type-system specification for the RNodeRust port (ρ-calculus → Calculus of Constructions)

The **ρ-calculus core** (the grammar with its two sorts, the structural operations, and the
refinements) is defined in [`RHO-CALCULUS.md`](RHO-CALCULUS.md); this document is the *type
discipline* of the Rust port's own code over that core. It is guided by
machine-checked Lean 4 proofs (in [`Rchain/Rho.lean`](Rchain/Rho.lean) and
[`Rchain/Ty.lean`](Rchain/Ty.lean)) that embed the ρ-calculus as the base sort of a Calculus of
Constructions (Lean's CIC) and prove the *fundamentals* below. It is **not** a behavior change and
does not pre-empt Laws 1–19 in [`INVENTORY.md`](INVENTORY.md); it hardens the port against silent
partiality.

---

## Part I — The type system

### 1.1 The base sort: the ρ-calculus

The node executes the **reflective higher-order ρ-calculus** (Meredith & Radestock 2005). Its single
grammar makes a *name* a quoted process (`@Proc`) and lets a *process* dereference a name (`*Name`):

```
Name = @Proc     Proc = *Name | Proc|Proc | !Proc | for(…) { Proc } | new … in Proc | match …
```

The flat `Par` ADT ([`Rchain/Par.lean`](Rchain/Par.lean)) **erases** the quote/eval distinction: a
`Par` in name position *is* a name. `Par` is a flat record of 8 *repeated* fields
(`sends`/`receives`/`news`/`exprs`/`matches`/`unforgeables`/`bundles`/`connectives`), each kept sorted
by the canonical order of Law 1. The reflective core is therefore **recovered in the type layer**,
not by extra `Par` constructors.

### 1.2 The two syntactic sorts

`PSort` ([`Rchain/Ty.lean`](Rchain/Ty.lean)) is the one genuine rholang type distinction:

```lean
inductive PSort where | proc | name
```

- `name` — a term usable in *name* position (a pure name: `Nil`, ground/expressions, bundles,
  unforgeables, connectives);
- `proc` — a term usable in *process* position (anything with a top-level send/receive/new/match).

The structural classification is `isPureName : Par → Bool` (a `Par` with empty
`sends`/`receives`/`news`/`matches`), giving `classify : Par → PSort`.

### 1.3 The de Bruijn context and the typing judgment

Binders use de Bruijn **levels** (`Var.bound`/`Var.free`/`Var.wildcard`,
[`Rchain/Syntax.lean`](Rchain/Syntax.lean)). A context is `Ctx := List PSort` (the sort of each level
in scope), and `varSort Γ v : Option PSort` classifies a variable occurrence:

```lean
def varSort : Ctx → Var → Option PSort
  | Γ, .bound l => Γ.get? l
  | _, .free _  => none
  | _, .wildcard => none
```

The typing judgment has two halves:

- **variable level** — `HasVarSort Γ v s := varSort Γ v = some s`;
- **term level** — `HasSort t s := classify t = s` (the unique structural process/name sort).

`HasVarSort` and `HasSort` are both **functional** and **decidable** (Fundamental 1).

### 1.4 Well-formedness refinements

Three refinements make the interpreter's partiality impossible:

- **`Closed p`** (Law 6) — no free variables. Decidable via the `closed*` family of `Bool` functions,
  and preserved by composition, structural congruence, and canonicalization (Part II).
- **`WellScoped Γ t`** — every bound level of `t` is within `Γ` (the variable half of the judgment,
  enforced by `HasVarSort`).
- **`BindsAtMostOnce`** (Law 5, stated) — a pattern binds each free variable at most once; carried by
  the `freeCount` fields of `ReceiveBind`/`MatchCase`.

### 1.5 Substitution and reduction (minimal)

- **`Subst := Var → Par`**; `subst σ p` is the *minimal* simultaneous substitution: it rewrites free
  `evar (free _)` occurrences in the top-level `exprs` field to `(σ v).exprs` and leaves the process
  constructors (`sends`/`receives`/`news`/`matches`) untouched — visibly sort-preserving. Deep
  capture-avoiding de Bruijn substitution is Coq's Autosubst obligation (`AGENTS.md`).
- **`Reduce (⟶)`** ([`Rchain/Rho.lean`](Rchain/Rho.lean)) — the COMM redex contracts to the receive
  body, and reduction is a congruence under `|`. Replication (`!`/`!!`) and `new` freshness are
  stated as Coq obligations; the redex-contraction fact is all the closedness-preservation proof
  needs.

### 1.6 The totality invariant

The node's own code admits **no silent partiality**: every partial operation is either proven total on
a refinement, or returns `Option`/`Except` at a declared boundary. The Lean spelling is

```lean
def TotalOn (f : Par → Par) : Prop := ∀ p, Closed p → Closed (f p)
```

(see Fundamental 6).

### 1.7 Refinement types are the security system (no type escape)

The port's own Rust types carry the refinements of §1.6 **structurally**. A refinement type `R` over
`T` is the sigma type `{ t : T | P t }` — the invariant `P` is *part of the type* and travels with the
value through the whole domain.

- **Construction** — the only ways to obtain an `R` are a *validated* constructor (`TryFrom<T>`, at a
  boundary) or a *total* constructor on an already-valid input (`R::new`/`From`).
- **Discharge** — exactly one: `From<R> for T`, an explicit, named, one-way conversion, used *only* at
  a declared boundary (prost/wire encode, FFI, external API).
- **No type escape** — a refinement newtype must **not** implement `Deref` or expose a public
  `.get()`. Those silently drop `P` mid-domain and re-introduce the exact silent-cast bug the
  refinement exists to prevent; they are the Rust analogue of projecting `.1` from `{ t // P t }`
  without the proof.
- **Domain arithmetic** uses the newtype's own `Add`/`Sub`/`Ord`/`Eq` impls that preserve `P`; where a
  result may leave the invariant (e.g. a height *difference*), the operator's output is the raw
  **signed** type, so the signedness is again visible in the type.

Concrete newtypes live in [`../shared/src/refined.rs`](../shared/src/refined.rs) (`Port`, `Hash32`,
`BlockHeight`, `SeqNum`, `WireLen`, `NonNegI64`; `ByteLen`/`ShortLen` reserved for 1c); domain-specific
serialized refinements (`SerializedNode`, `SerializedRandom`) live with their deserializers. The Lean
spelling is `def Refined α P := { a : α // P a }` with `totalOn_lifts_to_refined`
([`Rchain/Ty.lean`](Rchain/Ty.lean)).

---

## Part II — The fundamentals (proven in Lean)

Each theorem is named and `sorry`-free; `#print axioms` on `Rchain.Rho`/`Rchain.Ty` reports only the
core axioms `propext`, `Classical.choice`, `Quot.sound` (no residual custom axioms — those live in
`Rchain.Sort`'s 30 element-comparator axioms, which `Ty.lean` does **not** import).

### F1. Sort classification is functional and decidable

A term has a unique sort, and the judgment is runnable as a checker.

| Lean name | Statement |
|---|---|
| `Rchain.HasSort_functional` | `HasSort t s → HasSort t s' → s = s'` |
| `Rchain.HasSort_decidable` | `Decidable (HasSort t s)` |
| `Rchain.hasVarSort_functional` | `HasVarSort Γ v s → HasVarSort Γ v s' → s = s'` |
| `Rchain.hasVarSort_decidable` | `Decidable (HasVarSort Γ v s)` |

### F2. Structural congruence is an equivalence and a congruence

`≡` (par order, `| Nil`, associativity, congruence) is reflexive/symmetric/transitive and respected by
every constructor.

| Lean name | Statement |
|---|---|
| `Rchain.strCong_equivalence` | `Equivalence StrCong` |
| `Rchain.strCong_comm` / `_assoc` / `_ident` / `_nil_left` | the `≡` laws |

### F3. Substitution preserves sort

Well-typedness is stable under (minimal) substitution — the type-side of Law 3.

| Lean name | Statement |
|---|---|
| `Rchain.subst_classify` | `classify (subst σ p) = classify p` |
| `Rchain.subst_preserves_sort` | `HasSort t s → HasSort (subst σ t) s` |

### F4. Reduction preserves sort and closedness

COMM never introduces free variables or an ill-sorted term — the type-side of Laws 4 & 6.

| Lean name | Statement |
|---|---|
| `Rchain.Reduce` (inductive) | minimal `⟶`: `comm` + `parLeft` + `parRight` |
| `Rchain.reduce_closed` | `Reduce p p' → Closed p → Closed p'` |
| `Rchain.Closed_receivePar_iff` | `Closed (receivePar c b) ↔ Closed c ∧ Closed b` |

*(The `proc`-sort half is stated: a closed `proc` reduces to a closed term that is still well-sorted;
a COMM body may be a `name`, which `HasSort` classifies accordingly.)*

### F5. Canonicalization commutes with typing

`sortList`/`≡`/`parMerge` preserve `Closed` and the sort classification — links Law 1 to the type
layer.

| Lean name | Statement |
|---|---|
| `Rchain.sortList_mem_pred` | `(∀ x ∈ l, P x) → (∀ x ∈ sortList C l, P x)` (instantiate `P := Closed`) |
| `Rchain.strCong_closed` | `StrCong p q → Closed p → Closed q` |
| `Rchain.Closed_parMerge_iff` | `Closed (parMerge p q) ↔ Closed p ∧ Closed q` |

### F6. Totality (effect) soundness

A `Total` operation composed from `Total` parts is `Total` — the formal "no `.unwrap()`" guarantee.

| Lean name | Statement |
|---|---|
| `Rchain.TotalOn` | `TotalOn f := ∀ p, Closed p → Closed (f p)` |
| `Rchain.TotalOn_id` | `TotalOn id` |
| `Rchain.TotalOn_comp` | `TotalOn f → TotalOn g → TotalOn (g ∘ f)` |

---

## Part III — The vertical map and the partiality catalogue

### 3.1 Vertical map (top → low)

| Layer | Crate(s) | Type-of-truth | Partiality boundary |
|---|---|---|---|
| Rholang AST | `models` | `Rchain.Par` (`Par`/`Expr`/…), `HasSort`, `Closed` | protobuf decode → `Result` |
| Interpreter | (rholang, later) | `subst`, `Reduce`, `TotalOn` | reduction is `TotalOn` on `Closed` |
| Storage | `block-storage`, `rspace` | content-addressed trie (Law 10) | `Map` lookup → `Option`/`Except` |
| Crypto | `crypto` | Blake2b/Curve25519/secp256k1 (**axiom**, Law 19) | fixed-width coercion → `TryFrom` |
| Wire | `comm`, `shared` | `Packet`/`Protocol` (protobuf) | decode/optional fields → `Except` |

### 3.2 Partiality catalogue (production `.unwrap()`/`.expect(`/`panic!` sites)

Each site below is a **production** panic source (test-only `assert!`/`assert_eq!`/`unwrap()` in
round-trip tests are excluded). The typed fix is either a proven-total refinement (per Part I) or an
`Option`/`Except` at a declared boundary.

#### `models`
| Site | Partiality | Typed fix |
|---|---|---|
| `models/build.rs:13` | protobuf codegen `.expect` | build-time `?` (return `Result`) |

#### `block-storage`
| Site | Partiality | Typed fix |
|---|---|---|
| `block_store.rs:26` | `decompress_size_prepended` `.expect` | `Result` (decompression boundary) |
| `dag/message_map.rs:14` | `get(id).cloned().expect("message not found")` | `Option`/`Except` (content-addressed lookup) |
| `dag/finalizer.rs:52,166` | `get(id)`, `.last()` `.expect` | `Except` (DAG invariant, or prove non-empty) |
| `dag/message_state.rs:104,118` | `expect("empty latest messages")` | `Except` (fringe invariant) |

#### `crypto`
| Site | Partiality | Typed fix |
|---|---|---|
| `encryption/curve25519.rs:57` | `try_into().expect("32-byte public key")` | `TryFrom<&[u8]>` (axiom boundary) |
| `hash/blake2b512_block.rs:171,206,208,209` | `u64::from_le_bytes(…try_into().unwrap())` | `TryFrom<[u8; 8]>` (fixed-width, provable) |
| `hash/blake2b512_random.rs:99,101,252` | `try_into().unwrap()`, `.pop().unwrap()` | `TryFrom` / `Option` |
| `hash/blake2b256_hash.rs:19,25` | `try_into().unwrap()` | `TryFrom<[u8; LENGTH]>` (provable) |
| `signatures/secp256k1.rs:46,57` | `from_slice(…).expect("valid secret key")` | `Result` (key-material validation) |
| `signatures/secp256k1_eth.rs:25` | `sign_bytes(…).expect("valid secret key")` | `Result` |
| `signatures/ed25519.rs:19,25` | `try_into().expect("32-byte secret key")` | `TryFrom` |

#### `shared`
| Site | Partiality | Typed fix |
|---|---|---|
| `base16.rs:21` | `parse_hex_padded(…).expect("filtered digits…")` | `Except` (or prove `filter` invariant) |
| `serialize.rs:28` | `try_into().unwrap()` | `TryFrom` |
| `typed_store.rs:64,100,101` | codec `.expect("decode …")` | `Result` (codec boundary) |

#### `comm`
| Site | Partiality | Typed fix |
|---|---|---|
| `transport/stream_handler.rs:81` | `sender.as_ref().expect("chunk header sender")` | `Except` (protobuf optional field) |
| `transport/grpc_transport_client.rs:66,70` | URI/DNS-name `.expect` | `Result` (endpoint parsing) |
| `rp/protocol_helper.rs:30` | `header.as_ref().expect("header").sender.as_ref().expect("sender")` | `Except` |
| `discovery/peer_table.rs:100,118,137,144,161,173,188` | `lock().unwrap()` (7 sites) | poison-aware error (`PoisonError`) |

#### `rspace`
| Site | Partiality | Typed fix |
|---|---|---|
| `serializers/scodec_serialize.rs:222,236,238,372` | `decode(…).expect("decode …")` | `Result` (codec boundary) |
| `concurrent/multi_lock.rs:100` | `h.await.unwrap()` | poison-aware error |
| `history/radix_tree.rs:135,140,155,161,172,181,202,213` | `lock().unwrap()`, `.expect("cached key…")` | poison-aware / `Option` |
| `history/radix_tree.rs:470` | `.head_option().expect("prefix must be non-empty")` | prove non-empty, or `Except` |
| `history/instances/radix_history.rs:80` | `panic!("history commit failed")` | `Result` (storage commit) |
| `history/instances/rspace_history_reader_impl.rs:78,86,94` | `panic!("unexpected leaf …")` | `Result` (sum-type invariant) |

> **Completeness note:** the sweep is done. Re-running the grep over the workspace (excluding
> `#[cfg(test)]` modules, `build.rs`, and the parser's `self.expect(Tok)` method) leaves zero
> production `.unwrap()`/`.expect(`/`panic!`/`unreachable!` sites, except the two deliberately-unsafe
> `getUnsafe` helpers in `sdk/src/primitive.rs` (`MapOps::get_unsafe` / `TryOps::get_unsafe`), which
> are the explicit Scala `getUnsafe` escape hatch (panic by design, not silent partiality). The
> storage-layer fallibility (`KeyValueStore`/`KeyValueTypedStore` → `Result`) is part of this pass.
>
> **Machine gate.** `tools/audit-type-system.sh` is the authoritative, re-runnable gate: it strips
> `#[cfg(test)]` blocks (brace-depth aware), then fails on production `.unwrap()`/`.expect(`/`panic!`/
> `unreachable!`/`todo!`/`unimplemented!` (whitelisting `sdk/src/primitive.rs` `getUnsafe`),
> `unsafe {`, and silent defaulting of a fallible numeric conversion (`try_into()…unwrap[_or]`,
> `try_from(…)…unwrap_or`, `parse(…)…unwrap_or`). Its `cast`/`get` classes are candidate finders.
> The gate is green (`panic`/`unsafe`/`silent` clean). The full adversarial-audit findings — the
> fixed type-system violations, the *faithful* casts (Scala `Int`/`Long`/`Byte` fixed-width ports
> that must **not** be "fixed"), the ρ-calculus mirroring notes, the red-team register, and the
> Scala-deviation register — are recorded in [`AUDIT.md`](AUDIT.md).

---

## Verification

- `cd spec && lake build` — green (includes `Rchain.Sort`, `Rchain.Rho`, `Rchain.Ty`).
- `#print axioms Rchain.Rho` / `#print axioms Rchain.Ty` — only `propext`, `Classical.choice`,
  `Quot.sound`; **0 `sorry`** and **0 residual axioms** in `Rho.lean`/`Ty.lean`.
- `Ty.lean` imports only `Par`/`Cmp`/`Rho` (not `Sort`), so it is independent of the 30 residual element-comparator
  comparator-law axioms in `Rchain/Sort.lean`.
- Each of the six fundamentals has a named theorem (Part II), with no uncited theorem and no
  uncitable claim.

## Out of scope (this pass)

- **No Rust code changes** — Part III is the catalogue; refactoring `crates/` is a follow-on.
- **No self-hosting CoC** — embedded in Lean, not a bespoke type theory as data.
- **No discharge of the 30 residual element-comparator `Sort.lean` axioms** — the type system is independent of Law 1's
  total-order bundle.
