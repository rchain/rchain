# Formal specification & audit

The mathematical specification of the node lives in the [`spec/`](../../../spec/) tree, outside this
book. This page is an index to it; it does not duplicate its contents.

| Document | What it is |
|---|---|
| [`spec/RHO-CALCULUS.md`](../../../spec/RHO-CALCULUS.md) | The ρ-calculus core: the reflective sorted grammar (`Name = @Proc`, `Proc = *Name | …`), the `PSort` judgment, the flat 8-field `Par`, and the refinements. |
| [`spec/INVENTORY.md`](../../../spec/INVENTORY.md) | The **19-law invariant catalog** — one row per law, with source-of-truth, Rust realization, Lean/Coq target, and status. |
| [`spec/TYPE-SYSTEM.md`](../../../spec/TYPE-SYSTEM.md) | The ρ→CoC type discipline: totality (`TotalOn`), refinement sigma-types, and the "no silent partiality" guarantee. |
| [`spec/AUDIT.md`](../../../spec/AUDIT.md) | The adversarial audit findings register and the Scala-deviation register. |
| [`spec/Rchain/`](../../../spec/Rchain/) (Lean 4) | The machine-checked definitions and theorems (`lake build`). |
| [`spec/coq/`](../../../spec/coq/) (Coq) | The substitution / α-equivalence metatheory (`make`). |

The executable semantics of the language are the K-framework rules under
[`legacy/rholang/src/main/k/rholang/`](../../../legacy/rholang/src/main/k/rholang/) — the operational
definition of Laws 2–6.

The machine gate for the port's type-system conformance is
[`tools/audit-type-system.sh`](../../../tools/audit-type-system.sh), which fails the build on any
production `panic!`/`unsafe`/silent-conversion in the Rust crates.
