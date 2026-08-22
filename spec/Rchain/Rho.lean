import Rchain.Par

/-!
# The ρ-calculus base sort over the flat `Par`

The node executes the reflective higher-order **ρ-calculus**: a single grammar in which a *name* is a
quoted process (`@Proc`) and a *process* can dereference a name (`*Name`), with parallel composition
`|`, replication (`!`, carried by the `persistent` flags), `new` (fresh unforgeable names), `match`,
and the ground/arithmetic expressions. The flat `Par` ADT in `Rchain.Par` *erases* the quote/eval
distinction — a `Par` in name position *is* a name — so the reflective core is recovered in the type
layer (`Rchain.Ty`).

This module gives the ρ-calculus **structural congruence** `≡` (Law 2 core): parallel composition is
commutative/associative with identity `nilPar`, and `≡` is a congruence. This is the
`P | Q = Q | P`, `P = P | Nil`, `(P|Q)|R = P|(Q|R)` fragment of `name-equivalence.k` and
`rholangmatchingtut.md`. Deep α-equivalence and capture-avoiding substitution stay Coq's obligation
(Laws 2–3, per `AGENTS.md`); here we state the congruence the type system needs and prove the
fundamentals (`Rchain.Ty`) over it.
-/

namespace Rchain

/-- Structural congruence `≡` on processes (Law 2 core: par order, `| Nil`, associativity, congruence). -/
inductive StrCong : Par → Par → Prop where
  | refl  : ∀ p, StrCong p p
  | symm  : ∀ {p q}, StrCong p q → StrCong q p
  | trans : ∀ {p q r}, StrCong p q → StrCong q r → StrCong p r
  | comm  : ∀ p q, StrCong (parMerge p q) (parMerge q p)
  | assoc : ∀ p q r, StrCong (parMerge (parMerge p q) r) (parMerge p (parMerge q r))
  | ident : ∀ p, StrCong (parMerge p nilPar) p
  | par   : ∀ {p p' q q'}, StrCong p p' → StrCong q q' → StrCong (parMerge p q) (parMerge p' q')

/-- `≡` is an equivalence relation (refl/symm/trans are built in). -/
theorem strCong_equivalence : Equivalence StrCong :=
  ⟨StrCong.refl, StrCong.symm, StrCong.trans⟩

theorem strCong_comm (p q : Par) : StrCong (parMerge p q) (parMerge q p) := StrCong.comm p q

theorem strCong_assoc (p q r : Par) :
    StrCong (parMerge (parMerge p q) r) (parMerge p (parMerge q r)) := StrCong.assoc p q r

theorem strCong_ident (p : Par) : StrCong (parMerge p nilPar) p := StrCong.ident p

/-- `nilPar` is also a *left* identity, by commutativity + right identity. -/
theorem strCong_nil_left (p : Par) : StrCong (parMerge nilPar p) p :=
  StrCong.trans (StrCong.comm nilPar p) (StrCong.ident p)

/-! ## Reduction (Law 4 core): COMM + congruence -/

/-- A send on channel `chan` with data `data` (non-persistent). -/
def sendPar (chan : Par) (data : List Par) : Par :=
  Par.mk [Send.mk chan data false] [] [] [] [] [] [] []

/-- A receive on channel `chan` with body `body` (single bind, non-persistent). -/
def receivePar (chan : Par) (body : Par) : Par :=
  Par.mk [] [Receive.mk [ReceiveBind.mk [chan] body 1] body false 1] [] [] [] [] [] []

/-- Minimal reduction `⟶` (Law 4 core). COMM contracts a send/receive on the same channel to the
    receive body; reduction is a congruence under `|`. The capture-avoiding substitution of the
    sent data for the bound level — and replication (`!`/`!!` re-inserting the redex) and `new`
    freshness — are Coq's Autosubst/α-equivalence obligations (`AGENTS.md`); the redex contraction
    to the body is exactly the fact the closedness-preservation theorem in `Rchain.Ty` needs. -/
inductive Reduce : Par → Par → Prop where
  | comm (chan data body : Par) :
      Reduce (parMerge (sendPar chan [data]) (receivePar chan body)) body
  | parLeft {p p' q : Par} : Reduce p p' → Reduce (parMerge p q) (parMerge p' q)
  | parRight {p q q' : Par} : Reduce q q' → Reduce (parMerge p q) (parMerge p q')

end Rchain
