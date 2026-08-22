import Mathlib.Data.List.Sort
import Mathlib.Order.Compare
import Mathlib.Data.String.Basic

/-!
# Lawful comparators and canonical list sorting

The generic scaffolding for Law 1 over the flat `Par`. A `Comparator α` is a lawful three-way
comparison (`cmp : α → α → Ordering`) with the total-order bundle (`eq_iff`, `swap`, `lt_trans`).
From it we derive the induced `≤` (`le`) as a linear order (refl/antisymm/total/trans), obtain
`cmpList`/`cmpPair`/`listComparator`/`sortList`, and prove the list lemmas Law 1 needs:

- `sortList_idempotent` : sorting a sorted list is a no-op;
- `sortList_append_comm` : sorting is permutation-invariant, so `l ++ l'` and `l' ++ l` agree;
- `sortList_map_idempotent` : an idempotent `map` under a canonical sort is a fixed point.

The bare (`*F`) list/pair comparisons take a raw `α → α → Ordering` together with its lawfulness as
explicit hypotheses, so they can be used *inside* the mutual definition of the 11 comparators in
`Rchain.Sort` (whose lawfulness is only established afterwards, by mutual induction).
-/

namespace Rchain

/-! ## `lex` and its order laws -/

/-- Lexicographic composition of two comparisons. -/
def lex (o1 o2 : Ordering) : Ordering :=
  match o1 with
  | .eq => o2
  | o   => o

theorem lex_eq_iff (o1 o2 : Ordering) : lex o1 o2 = Ordering.eq ↔ o1 = Ordering.eq ∧ o2 = Ordering.eq := by
  cases o1 <;> cases o2 <;> simp [lex]

theorem lex_lt_iff (o1 o2 : Ordering) : lex o1 o2 = Ordering.lt ↔ o1 = Ordering.lt ∨ (o1 = Ordering.eq ∧ o2 = Ordering.lt) := by
  cases o1 <;> cases o2 <;> simp [lex]

theorem swap_lex (o1 o2 : Ordering) : Ordering.swap (lex o1 o2) = lex (Ordering.swap o1) (Ordering.swap o2) := by
  cases o1 <;> cases o2 <;> simp [lex, Ordering.swap]

/-! ## The `Comparator` structure and the induced linear order -/

/-- A lawful three-way comparator. -/
structure Comparator (α : Type u) where
  cmp : α → α → Ordering
  eq_iff : ∀ {a b : α}, cmp a b = Ordering.eq ↔ a = b
  swap : ∀ {a b : α}, cmp b a = Ordering.swap (cmp a b)
  lt_trans : ∀ {a b c : α}, cmp a b = Ordering.lt → cmp b c = Ordering.lt → cmp a c = Ordering.lt

namespace Comparator

variable {α : Type u} {β : Type v}

/-- The `≤` relation induced by a comparator: `cmp a b` is `.lt` or `.eq`. -/
def le (C : Comparator α) (a b : α) : Prop :=
  C.cmp a b = Ordering.lt ∨ C.cmp a b = Ordering.eq

theorem total (C : Comparator α) (a b : α) :
    C.cmp a b = Ordering.lt ∨ C.cmp a b = Ordering.eq ∨ C.cmp a b = Ordering.gt := by
  cases C.cmp a b <;> simp

theorem le_refl (C : Comparator α) (a : α) : C.le a a :=
  Or.inr (C.eq_iff.mpr rfl)

theorem le_antisymm (C : Comparator α) {a b : α} (hab : C.le a b) (hba : C.le b a) : a = b := by
  rcases hab with hab | hab
  · have hb : C.cmp b a = Ordering.gt := by rw [C.swap, hab]; rfl
    rcases hba with hba | hba
    · rw [hb] at hba; cases hba
    · rw [hb] at hba; cases hba
  · exact C.eq_iff.mp hab

theorem le_total (C : Comparator α) (a b : α) : C.le a b ∨ C.le b a := by
  by_cases h : C.cmp a b = Ordering.gt
  · right; left; rw [C.swap, h]; rfl
  · left
    cases h' : C.cmp a b with
    | lt => exact Or.inl h'
    | eq => exact Or.inr h'
    | gt => exact False.elim (h h')

theorem le_trans (C : Comparator α) {a b c : α} (hab : C.le a b) (hbc : C.le b c) : C.le a c := by
  rcases hab with hab | hab
  · rcases hbc with hbc | hbc
    · exact Or.inl (C.lt_trans hab hbc)
    · have hbc' : b = c := C.eq_iff.mp hbc
      exact Or.inl (hbc' ▸ hab)
  · have hab' : a = b := C.eq_iff.mp hab
    exact hab'.symm ▸ hbc

instance instIsRefl (C : Comparator α) : IsRefl α C.le := ⟨fun a => le_refl C a⟩
instance instIsTrans (C : Comparator α) : IsTrans α C.le := ⟨fun _ _ _ hab hbc => le_trans C hab hbc⟩
instance instIsTotal (C : Comparator α) : IsTotal α C.le := ⟨fun a b => le_total C a b⟩
instance instIsAntisymm (C : Comparator α) : IsAntisymm α C.le := ⟨fun _ _ hab hba => le_antisymm C hab hba⟩
instance instDecidableRel (C : Comparator α) : DecidableRel C.le :=
  fun a b => inferInstanceAs (Decidable (C.cmp a b = Ordering.lt ∨ C.cmp a b = Ordering.eq))

/-! ## Leaf comparators from any linear order -/

/-- The canonical comparator for a `LinearOrder` (e.g. `Bool`, `Nat`, `Int`, `String`). -/
def linearOrderComparator (α : Type u) [LinearOrder α] : Comparator α where
  cmp := _root_.cmp (α := α)
  eq_iff := by intro a b; exact @cmp_eq_eq_iff α _ a b
  swap := by intro a b; exact (cmp_swap a b).symm
  lt_trans := by
    intro a b c h1 h2
    rw [cmp_eq_lt_iff (x := a) (y := b)] at h1
    rw [cmp_eq_lt_iff (x := b) (y := c)] at h2
    rw [cmp_eq_lt_iff (x := a) (y := c)]
    exact _root_.lt_trans h1 h2

/-! ## Lexicographic transitivity helper -/

/-- Transitivity of `lex (f a b) (Dcmp x y)` given transitivity of the two parts. -/
theorem lex_lt_trans {f : α → α → Ordering}
    (h_eq : ∀ {a b : α}, f a b = Ordering.eq ↔ a = b)
    (h_lt : ∀ {a b c : α}, f a b = Ordering.lt → f b c = Ordering.lt → f a c = Ordering.lt)
    {Dcmp : β → β → Ordering} {a b c : α} {x y z : β}
    (hD : Dcmp x y = Ordering.lt → Dcmp y z = Ordering.lt → Dcmp x z = Ordering.lt) :
    lex (f a b) (Dcmp x y) = Ordering.lt →
    lex (f b c) (Dcmp y z) = Ordering.lt →
    lex (f a c) (Dcmp x z) = Ordering.lt := by
  intro h1 h2
  rw [lex_lt_iff] at h1 h2 ⊢
  rcases h1 with h1 | ⟨h1e, h1d⟩
  · rcases h2 with h2 | ⟨h2e, _⟩
    · exact Or.inl (h_lt h1 h2)
    · exact Or.inl ((h_eq.mp h2e) ▸ h1)
  · rcases h2 with h2 | ⟨h2e, h2d⟩
    · exact Or.inl ((h_eq.mp h1e).symm ▸ h2)
    · exact Or.inr ⟨h_eq.mpr ((h_eq.mp h1e).trans (h_eq.mp h2e)), hD h1d h2d⟩

/-! ## List and pair comparators -/

/-- Lexicographic comparison of lists, over a bare element comparator. -/
def cmpListF (f : α → α → Ordering) : List α → List α → Ordering
  | [], [] => Ordering.eq
  | [], _ => Ordering.lt
  | _, [] => Ordering.gt
  | a :: as, b :: bs => lex (f a b) (cmpListF f as bs)

/-- Lexicographic comparison of lists, over a lawful element comparator. -/
def cmpList (C : Comparator α) : List α → List α → Ordering := cmpListF C.cmp

theorem cmpListF_eq_iff (f : α → α → Ordering) (h_eq : ∀ {a b : α}, f a b = Ordering.eq ↔ a = b)
    (l l' : List α) : cmpListF f l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListF]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListF]
      | cons b bs => simp [cmpListF, lex_eq_iff, h_eq, ih, List.cons.injEq]

theorem cmpList_eq_iff (C : Comparator α) (l l' : List α) : cmpList C l l' = Ordering.eq ↔ l = l' :=
  cmpListF_eq_iff C.cmp C.eq_iff l l'

theorem cmpListF_swap (f : α → α → Ordering) (h_swap : ∀ {a b : α}, f b a = Ordering.swap (f a b))
    (l l' : List α) : cmpListF f l' l = Ordering.swap (cmpListF f l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListF, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListF, Ordering.swap]
      | cons b bs =>
          simp only [cmpListF]
          rw [swap_lex, ← h_swap, ← ih bs]

theorem cmpList_swap (C : Comparator α) (l l' : List α) : cmpList C l' l = Ordering.swap (cmpList C l l') :=
  cmpListF_swap C.cmp C.swap l l'

theorem cmpListF_lt_trans (f : α → α → Ordering)
    (h_eq : ∀ {a b : α}, f a b = Ordering.eq ↔ a = b)
    (h_lt : ∀ {a b c : α}, f a b = Ordering.lt → f b c = Ordering.lt → f a c = Ordering.lt)
    (l l' l'' : List α) :
    cmpListF f l l' = Ordering.lt → cmpListF f l' l'' = Ordering.lt → cmpListF f l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListF] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListF] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListF] at h2
          | cons c cs => exact lex_lt_trans (f := f) (h_eq := h_eq) (h_lt := h_lt) (hD := ih bs cs) h1 h2

theorem cmpList_lt_trans (C : Comparator α) (l l' l'' : List α) :
    cmpList C l l' = Ordering.lt → cmpList C l' l'' = Ordering.lt → cmpList C l l'' = Ordering.lt := by
  unfold cmpList
  exact cmpListF_lt_trans C.cmp C.eq_iff C.lt_trans l l' l''

/-- The list comparator built from an element comparator. -/
def listComparator (C : Comparator α) : Comparator (List α) where
  cmp := cmpList C
  eq_iff := by intro a b; exact cmpList_eq_iff C a b
  swap := by intro a b; exact cmpList_swap C a b
  lt_trans := by intro a b c; exact cmpList_lt_trans C a b c

/-- Lexicographic comparison of pairs, over bare element comparators. -/
def cmpPairF (f : α → α → Ordering) (g : β → β → Ordering) : (α × β) → (α × β) → Ordering
  | (a, b), (c, d) => lex (f a c) (g b d)

/-- The product comparator, lexicographic on `(fst, snd)`. -/
def cmpPair (C : Comparator α) (D : Comparator β) : Comparator (α × β) where
  cmp := cmpPairF C.cmp D.cmp
  eq_iff := by
    intro a b
    simp [cmpPairF, lex_eq_iff, C.eq_iff, D.eq_iff, Prod.ext_iff]
  swap := by
    intro a b
    change lex (C.cmp b.1 a.1) (D.cmp b.2 a.2) = Ordering.swap (lex (C.cmp a.1 b.1) (D.cmp a.2 b.2))
    rw [swap_lex, ← C.swap, ← D.swap]
  lt_trans := by
    intro a b c h1 h2
    exact lex_lt_trans (f := C.cmp) (h_eq := C.eq_iff) (h_lt := C.lt_trans)
      (hD := D.lt_trans (a := a.2) (b := b.2) (c := c.2)) h1 h2

/-! ## Canonical list sorting -/

/-- Sort a list by the induced `≤` (insertion sort; Mathlib). -/
def sortList (C : Comparator α) (l : List α) : List α :=
  l.insertionSort C.le

theorem sortList_idempotent (C : Comparator α) (l : List α) : sortList C (sortList C l) = sortList C l := by
  unfold sortList
  exact (List.sorted_insertionSort C.le l).insertionSort_eq

theorem sortList_perm (C : Comparator α) {l l' : List α} (h : List.Perm l l') : sortList C l = sortList C l' := by
  unfold sortList
  refine List.eq_of_perm_of_sorted (r := C.le) ?_ ?_ ?_
  · exact (List.perm_insertionSort C.le l).trans (h.trans (List.perm_insertionSort C.le l').symm)
  · exact List.sorted_insertionSort C.le l
  · exact List.sorted_insertionSort C.le l'

theorem sortList_append_comm (C : Comparator α) (l l' : List α) : sortList C (l ++ l') = sortList C (l' ++ l) := by
  exact sortList_perm C (List.perm_append_comm (l₁ := l) (l₂ := l'))

/-- If `f` is idempotent, then `sortList C (l.map f)` is a fixed point of `sortList C (·.map f)`. -/
theorem sortList_map_idempotent (C : Comparator α) {f : α → α} (hf : ∀ x, f (f x) = f x) (l : List α) :
    sortList C ((sortList C (l.map f)).map f) = sortList C (l.map f) := by
  have hmap : (sortList C (l.map f)).map f = sortList C (l.map f) := by
    unfold sortList
    conv_rhs => rw [← List.map_id (List.insertionSort C.le (l.map f))]
    apply List.map_congr_left
    intro x hx
    rw [List.mem_insertionSort C.le] at hx
    rcases List.mem_map.mp hx with ⟨y, _, rfl⟩
    exact hf y
  rw [hmap]
  exact sortList_idempotent C (l.map f)

end Comparator

end Rchain
