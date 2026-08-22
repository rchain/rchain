(* Laws 2–6 statements over the flat `Par` — the Coq substitution/α-equivalence metatheory.

   Coq owns the *definitions* (capture-avoiding de Bruijn substitution, α-equivalence) per
   `AGENTS.md`; this file states each law's signature precisely (axiomatized) over `Syntax.v` so the
   catalog is complete. The definitions themselves remain Phase-1 obligations (Autosubst-style). *)

From Rchain Require Import Syntax Sort.
Require Import List.

(* ------------------------------------------------------------------ *)
(* Law 2 — α / name equivalence.                                       *)
(* ------------------------------------------------------------------ *)

(* `alpha_equiv p q` — p and q are equal up to bound-variable renaming (and the quote/eval
   structural equalities of `name-equivalence.k`). *)
Axiom alpha_equiv : Par -> Par -> Prop.
Axiom alpha_equiv_refl : forall p : Par, alpha_equiv p p.
Axiom alpha_equiv_symm : forall p q : Par, alpha_equiv p q -> alpha_equiv q p.
Axiom alpha_equiv_trans : forall p q r : Par, alpha_equiv p q -> alpha_equiv q r -> alpha_equiv p r.

(* ------------------------------------------------------------------ *)
(* Law 3 — capture-avoiding de Bruijn substitution.                    *)
(* ------------------------------------------------------------------ *)

(* A simultaneous substitution: one `Par` per de Bruijn level. *)
Definition Subst : Type := Var -> Par.

(* `substPar σ p` — substitute every free occurrence of a level by its image under σ, shifting
   bound levels through binders (capture-avoiding). *)
Axiom substPar : Subst -> Par -> Par.

(* Law 3: canonicalization commutes with substitution (`sort(subst t) = subst(sort t)`). *)
Axiom subst_commutes_sort :
  forall (σ : Subst) (p : Par), sortPar (substPar σ p) = substPar σ (sortPar p).

(* ------------------------------------------------------------------ *)
(* Law 4 — reduction (COMM).                                           *)
(* ------------------------------------------------------------------ *)

(* `reduce p q` — p reduces to q by a COMM contraction (first-match-wins). *)
Axiom reduce : Par -> Par -> Prop.

(* ------------------------------------------------------------------ *)
(* Law 5 — spatial matching / a free var is bound at most once.        *)
(* ------------------------------------------------------------------ *)

(* `spatial_matches target pattern` — the spatial matcher accepts `target` against `pattern`. *)
Axiom spatial_matches : Par -> Par -> Prop.

(* Law 5: `binds_at_most_once p` — every free variable in `p` occurs at most once
   (`addedVars.distinct`). *)
Axiom binds_at_most_once : Par -> Prop.

(* ------------------------------------------------------------------ *)
(* Law 6 — no globally free variables.                                 *)
(* ------------------------------------------------------------------ *)

(* `closed p` — p has no free (unbound) de Bruijn level. *)
Axiom closed : Par -> Prop.

(* Law 6: closedness is decidable (the interpreter's `Closed` refinement newtype). *)
Axiom closed_decidable : forall p : Par, {closed p} + {~ closed p}.
