(* Canonicalization (Law 1) over the flat `Par` — M2 gate, axiomatized.

   The Phase-0 *binary* `Proc` proof of Law 1 was committed (see git history) but the flat `Par`
   ADT (a record of 8 `list` fields, mutually recursive with `Send`/`Receive`/…/`Connective`)
   requires a well-founded mutual recursion (`Program Fixpoint` + measure) that has not yet been
   ported to Coq. Until then, the comparator and canonical sort are declared as axioms, mirroring
   the Lean track's residual axioms.

   Law 1 is `sortPar (sortPar p) = sortPar p` (idempotence) and
   `sortPar (parMerge p q) = sortPar (parMerge q p)` (commutativity). *)

From Rchain Require Import Syntax.
Require Import ZArith.

(* The structural total order (constructor declaration order, lexicographic via `lex`). *)
Axiom cmpPar : Par -> Par -> comparison.

(* The canonical sort: sort each of the 8 fields' elements by the corresponding comparator. *)
Axiom sortPar : Par -> Par.

(* Law 1 (admitted): sort is idempotent and `|` (parMerge) is commutative after sorting. *)
Axiom sortPar_idempotent : forall p : Par, sortPar (sortPar p) = sortPar p.
Axiom sortPar_comm : forall p q : Par, sortPar (parMerge p q) = sortPar (parMerge q p).
