(* Rchain flat `Par` ADT (M2 gate) — mirrors spec/Rchain/Par.lean.

   The Scala source of truth is `models/src/main/protobuf/RhoTypes.proto:32-43`:
   `Par` is a **flat** record of 8 *repeated* (list) fields, not a tree. Each field's
   element type is a small record with its own arity and flags. Binders use de Bruijn
   *levels* (`Bound`/`Free`), matching the Scala `Var` (`bound_var`/`free_var`/`wildcard`).

   This replaces the Phase-0 binary `Proc` fragment. `GStr` carries code points as
   `list N` (Unicode code points), so string comparison is the lexicographic order on
   code points — which makes `GStr` comparison lawful with only the stdlib
   (`N.compare_eq_iff`/`N.compare_antisym`), eliminating the Phase-0 base axiom. *)

Require Import List NArith ZArith Bool.
Import ListNotations.

(* Ground scalar values (`Expr` with a `GBool`/`GInt`/`GString` instance). *)
Inductive Ground : Type :=
  | GBool (b : bool)
  | GInt  (n : Z)
  | GStr  (l : list N).   (* Unicode code points *)

(* Variables as de Bruijn levels (bound/free) or a wildcard. *)
Inductive Var : Type :=
  | Bound (level : nat)
  | Free  (level : nat)
  | Wildcard.

(* The flat process family, mutually recursive (per RhoTypes.proto:32-43). *)
Inductive Par : Type :=
| ParMk : list Send -> list Receive -> list New -> list Expr -> list Match ->
          list GUnforgeable -> list Bundle -> list Connective -> Par
with Send : Type :=
| SendMk : Par -> list Par -> bool -> Send
with ReceiveBind : Type :=
| ReceiveBindMk : list Par -> Par -> nat -> ReceiveBind
with Receive : Type :=
| ReceiveMk : list ReceiveBind -> Par -> bool -> nat -> Receive
with New : Type :=
| NewMk : nat -> Par -> New
with MatchCase : Type :=
| MatchCaseMk : Par -> Par -> nat -> MatchCase
with Match : Type :=
| MatchMk : Par -> list MatchCase -> Match
with Expr : Type :=
| EGround  : Ground -> Expr
| EVar     : Var -> Expr
| ENeg     : Par -> Expr
| ENot     : Par -> Expr
| EPlus    : Par -> Par -> Expr
| EMinus   : Par -> Par -> Expr
| EMult    : Par -> Par -> Expr
| EDiv     : Par -> Par -> Expr
| EMod     : Par -> Par -> Expr
| ELt      : Par -> Par -> Expr
| ELe      : Par -> Par -> Expr
| EGt      : Par -> Par -> Expr
| EGe      : Par -> Par -> Expr
| EEq      : Par -> Par -> Expr
| ENeq     : Par -> Par -> Expr
| EAnd     : Par -> Par -> Expr
| EOr      : Par -> Par -> Expr
| EList    : list Par -> Expr
| ETuple   : list Par -> Expr
| ESet     : list Par -> Expr
| EMap     : list (Par * Par) -> Expr
with Bundle : Type :=
| BundleMk : Par -> bool -> bool -> Bundle
with GUnforgeable : Type :=
| GPrivate     : nat -> GUnforgeable
| GDeployId    : nat -> GUnforgeable
| GDeployerId  : GUnforgeable
| GSysAuthToken : GUnforgeable
with Connective : Type :=
| ConnAnd    : list Par -> Connective
| ConnOr     : list Par -> Connective
| ConnNot    : Par -> Connective
| ConnVarRef : nat -> nat -> Connective.

(* ------------------------------------------------------------------ *)
(* Accessors for `Par`'s 8 fields.                                     *)
(* ------------------------------------------------------------------ *)

Definition par_sends (p : Par) : list Send :=
  match p with ParMk s _ _ _ _ _ _ _ => s end.
Definition par_receives (p : Par) : list Receive :=
  match p with ParMk _ r _ _ _ _ _ _ => r end.
Definition par_news (p : Par) : list New :=
  match p with ParMk _ _ n _ _ _ _ _ => n end.
Definition par_exprs (p : Par) : list Expr :=
  match p with ParMk _ _ _ e _ _ _ _ => e end.
Definition par_matches (p : Par) : list Match :=
  match p with ParMk _ _ _ _ m _ _ _ => m end.
Definition par_unforgeables (p : Par) : list GUnforgeable :=
  match p with ParMk _ _ _ _ _ u _ _ => u end.
Definition par_bundles (p : Par) : list Bundle :=
  match p with ParMk _ _ _ _ _ _ b _ => b end.
Definition par_connectives (p : Par) : list Connective :=
  match p with ParMk _ _ _ _ _ _ _ c => c end.

Definition send_chan (s : Send) : Par := match s with SendMk c _ _ => c end.
Definition send_data (s : Send) : list Par := match s with SendMk _ d _ => d end.
Definition send_persistent (s : Send) : bool := match s with SendMk _ _ p => p end.

Definition receive_binds (r : Receive) : list ReceiveBind := match r with ReceiveMk b _ _ _ => b end.
Definition receive_body (r : Receive) : Par := match r with ReceiveMk _ b _ _ => b end.
Definition receive_persistent (r : Receive) : bool := match r with ReceiveMk _ _ p _ => p end.
Definition receive_bindCount (r : Receive) : nat := match r with ReceiveMk _ _ _ n => n end.

Definition rb_patterns (rb : ReceiveBind) : list Par := match rb with ReceiveBindMk ps _ _ => ps end.
Definition rb_source (rb : ReceiveBind) : Par := match rb with ReceiveBindMk _ s _ => s end.
Definition rb_freeCount (rb : ReceiveBind) : nat := match rb with ReceiveBindMk _ _ n => n end.

Definition new_bindCount (n : New) : nat := match n with NewMk n _ => n end.
Definition new_body (n : New) : Par := match n with NewMk _ b => b end.

Definition mc_pattern (m : MatchCase) : Par := match m with MatchCaseMk p _ _ => p end.
Definition mc_source (m : MatchCase) : Par := match m with MatchCaseMk _ s _ => s end.
Definition mc_freeCount (m : MatchCase) : nat := match m with MatchCaseMk _ _ n => n end.

Definition match_target (m : Match) : Par := match m with MatchMk t _ => t end.
Definition match_cases (m : Match) : list MatchCase := match m with MatchMk _ c => c end.

Definition bundle_body (b : Bundle) : Par := match b with BundleMk b _ _ => b end.
Definition bundle_writeFlag (b : Bundle) : bool := match b with BundleMk _ w _ => w end.
Definition bundle_readFlag (b : Bundle) : bool := match b with BundleMk _ _ r => r end.

(* ------------------------------------------------------------------ *)
(* The empty process `Nil` — the `Par` with all 8 fields empty.         *)
(* ------------------------------------------------------------------ *)

Definition nilPar : Par := ParMk [] [] [] [] [] [] [] [].

(* ------------------------------------------------------------------ *)
(* `parMerge p q` = `p | q` — field-wise multiset union (list append).  *)
(* ------------------------------------------------------------------ *)

Definition parMerge (p q : Par) : Par :=
  ParMk (par_sends p ++ par_sends q)
        (par_receives p ++ par_receives q)
        (par_news p ++ par_news q)
        (par_exprs p ++ par_exprs q)
        (par_matches p ++ par_matches q)
        (par_unforgeables p ++ par_unforgeables q)
        (par_bundles p ++ par_bundles q)
        (par_connectives p ++ par_connectives q).
