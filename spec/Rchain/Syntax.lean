import Mathlib.Data.String.Basic

/-!
# Rholang leaf types (M2 gate)

Ground scalars and variables, shared by the flat `Par` ADT in `Rchain.Par`. The Phase-0 *binary*
`Proc` fragment has been removed (its Law 1 proof lives in git history as a regression anchor); the
real `Par` is the flat record defined in `Rchain.Par`.

`Ground` and `Var` derive `LinearOrder` so that their canonical comparison is the constructor-
declaration order (`bool < int < str` (code points); `bound < free < wildcard`), which is *exactly* the Phase-0
`cmpGround`/`cmpVar` — but now lawful via Mathlib's `cmp` machinery with no axioms.

Binders use de Bruijn *levels*, matching the Scala `Var` (`bound_var`/`free_var`/`wildcard`).
-/

namespace Rchain

/-- Ground scalar values (`Expr` with a `GBool`/`GInt`/`GString` instance). -/
inductive Ground where
  | bool (b : Bool)
  | int  (n : Int)
  | str  (l : List Nat)  -- Unicode code points
deriving BEq, Ord, DecidableEq

/-- Variables, represented as de Bruijn levels (bound/free) or a wildcard. -/
inductive Var where
  | bound (level : Nat)  -- bound_var
  | free  (level : Nat)  -- free_var
  | wildcard             -- `_`
deriving BEq, Ord, DecidableEq

end Rchain
