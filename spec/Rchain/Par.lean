import Rchain.Syntax

set_option maxHeartbeats 1000000

/-!
# The flat `Par` ADT (M2 gate)

The Scala source of truth is `models/src/main/protobuf/RhoTypes.proto:32-43`: `Par` is a **flat**
record of 8 *repeated* (list) fields, not a tree. Each field's element type is a small record with
its own arity and flags. This module defines that mutual type family, replacing the Phase-0 binary
`Proc` (which was a fragment). Binders use de Bruijn *levels* (`Var.bound`/`Var.free`).

This is the ADT that Law 1's real `sort`, Law 2's `≡`, Law 3's substitution, and Laws 4–5 all act on.
-/

namespace Rchain

mutual
  /-- A process: 8 parallel `list` fields, each kept sorted by `sort` (Law 1). -/
  inductive Par where
    | mk : List Send → List Receive → List New → List Expr → List Match →
           List GUnforgeable → List Bundle → List Connective → Par

  /-- `Send` — channel, data, and the persistent flag (`!` vs `!!`). -/
  inductive Send where
    | mk : Par → List Par → Bool → Send

  /-- `ReceiveBind` — patterns, source, and the free-var count for the pattern. -/
  inductive ReceiveBind where
    | mk : List Par → Par → Nat → ReceiveBind

  /-- `Receive` — binds, body, persistent flag (`<-` vs `<=`), and bind count. -/
  inductive Receive where
    | mk : List ReceiveBind → Par → Bool → Nat → Receive

  /-- `New` — `new bindCount in body` (fresh unforgeable names). -/
  inductive New where
    | mk : Nat → Par → New

  /-- `MatchCase` — pattern, source/body, and the pattern's free-var count. -/
  inductive MatchCase where
    | mk : Par → Par → Nat → MatchCase

  /-- `Match` — target and cases (first-match-wins). -/
  inductive Match where
    | mk : Par → List MatchCase → Match

  /-- `Expr` — ground values and the arithmetic/logical nodes Laws 2 and 4 need. -/
  inductive Expr where
    | ground : Ground → Expr
    | evar   : Var → Expr
    | eneg   : Par → Expr
    | enot   : Par → Expr
    | eplus  : Par → Par → Expr
    | eminus : Par → Par → Expr
    | emult  : Par → Par → Expr
    | ediv   : Par → Par → Expr
    | emod   : Par → Par → Expr
    | elt    : Par → Par → Expr
    | ele    : Par → Par → Expr
    | egt    : Par → Par → Expr
    | ege    : Par → Par → Expr
    | eeq    : Par → Par → Expr
    | eneq   : Par → Par → Expr
    | eand   : Par → Par → Expr
    | eor    : Par → Par → Expr
    | elist  : List Par → Expr
    | etuple : List Par → Expr
    | eset   : List Par → Expr
    | emap   : List (Par × Par) → Expr

  /-- `Bundle` — body plus the read/write capability flags. -/
  inductive Bundle where
    | mk : Par → Bool → Bool → Bundle

  /-- `GUnforgeable` — fresh names from `new` and the system/identity tokens. -/
  inductive GUnforgeable where
    | gPrivate    : Nat → GUnforgeable
    | gDeployId   : Nat → GUnforgeable
    | gDeployerId : GUnforgeable
    | gSysAuthToken : GUnforgeable

  /-- `Connective` — pattern connectives (minimal set; expanded in Law 5). -/
  inductive Connective where
    | connAnd   : List Par → Connective
    | connOr    : List Par → Connective
    | connNot   : Par → Connective
    | connVarRef : Nat → Nat → Connective
end

/-- Accessors for `Par`'s 8 fields. -/
@[simp] def Par.sends        : Par → List Send         | Par.mk s _ _ _ _ _ _ _ => s
@[simp] def Par.receives     : Par → List Receive      | Par.mk _ r _ _ _ _ _ _ => r
@[simp] def Par.news         : Par → List New          | Par.mk _ _ n _ _ _ _ _ => n
@[simp] def Par.exprs        : Par → List Expr         | Par.mk _ _ _ e _ _ _ _ => e
@[simp] def Par.matches      : Par → List Match        | Par.mk _ _ _ _ m _ _ _ => m
@[simp] def Par.unforgeables : Par → List GUnforgeable | Par.mk _ _ _ _ _ u _ _ => u
@[simp] def Par.bundles      : Par → List Bundle       | Par.mk _ _ _ _ _ _ b _ => b
@[simp] def Par.connectives  : Par → List Connective   | Par.mk _ _ _ _ _ _ _ c => c

def Send.chan       : Send → Par      | Send.mk c _ _ => c
def Send.data       : Send → List Par | Send.mk _ d _ => d
def Send.persistent : Send → Bool     | Send.mk _ _ p => p

def Receive.binds      : Receive → List ReceiveBind | Receive.mk bs _ _ _ => bs
def Receive.body       : Receive → Par             | Receive.mk _ b _ _ => b
def Receive.persistent : Receive → Bool            | Receive.mk _ _ p _ => p
def Receive.bindCount  : Receive → Nat             | Receive.mk _ _ _ n => n

def ReceiveBind.patterns  : ReceiveBind → List Par | ReceiveBind.mk ps _ _ => ps
def ReceiveBind.source    : ReceiveBind → Par      | ReceiveBind.mk _ s _ => s
def ReceiveBind.freeCount : ReceiveBind → Nat      | ReceiveBind.mk _ _ n => n

def New.bindCount : New → Nat | New.mk n _ => n
def New.body      : New → Par | New.mk _ b => b

def MatchCase.pattern   : MatchCase → Par | MatchCase.mk p _ _ => p
def MatchCase.source    : MatchCase → Par | MatchCase.mk _ s _ => s
def MatchCase.freeCount : MatchCase → Nat | MatchCase.mk _ _ n => n

def Match.target : Match → Par            | Match.mk t _ => t
def Match.cases  : Match → List MatchCase | Match.mk _ cs => cs

def Bundle.body      : Bundle → Par  | Bundle.mk b _ _ => b
def Bundle.writeFlag : Bundle → Bool | Bundle.mk _ w _ => w
def Bundle.readFlag  : Bundle → Bool | Bundle.mk _ _ r => r

/-- The empty process `Nil` — the `Par` with all 8 fields empty. -/
def nilPar : Par :=
  Par.mk [] [] [] [] [] [] [] []

/-- `parMerge p q` = `p | q` — field-wise multiset union (list append). -/
def parMerge (p q : Par) : Par :=
  Par.mk (p.sends ++ q.sends) (p.receives ++ q.receives) (p.news ++ q.news)
         (p.exprs ++ q.exprs) (p.matches ++ q.matches) (p.unforgeables ++ q.unforgeables)
         (p.bundles ++ q.bundles) (p.connectives ++ q.connectives)

end Rchain
