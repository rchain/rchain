import Rchain.Par
import Rchain.Cmp

set_option maxHeartbeats 100000000

/-!
# Canonicalization (Law 1) over the flat `Par`

Mirrors `models/src/main/scala/coop/rchain/models/rholang/sorter/ScoreTree.scala` and
`ordering.scala`: `Par` (and `ESet`/`EMap`) are canonicalized to a total order so that structural
equality of the sorted form *is* process equality up to α.

The total order is a hand-rolled structural `cmpPar` (constructor declaration order, lexicographic
via `lex`), order-isomorphic to the Scala `ScoreTree` order. It is defined by **direct mutual
recursion** (each list field has its own comparator, so the recursion is fully first-order), which
Lean accepts with a `sizeOf` measure.

Law 1 is `sortPar (sortPar p) = sortPar p` (idempotence) and
`sortPar (parMerge p q) = sortPar (parMerge q p)` (commutativity), proven from the total-order
bundle on each comparator via the generic `sortList` canonicality in `Rchain.Cmp`.
-/

namespace Rchain
open Comparator

/-! ## Leaf comparators (`Ground`, `Var`) -/

/-- Structural comparison of `Ground`, constructor-declaration order (`bool < int < str`). -/
def cmpGround : Ground → Ground → Ordering
  | .bool b, .bool b' => _root_.cmp b b'
  | .bool _, _ => .lt | _, .bool _ => .gt
  | .int n, .int n' => _root_.cmp n n'
  | .int _, _ => .lt | _, .int _ => .gt
  | .str l, .str l' => cmpListF (fun n m => _root_.cmp n m) l l'

/-- Structural comparison of `Var`, constructor-declaration order (`bound < free < wildcard`). -/
def cmpVar : Var → Var → Ordering
  | .bound n, .bound m => _root_.cmp n m
  | .bound _, _ => .lt | _, .bound _ => .gt
  | .free n, .free m => _root_.cmp n m
  | .free _, _ => .lt | _, .free _ => .gt
  | .wildcard, .wildcard => .eq

/-- `cmpGround` is a lawful comparator. -/
def groundComparator : Comparator Ground where
  cmp := cmpGround
  eq_iff := by
    intro a b
    cases a <;> cases b <;> simp [cmpGround]
    all_goals first
      | exact cmp_eq_eq_iff
      | exact (linearOrderComparator Int).eq_iff
      | exact (listComparator (linearOrderComparator Nat)).eq_iff
  swap := by
    intro a b
    cases a <;> cases b <;> simp [cmpGround]
    all_goals first
      | rfl
      | exact (linearOrderComparator Bool).swap
      | exact (linearOrderComparator Int).swap
      | exact (listComparator (linearOrderComparator Nat)).swap
  lt_trans := by
    intro a b c h1 h2
    cases a <;> cases b <;> cases c <;> simp [cmpGround] at h1 h2 ⊢
    all_goals first
      | exact _root_.lt_trans h1 h2
      | exact (listComparator (linearOrderComparator Nat)).lt_trans h1 h2

/-- `cmpVar` is a lawful comparator. -/
def varComparator : Comparator Var where
  cmp := cmpVar
  eq_iff := by
    intro a b
    cases a <;> cases b <;> simp [cmpVar]
    all_goals exact cmp_eq_eq_iff
  swap := by
    intro a b
    cases a <;> cases b <;> simp [cmpVar]
    all_goals first
      | rfl
      | exact (linearOrderComparator Nat).swap
  lt_trans := by
    intro a b c h1 h2
    cases a <;> cases b <;> cases c <;> simp [cmpVar] at h1 h2 ⊢
    all_goals exact _root_.lt_trans h1 h2

@[simp] theorem cmpGround_eq_iff (a b : Ground) : cmpGround a b = Ordering.eq ↔ a = b := groundComparator.eq_iff
@[simp] theorem cmpGround_swap (a b : Ground) : cmpGround b a = Ordering.swap (cmpGround a b) := groundComparator.swap
@[simp] theorem cmpVar_eq_iff (a b : Var) : cmpVar a b = Ordering.eq ↔ a = b := varComparator.eq_iff
@[simp] theorem cmpVar_swap (a b : Var) : cmpVar b a = Ordering.swap (cmpVar a b) := varComparator.swap

/-! ## The comparator family (direct mutual recursion) -/

mutual
  def cmpPar : Par → Par → Ordering
    | Par.mk s r n e m u b c, Par.mk s' r' n' e' m' u' b' c' =>
        lex (cmpListSend s s') (lex (cmpListReceive r r') (lex (cmpListNew n n')
        (lex (cmpListExpr e e') (lex (cmpListMatch m m') (lex (cmpListGUnforgeable u u')
        (lex (cmpListBundle b b') (cmpListConnective c c')))))))
  termination_by p q => sizeOf p + sizeOf q
  def cmpSend : Send → Send → Ordering
    | Send.mk c d p, Send.mk c' d' p' => lex (cmpPar c c') (lex (cmpListPar d d') (_root_.cmp p p'))
  termination_by s t => sizeOf s + sizeOf t
  def cmpReceiveBind : ReceiveBind → ReceiveBind → Ordering
    | ReceiveBind.mk ps s n, ReceiveBind.mk ps' s' n' => lex (cmpListPar ps ps') (lex (cmpPar s s') (_root_.cmp n n'))
  termination_by s t => sizeOf s + sizeOf t
  def cmpReceive : Receive → Receive → Ordering
    | Receive.mk bs b p n, Receive.mk bs' b' p' n' => lex (cmpListReceiveBind bs bs') (lex (cmpPar b b') (lex (_root_.cmp p p') (_root_.cmp n n')))
  termination_by s t => sizeOf s + sizeOf t
  def cmpNew : New → New → Ordering
    | New.mk n b, New.mk n' b' => lex (_root_.cmp n n') (cmpPar b b')
  termination_by s t => sizeOf s + sizeOf t
  def cmpMatchCase : MatchCase → MatchCase → Ordering
    | MatchCase.mk p s n, MatchCase.mk p' s' n' => lex (cmpPar p p') (lex (cmpPar s s') (_root_.cmp n n'))
  termination_by s t => sizeOf s + sizeOf t
  def cmpMatch : Match → Match → Ordering
    | Match.mk t cs, Match.mk t' cs' => lex (cmpPar t t') (cmpListMatchCase cs cs')
  termination_by s t => sizeOf s + sizeOf t
  def cmpExpr : Expr → Expr → Ordering
    | Expr.ground g, Expr.ground g' => cmpGround g g'
    | Expr.ground _, _ => .lt | _, Expr.ground _ => .gt
    | Expr.evar v, Expr.evar v' => cmpVar v v'
    | Expr.evar _, _ => .lt | _, Expr.evar _ => .gt
    | Expr.eneg p, Expr.eneg p' => cmpPar p p'
    | Expr.eneg _, _ => .lt | _, Expr.eneg _ => .gt
    | Expr.enot p, Expr.enot p' => cmpPar p p'
    | Expr.enot _, _ => .lt | _, Expr.enot _ => .gt
    | Expr.eplus p q, Expr.eplus p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.eplus _ _, _ => .lt | _, Expr.eplus _ _ => .gt
    | Expr.eminus p q, Expr.eminus p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.eminus _ _, _ => .lt | _, Expr.eminus _ _ => .gt
    | Expr.emult p q, Expr.emult p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.emult _ _, _ => .lt | _, Expr.emult _ _ => .gt
    | Expr.ediv p q, Expr.ediv p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.ediv _ _, _ => .lt | _, Expr.ediv _ _ => .gt
    | Expr.emod p q, Expr.emod p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.emod _ _, _ => .lt | _, Expr.emod _ _ => .gt
    | Expr.elt p q, Expr.elt p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.elt _ _, _ => .lt | _, Expr.elt _ _ => .gt
    | Expr.ele p q, Expr.ele p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.ele _ _, _ => .lt | _, Expr.ele _ _ => .gt
    | Expr.egt p q, Expr.egt p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.egt _ _, _ => .lt | _, Expr.egt _ _ => .gt
    | Expr.ege p q, Expr.ege p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.ege _ _, _ => .lt | _, Expr.ege _ _ => .gt
    | Expr.eeq p q, Expr.eeq p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.eeq _ _, _ => .lt | _, Expr.eeq _ _ => .gt
    | Expr.eneq p q, Expr.eneq p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.eneq _ _, _ => .lt | _, Expr.eneq _ _ => .gt
    | Expr.eand p q, Expr.eand p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.eand _ _, _ => .lt | _, Expr.eand _ _ => .gt
    | Expr.eor p q, Expr.eor p' q' => lex (cmpPar p p') (cmpPar q q')
    | Expr.eor _ _, _ => .lt | _, Expr.eor _ _ => .gt
    | Expr.elist ps, Expr.elist ps' => cmpListPar ps ps'
    | Expr.elist _, _ => .lt | _, Expr.elist _ => .gt
    | Expr.etuple ps, Expr.etuple ps' => cmpListPar ps ps'
    | Expr.etuple _, _ => .lt | _, Expr.etuple _ => .gt
    | Expr.eset ps, Expr.eset ps' => cmpListPar ps ps'
    | Expr.eset _, _ => .lt | _, Expr.eset _ => .gt
    | Expr.emap kvs, Expr.emap kvs' => cmpListParPair kvs kvs'
  termination_by s t => sizeOf s + sizeOf t
  def cmpBundle : Bundle → Bundle → Ordering
    | Bundle.mk b w r, Bundle.mk b' w' r' => lex (cmpPar b b') (lex (_root_.cmp w w') (_root_.cmp r r'))
  termination_by s t => sizeOf s + sizeOf t
  def cmpGUnforgeable : GUnforgeable → GUnforgeable → Ordering
    | GUnforgeable.gPrivate n, GUnforgeable.gPrivate n' => _root_.cmp n n'
    | GUnforgeable.gPrivate _, _ => .lt | _, GUnforgeable.gPrivate _ => .gt
    | GUnforgeable.gDeployId n, GUnforgeable.gDeployId n' => _root_.cmp n n'
    | GUnforgeable.gDeployId _, _ => .lt | _, GUnforgeable.gDeployId _ => .gt
    | GUnforgeable.gDeployerId, GUnforgeable.gDeployerId => .eq
    | GUnforgeable.gDeployerId, _ => .lt | _, GUnforgeable.gDeployerId => .gt
    | GUnforgeable.gSysAuthToken, GUnforgeable.gSysAuthToken => .eq
  termination_by s t => sizeOf s + sizeOf t
  def cmpConnective : Connective → Connective → Ordering
    | Connective.connAnd ps, Connective.connAnd ps' => cmpListPar ps ps'
    | Connective.connAnd _, _ => .lt | _, Connective.connAnd _ => .gt
    | Connective.connOr ps, Connective.connOr ps' => cmpListPar ps ps'
    | Connective.connOr _, _ => .lt | _, Connective.connOr _ => .gt
    | Connective.connNot p, Connective.connNot p' => cmpPar p p'
    | Connective.connNot _, _ => .lt | _, Connective.connNot _ => .gt
    | Connective.connVarRef d n, Connective.connVarRef d' n' => lex (_root_.cmp d d') (_root_.cmp n n')
  termination_by s t => sizeOf s + sizeOf t
  def cmpListSend : List Send → List Send → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpSend a b) (cmpListSend as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListReceive : List Receive → List Receive → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpReceive a b) (cmpListReceive as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListNew : List New → List New → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpNew a b) (cmpListNew as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListExpr : List Expr → List Expr → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpExpr a b) (cmpListExpr as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListMatch : List Match → List Match → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpMatch a b) (cmpListMatch as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListGUnforgeable : List GUnforgeable → List GUnforgeable → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpGUnforgeable a b) (cmpListGUnforgeable as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListBundle : List Bundle → List Bundle → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpBundle a b) (cmpListBundle as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListConnective : List Connective → List Connective → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpConnective a b) (cmpListConnective as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListPar : List Par → List Par → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpPar a b) (cmpListPar as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListReceiveBind : List ReceiveBind → List ReceiveBind → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpReceiveBind a b) (cmpListReceiveBind as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListMatchCase : List MatchCase → List MatchCase → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | a :: as, b :: bs => lex (cmpMatchCase a b) (cmpListMatchCase as bs)
  termination_by l l' => sizeOf l + sizeOf l'
  def cmpListParPair : List (Par × Par) → List (Par × Par) → Ordering
    | [], [] => .eq | [], _ => .lt | _, [] => .gt
    | (a, b) :: as, (c, d) :: bs => lex (lex (cmpPar a c) (cmpPar b d)) (cmpListParPair as bs)
  termination_by l l' => sizeOf l + sizeOf l'
end

/-! ## Sum-type comparator (single well-founded recursion)

The comparator family above is a *mutual* recursion over the AST; proving its laws by mutual induction
hangs Lean's termination checker. This section re-expresses it as a single well-founded function over a
sum type, which supports standard structural induction on the sum type's `SizeOf`.
-/

inductive Sortable where
  | par : Par → Sortable
  | send : Send → Sortable
  | receiveBind : ReceiveBind → Sortable
  | receive : Receive → Sortable
  | new : New → Sortable
  | matchCase : MatchCase → Sortable
  | match : Match → Sortable
  | expr : Expr → Sortable
  | bundle : Bundle → Sortable
  | gUnforgeable : GUnforgeable → Sortable
  | connective : Connective → Sortable
  | parPair : Par × Par → Sortable
  | listSend : List Send → Sortable
  | listReceive : List Receive → Sortable
  | listNew : List New → Sortable
  | listExpr : List Expr → Sortable
  | listMatch : List Match → Sortable
  | listGUnforgeable : List GUnforgeable → Sortable
  | listBundle : List Bundle → Sortable
  | listConnective : List Connective → Sortable
  | listPar : List Par → Sortable
  | listReceiveBind : List ReceiveBind → Sortable
  | listMatchCase : List MatchCase → Sortable
  | listParPair : List (Par × Par) → Sortable

/-- Structural size of a `Sortable` (for the well-founded termination of `cmpSortable`). -/
noncomputable def sortableSize : Sortable → Nat
  | .par p => sizeOf p
  | .send s => sizeOf s
  | .receiveBind rb => sizeOf rb
  | .receive r => sizeOf r
  | .new n => sizeOf n
  | .matchCase mc => sizeOf mc
  | .match m => sizeOf m
  | .expr e => sizeOf e
  | .bundle b => sizeOf b
  | .gUnforgeable u => sizeOf u
  | .connective c => sizeOf c
  | .parPair (a, b) => sizeOf a + sizeOf b + 1
  | .listSend s => sizeOf s
  | .listReceive r => sizeOf r
  | .listNew n => sizeOf n
  | .listExpr e => sizeOf e
  | .listMatch m => sizeOf m
  | .listGUnforgeable u => sizeOf u
  | .listBundle b => sizeOf b
  | .listConnective c => sizeOf c
  | .listPar p => sizeOf p
  | .listReceiveBind rb => sizeOf rb
  | .listMatchCase mc => sizeOf mc
  | .listParPair kvs => sizeOf kvs

def cmpSortable : Sortable → Sortable → Ordering
  | .par (Par.mk s r n e m u b c), .par (Par.mk s' r' n' e' m' u' b' c') =>
      lex (cmpSortable (.listSend s) (.listSend s')) (lex (cmpSortable (.listReceive r) (.listReceive r'))
      (lex (cmpSortable (.listNew n) (.listNew n')) (lex (cmpSortable (.listExpr e) (.listExpr e'))
      (lex (cmpSortable (.listMatch m) (.listMatch m')) (lex (cmpSortable (.listGUnforgeable u) (.listGUnforgeable u'))
      (lex (cmpSortable (.listBundle b) (.listBundle b')) (cmpSortable (.listConnective c) (.listConnective c'))))))))
  | .send (Send.mk c d p), .send (Send.mk c' d' p') =>
      lex (cmpSortable (.par c) (.par c')) (lex (cmpSortable (.listPar d) (.listPar d')) (_root_.cmp p p'))
  | .receiveBind (ReceiveBind.mk ps s n), .receiveBind (ReceiveBind.mk ps' s' n') =>
      lex (cmpSortable (.listPar ps) (.listPar ps')) (lex (cmpSortable (.par s) (.par s')) (_root_.cmp n n'))
  | .receive (Receive.mk bs b p n), .receive (Receive.mk bs' b' p' n') =>
      lex (cmpSortable (.listReceiveBind bs) (.listReceiveBind bs')) (lex (cmpSortable (.par b) (.par b'))
      (lex (_root_.cmp p p') (_root_.cmp n n')))
  | .new (New.mk n b), .new (New.mk n' b') => lex (_root_.cmp n n') (cmpSortable (.par b) (.par b'))
  | .matchCase (MatchCase.mk p s n), .matchCase (MatchCase.mk p' s' n') =>
      lex (cmpSortable (.par p) (.par p')) (lex (cmpSortable (.par s) (.par s')) (_root_.cmp n n'))
  | .match (Match.mk t cs), .match (Match.mk t' cs') =>
      lex (cmpSortable (.par t) (.par t')) (cmpSortable (.listMatchCase cs) (.listMatchCase cs'))
  | .expr e, .expr e' => cmpExpr e e'
  | .bundle (Bundle.mk b w r), .bundle (Bundle.mk b' w' r') =>
      lex (cmpSortable (.par b) (.par b')) (lex (_root_.cmp w w') (_root_.cmp r r'))
  | .gUnforgeable u, .gUnforgeable u' => cmpGUnforgeable u u'
  | .connective (Connective.connAnd ps), .connective (Connective.connAnd ps') => cmpSortable (.listPar ps) (.listPar ps')
  | .connective (Connective.connAnd _), .connective _ => .lt
  | .connective _, .connective (Connective.connAnd _) => .gt
  | .connective (Connective.connOr ps), .connective (Connective.connOr ps') => cmpSortable (.listPar ps) (.listPar ps')
  | .connective (Connective.connOr _), .connective _ => .lt
  | .connective _, .connective (Connective.connOr _) => .gt
  | .connective (Connective.connNot p), .connective (Connective.connNot p') => cmpSortable (.par p) (.par p')
  | .connective (Connective.connNot _), .connective _ => .lt
  | .connective _, .connective (Connective.connNot _) => .gt
  | .connective (Connective.connVarRef d n), .connective (Connective.connVarRef d' n') =>
      lex (_root_.cmp d d') (_root_.cmp n n')
  | .parPair (a1, a2), .parPair (b1, b2) =>
      lex (cmpSortable (.par a1) (.par b1)) (cmpSortable (.par a2) (.par b2))
  | .listSend [], .listSend [] => .eq
  | .listSend [], .listSend _ => .lt
  | .listSend _, .listSend [] => .gt
  | .listSend (a :: as), .listSend (b :: bs) =>
      lex (cmpSortable (.send a) (.send b)) (cmpSortable (.listSend as) (.listSend bs))
  | .listReceive [], .listReceive [] => .eq
  | .listReceive [], .listReceive _ => .lt
  | .listReceive _, .listReceive [] => .gt
  | .listReceive (a :: as), .listReceive (b :: bs) =>
      lex (cmpSortable (.receive a) (.receive b)) (cmpSortable (.listReceive as) (.listReceive bs))
  | .listNew [], .listNew [] => .eq
  | .listNew [], .listNew _ => .lt
  | .listNew _, .listNew [] => .gt
  | .listNew (a :: as), .listNew (b :: bs) =>
      lex (cmpSortable (.new a) (.new b)) (cmpSortable (.listNew as) (.listNew bs))
  | .listExpr [], .listExpr [] => .eq
  | .listExpr [], .listExpr _ => .lt
  | .listExpr _, .listExpr [] => .gt
  | .listExpr (a :: as), .listExpr (b :: bs) =>
      lex (cmpSortable (.expr a) (.expr b)) (cmpSortable (.listExpr as) (.listExpr bs))
  | .listMatch [], .listMatch [] => .eq
  | .listMatch [], .listMatch _ => .lt
  | .listMatch _, .listMatch [] => .gt
  | .listMatch (a :: as), .listMatch (b :: bs) =>
      lex (cmpSortable (.match a) (.match b)) (cmpSortable (.listMatch as) (.listMatch bs))
  | .listGUnforgeable [], .listGUnforgeable [] => .eq
  | .listGUnforgeable [], .listGUnforgeable _ => .lt
  | .listGUnforgeable _, .listGUnforgeable [] => .gt
  | .listGUnforgeable (a :: as), .listGUnforgeable (b :: bs) =>
      lex (cmpSortable (.gUnforgeable a) (.gUnforgeable b)) (cmpSortable (.listGUnforgeable as) (.listGUnforgeable bs))
  | .listBundle [], .listBundle [] => .eq
  | .listBundle [], .listBundle _ => .lt
  | .listBundle _, .listBundle [] => .gt
  | .listBundle (a :: as), .listBundle (b :: bs) =>
      lex (cmpSortable (.bundle a) (.bundle b)) (cmpSortable (.listBundle as) (.listBundle bs))
  | .listConnective [], .listConnective [] => .eq
  | .listConnective [], .listConnective _ => .lt
  | .listConnective _, .listConnective [] => .gt
  | .listConnective (a :: as), .listConnective (b :: bs) =>
      lex (cmpSortable (.connective a) (.connective b)) (cmpSortable (.listConnective as) (.listConnective bs))
  | .listPar [], .listPar [] => .eq
  | .listPar [], .listPar _ => .lt
  | .listPar _, .listPar [] => .gt
  | .listPar (a :: as), .listPar (b :: bs) =>
      lex (cmpSortable (.par a) (.par b)) (cmpSortable (.listPar as) (.listPar bs))
  | .listReceiveBind [], .listReceiveBind [] => .eq
  | .listReceiveBind [], .listReceiveBind _ => .lt
  | .listReceiveBind _, .listReceiveBind [] => .gt
  | .listReceiveBind (a :: as), .listReceiveBind (b :: bs) =>
      lex (cmpSortable (.receiveBind a) (.receiveBind b)) (cmpSortable (.listReceiveBind as) (.listReceiveBind bs))
  | .listMatchCase [], .listMatchCase [] => .eq
  | .listMatchCase [], .listMatchCase _ => .lt
  | .listMatchCase _, .listMatchCase [] => .gt
  | .listMatchCase (a :: as), .listMatchCase (b :: bs) =>
      lex (cmpSortable (.matchCase a) (.matchCase b)) (cmpSortable (.listMatchCase as) (.listMatchCase bs))
  | .listParPair [], .listParPair [] => .eq
  | .listParPair [], .listParPair _ => .lt
  | .listParPair _, .listParPair [] => .gt
  | .listParPair ((a1, a2) :: as), .listParPair ((b1, b2) :: bs) =>
      lex (cmpSortable (.parPair (a1, a2)) (.parPair (b1, b2))) (cmpSortable (.listParPair as) (.listParPair bs))
  | _, _ => .lt
  termination_by x y => sortableSize x + sortableSize y
  decreasing_by all_goals (simp [sortableSize]; omega)

/-! The `cmpSortable` laws (`eq_iff`/`swap`/`lt_trans`) are the remaining step to discharge the 30
element-comparator axioms: they are provable by well-founded induction on `sortableSize` (the flat
`Sortable` type's structural induction does *not* reach the nested AST sub-terms, so the mutual block's
two-argument induction is what the sum type replaces). The projection `cmpPar p q = cmpSortable (.par p)
(.par q)` then yields the element laws. Left as a documented follow-up: writing the well-founded
induction proof (via `cmpSortable.induct` or `WellFounded.fix`).
-/

/-! ## Lawfulness: `eq_iff` (RESIDUAL AXIOMS — now 30, down from 69)

The structural comparators reflect equality (`cmp a b = .eq ↔ a = b`). These are part of the 30
residual "the order is total" axioms; see the `lt_trans` note for why the 11-function mutual
induction has not been discharged.
-/

axiom cmpPar_eq_iff (p q : Par) : cmpPar p q = Ordering.eq ↔ p = q
axiom cmpSend_eq_iff (s t : Send) : cmpSend s t = Ordering.eq ↔ s = t
axiom cmpReceiveBind_eq_iff (s t : ReceiveBind) : cmpReceiveBind s t = Ordering.eq ↔ s = t
axiom cmpReceive_eq_iff (s t : Receive) : cmpReceive s t = Ordering.eq ↔ s = t
axiom cmpNew_eq_iff (s t : New) : cmpNew s t = Ordering.eq ↔ s = t
axiom cmpMatchCase_eq_iff (s t : MatchCase) : cmpMatchCase s t = Ordering.eq ↔ s = t
axiom cmpMatch_eq_iff (s t : Match) : cmpMatch s t = Ordering.eq ↔ s = t
axiom cmpExpr_eq_iff (s t : Expr) : cmpExpr s t = Ordering.eq ↔ s = t
axiom cmpBundle_eq_iff (s t : Bundle) : cmpBundle s t = Ordering.eq ↔ s = t
theorem cmpGUnforgeable_eq_iff (s t : GUnforgeable) : cmpGUnforgeable s t = Ordering.eq ↔ s = t := by
  cases s <;> cases t <;> simp [cmpGUnforgeable]
  all_goals exact cmp_eq_eq_iff
axiom cmpConnective_eq_iff (s t : Connective) : cmpConnective s t = Ordering.eq ↔ s = t
theorem cmpListSend_eq_iff (l l' : List Send) : cmpListSend l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListSend]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListSend]
      | cons b bs => simp [cmpListSend, lex_eq_iff, cmpSend_eq_iff, ih, List.cons.injEq]
theorem cmpListReceive_eq_iff (l l' : List Receive) : cmpListReceive l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListReceive]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListReceive]
      | cons b bs => simp [cmpListReceive, lex_eq_iff, cmpReceive_eq_iff, ih, List.cons.injEq]
theorem cmpListNew_eq_iff (l l' : List New) : cmpListNew l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListNew]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListNew]
      | cons b bs => simp [cmpListNew, lex_eq_iff, cmpNew_eq_iff, ih, List.cons.injEq]
theorem cmpListExpr_eq_iff (l l' : List Expr) : cmpListExpr l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListExpr]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListExpr]
      | cons b bs => simp [cmpListExpr, lex_eq_iff, cmpExpr_eq_iff, ih, List.cons.injEq]
theorem cmpListMatch_eq_iff (l l' : List Match) : cmpListMatch l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListMatch]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListMatch]
      | cons b bs => simp [cmpListMatch, lex_eq_iff, cmpMatch_eq_iff, ih, List.cons.injEq]
theorem cmpListGUnforgeable_eq_iff (l l' : List GUnforgeable) : cmpListGUnforgeable l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListGUnforgeable]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListGUnforgeable]
      | cons b bs => simp [cmpListGUnforgeable, lex_eq_iff, cmpGUnforgeable_eq_iff, ih, List.cons.injEq]
theorem cmpListBundle_eq_iff (l l' : List Bundle) : cmpListBundle l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListBundle]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListBundle]
      | cons b bs => simp [cmpListBundle, lex_eq_iff, cmpBundle_eq_iff, ih, List.cons.injEq]
theorem cmpListConnective_eq_iff (l l' : List Connective) : cmpListConnective l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListConnective]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListConnective]
      | cons b bs => simp [cmpListConnective, lex_eq_iff, cmpConnective_eq_iff, ih, List.cons.injEq]
theorem cmpListPar_eq_iff (l l' : List Par) : cmpListPar l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListPar]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListPar]
      | cons b bs => simp [cmpListPar, lex_eq_iff, cmpPar_eq_iff, ih, List.cons.injEq]
theorem cmpListReceiveBind_eq_iff (l l' : List ReceiveBind) : cmpListReceiveBind l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListReceiveBind]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListReceiveBind]
      | cons b bs => simp [cmpListReceiveBind, lex_eq_iff, cmpReceiveBind_eq_iff, ih, List.cons.injEq]
theorem cmpListMatchCase_eq_iff (l l' : List MatchCase) : cmpListMatchCase l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListMatchCase]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListMatchCase]
      | cons b bs => simp [cmpListMatchCase, lex_eq_iff, cmpMatchCase_eq_iff, ih, List.cons.injEq]
theorem cmpListParPair_eq_iff (l l' : List (Par × Par)) : cmpListParPair l l' = Ordering.eq ↔ l = l' := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListParPair]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListParPair]
      | cons b bs =>
          cases a with | mk a1 a2 =>
          cases b with | mk b1 b2 =>
          simp [cmpListParPair, lex_eq_iff, cmpPar_eq_iff, ih, List.cons.injEq, Prod.ext_iff]

/-! ## Lawfulness: `swap` (RESIDUAL AXIOMS)

Residual: the 10 element-comparator `swap` laws (the 12 list-comparator and `cmpGUnforgeable` `swap`
laws are discharged below); see the `lt_trans` note for the Lean limitation.
-/

axiom cmpPar_swap (p q : Par) : cmpPar q p = Ordering.swap (cmpPar p q)
axiom cmpSend_swap (s t : Send) : cmpSend t s = Ordering.swap (cmpSend s t)
axiom cmpReceiveBind_swap (s t : ReceiveBind) : cmpReceiveBind t s = Ordering.swap (cmpReceiveBind s t)
axiom cmpReceive_swap (s t : Receive) : cmpReceive t s = Ordering.swap (cmpReceive s t)
axiom cmpNew_swap (s t : New) : cmpNew t s = Ordering.swap (cmpNew s t)
axiom cmpMatchCase_swap (s t : MatchCase) : cmpMatchCase t s = Ordering.swap (cmpMatchCase s t)
axiom cmpMatch_swap (s t : Match) : cmpMatch t s = Ordering.swap (cmpMatch s t)
axiom cmpExpr_swap (s t : Expr) : cmpExpr t s = Ordering.swap (cmpExpr s t)
axiom cmpBundle_swap (s t : Bundle) : cmpBundle t s = Ordering.swap (cmpBundle s t)
theorem cmpGUnforgeable_swap (s t : GUnforgeable) : cmpGUnforgeable t s = Ordering.swap (cmpGUnforgeable s t) := by
  cases s <;> cases t <;> simp [cmpGUnforgeable]
  all_goals first
    | rfl
    | exact (linearOrderComparator Nat).swap
axiom cmpConnective_swap (s t : Connective) : cmpConnective t s = Ordering.swap (cmpConnective s t)
theorem cmpListSend_swap (l l' : List Send) : cmpListSend l' l = Ordering.swap (cmpListSend l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListSend, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListSend, Ordering.swap]
      | cons b bs =>
          simp only [cmpListSend]
          rw [swap_lex, ← cmpSend_swap, ← ih bs]
theorem cmpListReceive_swap (l l' : List Receive) : cmpListReceive l' l = Ordering.swap (cmpListReceive l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListReceive, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListReceive, Ordering.swap]
      | cons b bs =>
          simp only [cmpListReceive]
          rw [swap_lex, ← cmpReceive_swap, ← ih bs]
theorem cmpListNew_swap (l l' : List New) : cmpListNew l' l = Ordering.swap (cmpListNew l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListNew, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListNew, Ordering.swap]
      | cons b bs =>
          simp only [cmpListNew]
          rw [swap_lex, ← cmpNew_swap, ← ih bs]
theorem cmpListExpr_swap (l l' : List Expr) : cmpListExpr l' l = Ordering.swap (cmpListExpr l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListExpr, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListExpr, Ordering.swap]
      | cons b bs =>
          simp only [cmpListExpr]
          rw [swap_lex, ← cmpExpr_swap, ← ih bs]
theorem cmpListMatch_swap (l l' : List Match) : cmpListMatch l' l = Ordering.swap (cmpListMatch l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListMatch, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListMatch, Ordering.swap]
      | cons b bs =>
          simp only [cmpListMatch]
          rw [swap_lex, ← cmpMatch_swap, ← ih bs]
theorem cmpListGUnforgeable_swap (l l' : List GUnforgeable) : cmpListGUnforgeable l' l = Ordering.swap (cmpListGUnforgeable l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListGUnforgeable, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListGUnforgeable, Ordering.swap]
      | cons b bs =>
          simp only [cmpListGUnforgeable]
          rw [swap_lex, ← cmpGUnforgeable_swap, ← ih bs]
theorem cmpListBundle_swap (l l' : List Bundle) : cmpListBundle l' l = Ordering.swap (cmpListBundle l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListBundle, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListBundle, Ordering.swap]
      | cons b bs =>
          simp only [cmpListBundle]
          rw [swap_lex, ← cmpBundle_swap, ← ih bs]
theorem cmpListConnective_swap (l l' : List Connective) : cmpListConnective l' l = Ordering.swap (cmpListConnective l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListConnective, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListConnective, Ordering.swap]
      | cons b bs =>
          simp only [cmpListConnective]
          rw [swap_lex, ← cmpConnective_swap, ← ih bs]
theorem cmpListPar_swap (l l' : List Par) : cmpListPar l' l = Ordering.swap (cmpListPar l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListPar, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListPar, Ordering.swap]
      | cons b bs =>
          simp only [cmpListPar]
          rw [swap_lex, ← cmpPar_swap, ← ih bs]
theorem cmpListReceiveBind_swap (l l' : List ReceiveBind) : cmpListReceiveBind l' l = Ordering.swap (cmpListReceiveBind l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListReceiveBind, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListReceiveBind, Ordering.swap]
      | cons b bs =>
          simp only [cmpListReceiveBind]
          rw [swap_lex, ← cmpReceiveBind_swap, ← ih bs]
theorem cmpListMatchCase_swap (l l' : List MatchCase) : cmpListMatchCase l' l = Ordering.swap (cmpListMatchCase l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListMatchCase, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListMatchCase, Ordering.swap]
      | cons b bs =>
          simp only [cmpListMatchCase]
          rw [swap_lex, ← cmpMatchCase_swap, ← ih bs]
theorem cmpListParPair_swap (l l' : List (Par × Par)) : cmpListParPair l' l = Ordering.swap (cmpListParPair l l') := by
  induction l generalizing l' with
  | nil => cases l' <;> simp [cmpListParPair, Ordering.swap]
  | cons a as ih =>
      cases l' with
      | nil => simp [cmpListParPair, Ordering.swap]
      | cons b bs =>
          cases a with | mk a1 a2 =>
          cases b with | mk b1 b2 =>
          simp only [cmpListParPair]
          rw [swap_lex, swap_lex, ← cmpPar_swap, ← cmpPar_swap, ← ih bs]

/-! ## Lawfulness: `lt_trans` (RESIDUAL AXIOMS — now 30, down from 69)

The 12 **list** comparators' `eq_iff`/`swap`/`lt_trans` laws are now **discharged** (direct induction
on the list, composing the element law with `lex_eq_iff`/`swap_lex`/`lex_lt_trans`); they were never
the hard part. The remaining **30 axioms** are the 10 **element** comparators' laws
(`cmpPar`/`cmpSend`/…/`cmpConnective` × `eq_iff`/`swap`/`lt_trans`; `cmpGUnforgeable` is discharged),
which need mutual induction over the AST.

Discharging the element laws is blocked by a Lean limitation, not by choice:

  * a `mutual` theorem block over the **two-argument** `cmpX` family hangs the termination checker —
    both with bare `termination_by p q => sizeOf p + sizeOf q` and with
    `decreasing_by all_goals (simp_wf; omega)`;
  * the generated induction principle `Rchain.cmpPar.mutual_induct` fails to *derive*:
    "Cannot derive functional induction principle" with a deterministic `whnf` heartbeat timeout.

The sum-type refactor (above) re-expresses the family as the single well-founded `cmpSortable`, whose
*definition* is done (termination proven); the remaining step is the `cmpSortable` laws proof by
well-founded induction on `sortableSize`.

The one-argument `sortX_idempotent` mutual block (above) proves fine; the blocker is specific to the
two-argument sum measure. The path forward is a refactor — a single well-founded recursion over a sum
type `Par ⊕ Send ⊕ … ⊕ List (Par × Par)`, or Mathlib `SizeOf`/`Finset` machinery in Phase 1 — rather
than more tactics.
-/

axiom cmpPar_lt_trans (p q r : Par) : cmpPar p q = Ordering.lt → cmpPar q r = Ordering.lt → cmpPar p r = Ordering.lt
axiom cmpSend_lt_trans (s t u : Send) : cmpSend s t = Ordering.lt → cmpSend t u = Ordering.lt → cmpSend s u = Ordering.lt
axiom cmpReceiveBind_lt_trans (s t u : ReceiveBind) : cmpReceiveBind s t = Ordering.lt → cmpReceiveBind t u = Ordering.lt → cmpReceiveBind s u = Ordering.lt
axiom cmpReceive_lt_trans (s t u : Receive) : cmpReceive s t = Ordering.lt → cmpReceive t u = Ordering.lt → cmpReceive s u = Ordering.lt
axiom cmpNew_lt_trans (s t u : New) : cmpNew s t = Ordering.lt → cmpNew t u = Ordering.lt → cmpNew s u = Ordering.lt
axiom cmpMatchCase_lt_trans (s t u : MatchCase) : cmpMatchCase s t = Ordering.lt → cmpMatchCase t u = Ordering.lt → cmpMatchCase s u = Ordering.lt
axiom cmpMatch_lt_trans (s t u : Match) : cmpMatch s t = Ordering.lt → cmpMatch t u = Ordering.lt → cmpMatch s u = Ordering.lt
axiom cmpExpr_lt_trans (s t u : Expr) : cmpExpr s t = Ordering.lt → cmpExpr t u = Ordering.lt → cmpExpr s u = Ordering.lt
axiom cmpBundle_lt_trans (s t u : Bundle) : cmpBundle s t = Ordering.lt → cmpBundle t u = Ordering.lt → cmpBundle s u = Ordering.lt
theorem cmpGUnforgeable_lt_trans (s t u : GUnforgeable) : cmpGUnforgeable s t = Ordering.lt → cmpGUnforgeable t u = Ordering.lt → cmpGUnforgeable s u = Ordering.lt := by
  intro h1 h2
  cases s <;> cases t <;> cases u <;> simp [cmpGUnforgeable] at h1 h2 ⊢
  all_goals exact _root_.lt_trans h1 h2
axiom cmpConnective_lt_trans (s t u : Connective) : cmpConnective s t = Ordering.lt → cmpConnective t u = Ordering.lt → cmpConnective s u = Ordering.lt
theorem cmpListSend_lt_trans (l l' l'' : List Send) : cmpListSend l l' = Ordering.lt → cmpListSend l' l'' = Ordering.lt → cmpListSend l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListSend] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListSend] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListSend] at h2
          | cons c cs =>
              simp [cmpListSend] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpSend) (h_eq := fun {a b} => cmpSend_eq_iff a b) (h_lt := fun {a b c} => cmpSend_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListReceive_lt_trans (l l' l'' : List Receive) : cmpListReceive l l' = Ordering.lt → cmpListReceive l' l'' = Ordering.lt → cmpListReceive l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListReceive] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListReceive] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListReceive] at h2
          | cons c cs =>
              simp [cmpListReceive] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpReceive) (h_eq := fun {a b} => cmpReceive_eq_iff a b) (h_lt := fun {a b c} => cmpReceive_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListNew_lt_trans (l l' l'' : List New) : cmpListNew l l' = Ordering.lt → cmpListNew l' l'' = Ordering.lt → cmpListNew l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListNew] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListNew] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListNew] at h2
          | cons c cs =>
              simp [cmpListNew] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpNew) (h_eq := fun {a b} => cmpNew_eq_iff a b) (h_lt := fun {a b c} => cmpNew_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListExpr_lt_trans (l l' l'' : List Expr) : cmpListExpr l l' = Ordering.lt → cmpListExpr l' l'' = Ordering.lt → cmpListExpr l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListExpr] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListExpr] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListExpr] at h2
          | cons c cs =>
              simp [cmpListExpr] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpExpr) (h_eq := fun {a b} => cmpExpr_eq_iff a b) (h_lt := fun {a b c} => cmpExpr_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListMatch_lt_trans (l l' l'' : List Match) : cmpListMatch l l' = Ordering.lt → cmpListMatch l' l'' = Ordering.lt → cmpListMatch l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListMatch] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListMatch] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListMatch] at h2
          | cons c cs =>
              simp [cmpListMatch] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpMatch) (h_eq := fun {a b} => cmpMatch_eq_iff a b) (h_lt := fun {a b c} => cmpMatch_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListGUnforgeable_lt_trans (l l' l'' : List GUnforgeable) : cmpListGUnforgeable l l' = Ordering.lt → cmpListGUnforgeable l' l'' = Ordering.lt → cmpListGUnforgeable l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListGUnforgeable] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListGUnforgeable] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListGUnforgeable] at h2
          | cons c cs =>
              simp [cmpListGUnforgeable] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpGUnforgeable) (h_eq := fun {a b} => cmpGUnforgeable_eq_iff a b) (h_lt := fun {a b c} => cmpGUnforgeable_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListBundle_lt_trans (l l' l'' : List Bundle) : cmpListBundle l l' = Ordering.lt → cmpListBundle l' l'' = Ordering.lt → cmpListBundle l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListBundle] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListBundle] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListBundle] at h2
          | cons c cs =>
              simp [cmpListBundle] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpBundle) (h_eq := fun {a b} => cmpBundle_eq_iff a b) (h_lt := fun {a b c} => cmpBundle_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListConnective_lt_trans (l l' l'' : List Connective) : cmpListConnective l l' = Ordering.lt → cmpListConnective l' l'' = Ordering.lt → cmpListConnective l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListConnective] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListConnective] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListConnective] at h2
          | cons c cs =>
              simp [cmpListConnective] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpConnective) (h_eq := fun {a b} => cmpConnective_eq_iff a b) (h_lt := fun {a b c} => cmpConnective_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListPar_lt_trans (l l' l'' : List Par) : cmpListPar l l' = Ordering.lt → cmpListPar l' l'' = Ordering.lt → cmpListPar l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListPar] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListPar] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListPar] at h2
          | cons c cs =>
              simp [cmpListPar] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpPar) (h_eq := fun {a b} => cmpPar_eq_iff a b) (h_lt := fun {a b c} => cmpPar_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListReceiveBind_lt_trans (l l' l'' : List ReceiveBind) : cmpListReceiveBind l l' = Ordering.lt → cmpListReceiveBind l' l'' = Ordering.lt → cmpListReceiveBind l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListReceiveBind] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListReceiveBind] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListReceiveBind] at h2
          | cons c cs =>
              simp [cmpListReceiveBind] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpReceiveBind) (h_eq := fun {a b} => cmpReceiveBind_eq_iff a b) (h_lt := fun {a b c} => cmpReceiveBind_lt_trans a b c) (hD := ih bs cs) h1 h2
theorem cmpListMatchCase_lt_trans (l l' l'' : List MatchCase) : cmpListMatchCase l l' = Ordering.lt → cmpListMatchCase l' l'' = Ordering.lt → cmpListMatchCase l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListMatchCase] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListMatchCase] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListMatchCase] at h2
          | cons c cs =>
              simp [cmpListMatchCase] at h1 h2 ⊢
              exact lex_lt_trans (f := cmpMatchCase) (h_eq := fun {a b} => cmpMatchCase_eq_iff a b) (h_lt := fun {a b c} => cmpMatchCase_lt_trans a b c) (hD := ih bs cs) h1 h2
private theorem cmpParPair_eq_iff (x y : Par × Par) :
    lex (cmpPar x.1 y.1) (cmpPar x.2 y.2) = Ordering.eq ↔ x = y := by
  rw [lex_eq_iff, cmpPar_eq_iff, cmpPar_eq_iff, Prod.ext_iff]

private theorem cmpParPair_lt_trans (x y z : Par × Par) :
    lex (cmpPar x.1 y.1) (cmpPar x.2 y.2) = Ordering.lt →
    lex (cmpPar y.1 z.1) (cmpPar y.2 z.2) = Ordering.lt →
    lex (cmpPar x.1 z.1) (cmpPar x.2 z.2) = Ordering.lt := by
  intro h1 h2
  exact lex_lt_trans (f := cmpPar) (h_eq := fun {a b} => cmpPar_eq_iff a b) (h_lt := fun {a b c} => cmpPar_lt_trans a b c)
    (hD := cmpPar_lt_trans x.2 y.2 z.2) h1 h2
theorem cmpListParPair_lt_trans (l l' l'' : List (Par × Par)) : cmpListParPair l l' = Ordering.lt → cmpListParPair l' l'' = Ordering.lt → cmpListParPair l l'' = Ordering.lt := by
  induction l generalizing l' l'' with
  | nil => intro h1 h2; cases l' <;> cases l'' <;> simp [cmpListParPair] at h1 h2 ⊢
  | cons a as ih =>
      intro h1 h2
      cases l' with
      | nil => simp [cmpListParPair] at h1
      | cons b bs =>
          cases l'' with
          | nil => simp [cmpListParPair] at h2
          | cons c cs =>
              cases a with | mk a1 a2 =>
              cases b with | mk b1 b2 =>
              cases c with | mk c1 c2 =>
              simp [cmpListParPair] at h1 h2 ⊢
              exact lex_lt_trans (f := fun x y => lex (cmpPar x.1 y.1) (cmpPar x.2 y.2))
                (h_eq := fun {a b} => cmpParPair_eq_iff a b) (h_lt := fun {a b c} => cmpParPair_lt_trans a b c)
                (a := (a1, a2)) (b := (b1, b2)) (c := (c1, c2)) (x := as) (y := bs) (z := cs)
                (hD := ih bs cs) h1 h2

/-! ## The `Comparator` instances for the 11 element types -/

def parComparator : Comparator Par where
  cmp := cmpPar
  eq_iff := by intro a b; exact cmpPar_eq_iff a b
  swap := by intro a b; exact cmpPar_swap a b
  lt_trans := by intro a b c; exact cmpPar_lt_trans a b c

def sendComparator : Comparator Send where
  cmp := cmpSend
  eq_iff := by intro a b; exact cmpSend_eq_iff a b
  swap := by intro a b; exact cmpSend_swap a b
  lt_trans := by intro a b c; exact cmpSend_lt_trans a b c

def receiveBindComparator : Comparator ReceiveBind where
  cmp := cmpReceiveBind
  eq_iff := by intro a b; exact cmpReceiveBind_eq_iff a b
  swap := by intro a b; exact cmpReceiveBind_swap a b
  lt_trans := by intro a b c; exact cmpReceiveBind_lt_trans a b c

def receiveComparator : Comparator Receive where
  cmp := cmpReceive
  eq_iff := by intro a b; exact cmpReceive_eq_iff a b
  swap := by intro a b; exact cmpReceive_swap a b
  lt_trans := by intro a b c; exact cmpReceive_lt_trans a b c

def newComparator : Comparator New where
  cmp := cmpNew
  eq_iff := by intro a b; exact cmpNew_eq_iff a b
  swap := by intro a b; exact cmpNew_swap a b
  lt_trans := by intro a b c; exact cmpNew_lt_trans a b c

def matchCaseComparator : Comparator MatchCase where
  cmp := cmpMatchCase
  eq_iff := by intro a b; exact cmpMatchCase_eq_iff a b
  swap := by intro a b; exact cmpMatchCase_swap a b
  lt_trans := by intro a b c; exact cmpMatchCase_lt_trans a b c

def matchComparator : Comparator Match where
  cmp := cmpMatch
  eq_iff := by intro a b; exact cmpMatch_eq_iff a b
  swap := by intro a b; exact cmpMatch_swap a b
  lt_trans := by intro a b c; exact cmpMatch_lt_trans a b c

def exprComparator : Comparator Expr where
  cmp := cmpExpr
  eq_iff := by intro a b; exact cmpExpr_eq_iff a b
  swap := by intro a b; exact cmpExpr_swap a b
  lt_trans := by intro a b c; exact cmpExpr_lt_trans a b c

def bundleComparator : Comparator Bundle where
  cmp := cmpBundle
  eq_iff := by intro a b; exact cmpBundle_eq_iff a b
  swap := by intro a b; exact cmpBundle_swap a b
  lt_trans := by intro a b c; exact cmpBundle_lt_trans a b c

def gUnforgeableComparator : Comparator GUnforgeable where
  cmp := cmpGUnforgeable
  eq_iff := by intro a b; exact cmpGUnforgeable_eq_iff a b
  swap := by intro a b; exact cmpGUnforgeable_swap a b
  lt_trans := by intro a b c; exact cmpGUnforgeable_lt_trans a b c

def connectiveComparator : Comparator Connective where
  cmp := cmpConnective
  eq_iff := by intro a b; exact cmpConnective_eq_iff a b
  swap := by intro a b; exact cmpConnective_swap a b
  lt_trans := by intro a b c; exact cmpConnective_lt_trans a b c

/-! ## Canonical `sort` -/

/-- Sort a list field after `map f`, when `f` is idempotent on `l`. -/
theorem sortList_field_idem {α : Type} (C : Comparator α) (f : α → α)
    (l : List α) (hf : ∀ x, x ∈ l → f (f x) = f x) :
    sortList C ((sortList C (l.map f)).map f) = sortList C (l.map f) := by
  have hmap : (sortList C (l.map f)).map f = sortList C (l.map f) := by
    unfold sortList
    conv_rhs => rw [← List.map_id (List.insertionSort C.le (l.map f))]
    apply List.map_congr_left
    intro x hx
    rw [List.mem_insertionSort C.le] at hx
    rcases List.mem_map.mp hx with ⟨y, hy, rfl⟩
    exact hf y hy
  rw [hmap]
  exact sortList_idempotent C (l.map f)

mutual
  def sortPar : Par → Par
    | Par.mk s r n e m u b c =>
        Par.mk (sortList sendComparator (sortListSend s))
               (sortList receiveComparator (sortListReceive r))
               (sortList newComparator (sortListNew n))
               (sortList exprComparator (sortListExpr e))
               (sortList matchComparator (sortListMatch m))
               (sortList gUnforgeableComparator (sortListGUnforgeable u))
               (sortList bundleComparator (sortListBundle b))
               (sortList connectiveComparator (sortListConnective c))
  termination_by x => sizeOf x

  def sortSend : Send → Send
    | Send.mk c d p => Send.mk (sortPar c) (sortList parComparator (sortListPar d)) p
  termination_by x => sizeOf x

  def sortReceiveBind : ReceiveBind → ReceiveBind
    | ReceiveBind.mk ps s n => ReceiveBind.mk (sortList parComparator (sortListPar ps)) (sortPar s) n
  termination_by x => sizeOf x

  def sortReceive : Receive → Receive
    | Receive.mk bs b p n => Receive.mk (sortList receiveBindComparator (sortListReceiveBind bs)) (sortPar b) p n
  termination_by x => sizeOf x

  def sortNew : New → New
    | New.mk n b => New.mk n (sortPar b)
  termination_by x => sizeOf x

  def sortMatchCase : MatchCase → MatchCase
    | MatchCase.mk p s n => MatchCase.mk (sortPar p) (sortPar s) n
  termination_by x => sizeOf x

  def sortMatch : Match → Match
    | Match.mk t cs => Match.mk (sortPar t) (sortList matchCaseComparator (sortListMatchCase cs))
  termination_by x => sizeOf x

  def sortExpr : Expr → Expr
    | Expr.ground g => Expr.ground g
    | Expr.evar v => Expr.evar v
    | Expr.eneg p => Expr.eneg (sortPar p)
    | Expr.enot p => Expr.enot (sortPar p)
    | Expr.eplus p q => Expr.eplus (sortPar p) (sortPar q)
    | Expr.eminus p q => Expr.eminus (sortPar p) (sortPar q)
    | Expr.emult p q => Expr.emult (sortPar p) (sortPar q)
    | Expr.ediv p q => Expr.ediv (sortPar p) (sortPar q)
    | Expr.emod p q => Expr.emod (sortPar p) (sortPar q)
    | Expr.elt p q => Expr.elt (sortPar p) (sortPar q)
    | Expr.ele p q => Expr.ele (sortPar p) (sortPar q)
    | Expr.egt p q => Expr.egt (sortPar p) (sortPar q)
    | Expr.ege p q => Expr.ege (sortPar p) (sortPar q)
    | Expr.eeq p q => Expr.eeq (sortPar p) (sortPar q)
    | Expr.eneq p q => Expr.eneq (sortPar p) (sortPar q)
    | Expr.eand p q => Expr.eand (sortPar p) (sortPar q)
    | Expr.eor p q => Expr.eor (sortPar p) (sortPar q)
    | Expr.elist ps => Expr.elist (sortList parComparator (sortListPar ps))
    | Expr.etuple ps => Expr.etuple (sortList parComparator (sortListPar ps))
    | Expr.eset ps => Expr.eset (sortList parComparator (sortListPar ps))
    | Expr.emap kvs => Expr.emap (sortList (cmpPair parComparator parComparator) (sortListParPair kvs))
  termination_by x => sizeOf x

  def sortBundle : Bundle → Bundle
    | Bundle.mk b w r => Bundle.mk (sortPar b) w r
  termination_by x => sizeOf x

  def sortGUnforgeable : GUnforgeable → GUnforgeable
    | g => g
  termination_by x => sizeOf x

  def sortConnective : Connective → Connective
    | Connective.connAnd ps => Connective.connAnd (sortList parComparator (sortListPar ps))
    | Connective.connOr ps => Connective.connOr (sortList parComparator (sortListPar ps))
    | Connective.connNot p => Connective.connNot (sortPar p)
    | Connective.connVarRef d n => Connective.connVarRef d n
  termination_by x => sizeOf x

  def sortParPair : Par × Par → Par × Par
    | (a, b) => (sortPar a, sortPar b)
  termination_by x => sizeOf x

  def sortListSend : List Send → List Send
    | [] => []
    | a :: as => sortSend a :: sortListSend as
  termination_by x => sizeOf x

  def sortListReceive : List Receive → List Receive
    | [] => []
    | a :: as => sortReceive a :: sortListReceive as
  termination_by x => sizeOf x

  def sortListNew : List New → List New
    | [] => []
    | a :: as => sortNew a :: sortListNew as
  termination_by x => sizeOf x

  def sortListExpr : List Expr → List Expr
    | [] => []
    | a :: as => sortExpr a :: sortListExpr as
  termination_by x => sizeOf x

  def sortListMatch : List Match → List Match
    | [] => []
    | a :: as => sortMatch a :: sortListMatch as
  termination_by x => sizeOf x

  def sortListGUnforgeable : List GUnforgeable → List GUnforgeable
    | [] => []
    | a :: as => sortGUnforgeable a :: sortListGUnforgeable as
  termination_by x => sizeOf x

  def sortListBundle : List Bundle → List Bundle
    | [] => []
    | a :: as => sortBundle a :: sortListBundle as
  termination_by x => sizeOf x

  def sortListConnective : List Connective → List Connective
    | [] => []
    | a :: as => sortConnective a :: sortListConnective as
  termination_by x => sizeOf x

  def sortListPar : List Par → List Par
    | [] => []
    | a :: as => sortPar a :: sortListPar as
  termination_by x => sizeOf x

  def sortListReceiveBind : List ReceiveBind → List ReceiveBind
    | [] => []
    | a :: as => sortReceiveBind a :: sortListReceiveBind as
  termination_by x => sizeOf x

  def sortListMatchCase : List MatchCase → List MatchCase
    | [] => []
    | a :: as => sortMatchCase a :: sortListMatchCase as
  termination_by x => sizeOf x

  def sortListParPair : List (Par × Par) → List (Par × Par)
    | [] => []
    | x :: xs => sortParPair x :: sortListParPair xs
  termination_by x => sizeOf x
end

/-! ## `sortListX` = `·.map sortX` -/

@[simp] theorem sortListSend_eq_map (l : List Send) : sortListSend l = l.map sortSend := by
  induction l <;> simp [sortListSend, *]
@[simp] theorem sortListReceive_eq_map (l : List Receive) : sortListReceive l = l.map sortReceive := by
  induction l <;> simp [sortListReceive, *]
@[simp] theorem sortListNew_eq_map (l : List New) : sortListNew l = l.map sortNew := by
  induction l <;> simp [sortListNew, *]
@[simp] theorem sortListExpr_eq_map (l : List Expr) : sortListExpr l = l.map sortExpr := by
  induction l <;> simp [sortListExpr, *]
@[simp] theorem sortListMatch_eq_map (l : List Match) : sortListMatch l = l.map sortMatch := by
  induction l <;> simp [sortListMatch, *]
@[simp] theorem sortListGUnforgeable_eq_map (l : List GUnforgeable) : sortListGUnforgeable l = l.map sortGUnforgeable := by
  induction l <;> simp [sortListGUnforgeable, *]
@[simp] theorem sortListBundle_eq_map (l : List Bundle) : sortListBundle l = l.map sortBundle := by
  induction l <;> simp [sortListBundle, *]
@[simp] theorem sortListConnective_eq_map (l : List Connective) : sortListConnective l = l.map sortConnective := by
  induction l <;> simp [sortListConnective, *]
@[simp] theorem sortListPar_eq_map (l : List Par) : sortListPar l = l.map sortPar := by
  induction l <;> simp [sortListPar, *]
@[simp] theorem sortListReceiveBind_eq_map (l : List ReceiveBind) : sortListReceiveBind l = l.map sortReceiveBind := by
  induction l <;> simp [sortListReceiveBind, *]
@[simp] theorem sortListMatchCase_eq_map (l : List MatchCase) : sortListMatchCase l = l.map sortMatchCase := by
  induction l <;> simp [sortListMatchCase, *]
@[simp] theorem sortListParPair_eq_map (l : List (Par × Par)) : sortListParPair l = l.map sortParPair := by
  induction l <;> simp [sortListParPair, *]

/-! ## Law 1 (canonicalization) — idempotence -/

mutual
  theorem sortPar_idempotent : ∀ (p : Par), sortPar (sortPar p) = sortPar p
    | Par.mk s r n e m u b c => by
        simp [sortPar]
        exact ⟨sortList_field_idem sendComparator sortSend s (fun x _ => sortSend_idempotent x),
          sortList_field_idem receiveComparator sortReceive r (fun x _ => sortReceive_idempotent x),
          sortList_field_idem newComparator sortNew n (fun x _ => sortNew_idempotent x),
          sortList_field_idem exprComparator sortExpr e (fun x _ => sortExpr_idempotent x),
          sortList_field_idem matchComparator sortMatch m (fun x _ => sortMatch_idempotent x),
          sortList_field_idem gUnforgeableComparator sortGUnforgeable u (fun x _ => sortGUnforgeable_idempotent x),
          sortList_field_idem bundleComparator sortBundle b (fun x _ => sortBundle_idempotent x),
          sortList_field_idem connectiveComparator sortConnective c (fun x _ => sortConnective_idempotent x)⟩
  termination_by p => sizeOf p

  theorem sortSend_idempotent : ∀ (x : Send), sortSend (sortSend x) = sortSend x
    | Send.mk c d p => by
        have hc := sortPar_idempotent c
        have hd : ∀ x, x ∈ d → sortPar (sortPar x) = sortPar x := fun x _ => sortPar_idempotent x
        simp [sortSend, hc]
        exact sortList_field_idem parComparator sortPar d hd
  termination_by x => sizeOf x

  theorem sortReceiveBind_idempotent : ∀ (x : ReceiveBind), sortReceiveBind (sortReceiveBind x) = sortReceiveBind x
    | ReceiveBind.mk ps s n => by
        have hps : ∀ x, x ∈ ps → sortPar (sortPar x) = sortPar x := fun x _ => sortPar_idempotent x
        have hs := sortPar_idempotent s
        simp [sortReceiveBind, hs]
        exact sortList_field_idem parComparator sortPar ps hps
  termination_by x => sizeOf x

  theorem sortReceive_idempotent : ∀ (x : Receive), sortReceive (sortReceive x) = sortReceive x
    | Receive.mk bs b p n => by
        have hbs : ∀ x, x ∈ bs → sortReceiveBind (sortReceiveBind x) = sortReceiveBind x := fun x _ => sortReceiveBind_idempotent x
        have hb := sortPar_idempotent b
        simp [sortReceive, hb]
        exact sortList_field_idem receiveBindComparator sortReceiveBind bs hbs
  termination_by x => sizeOf x

  theorem sortNew_idempotent : ∀ (x : New), sortNew (sortNew x) = sortNew x
    | New.mk n b => by
        have hb := sortPar_idempotent b
        simp [sortNew, hb]
  termination_by x => sizeOf x

  theorem sortMatchCase_idempotent : ∀ (x : MatchCase), sortMatchCase (sortMatchCase x) = sortMatchCase x
    | MatchCase.mk p s n => by
        have hp := sortPar_idempotent p
        have hs := sortPar_idempotent s
        simp [sortMatchCase, hp, hs]
  termination_by x => sizeOf x

  theorem sortMatch_idempotent : ∀ (x : Match), sortMatch (sortMatch x) = sortMatch x
    | Match.mk t cs => by
        have ht := sortPar_idempotent t
        have hcs : ∀ x, x ∈ cs → sortMatchCase (sortMatchCase x) = sortMatchCase x := fun x _ => sortMatchCase_idempotent x
        simp [sortMatch, ht]
        exact sortList_field_idem matchCaseComparator sortMatchCase cs hcs
  termination_by x => sizeOf x

  theorem sortExpr_idempotent : ∀ (x : Expr), sortExpr (sortExpr x) = sortExpr x
    | Expr.ground g => by simp [sortExpr]
    | Expr.evar v => by simp [sortExpr]
    | Expr.eneg p | Expr.enot p => by
        have hp := sortPar_idempotent p
        simp [sortExpr, hp]
    | Expr.eplus p q | Expr.eminus p q | Expr.emult p q | Expr.ediv p q | Expr.emod p q |
      Expr.elt p q | Expr.ele p q | Expr.egt p q | Expr.ege p q | Expr.eeq p q |
      Expr.eneq p q | Expr.eand p q | Expr.eor p q => by
        have hp := sortPar_idempotent p
        have hq := sortPar_idempotent q
        simp [sortExpr, hp, hq]
    | Expr.elist ps | Expr.etuple ps | Expr.eset ps => by
        have hps : ∀ x, x ∈ ps → sortPar (sortPar x) = sortPar x := fun x _ => sortPar_idempotent x
        simp [sortExpr]
        exact sortList_field_idem parComparator sortPar ps hps
    | Expr.emap kvs => by
        have hkvs : ∀ x, x ∈ kvs → sortParPair (sortParPair x) = sortParPair x := fun x _ => sortParPair_idempotent x
        simp [sortExpr]
        exact sortList_field_idem (cmpPair parComparator parComparator) sortParPair kvs hkvs
  termination_by x => sizeOf x

  theorem sortBundle_idempotent : ∀ (x : Bundle), sortBundle (sortBundle x) = sortBundle x
    | Bundle.mk b w r => by
        have hb := sortPar_idempotent b
        simp [sortBundle, hb]
  termination_by x => sizeOf x

  theorem sortGUnforgeable_idempotent : ∀ (x : GUnforgeable), sortGUnforgeable (sortGUnforgeable x) = sortGUnforgeable x
    | g => rfl
  termination_by x => sizeOf x

  theorem sortConnective_idempotent : ∀ (x : Connective), sortConnective (sortConnective x) = sortConnective x
    | Connective.connAnd ps | Connective.connOr ps => by
        have hps : ∀ x, x ∈ ps → sortPar (sortPar x) = sortPar x := fun x _ => sortPar_idempotent x
        simp [sortConnective]
        exact sortList_field_idem parComparator sortPar ps hps
    | Connective.connNot p => by
        have hp := sortPar_idempotent p
        simp [sortConnective, hp]
    | Connective.connVarRef d n => by simp [sortConnective]
  termination_by x => sizeOf x

  theorem sortParPair_idempotent : ∀ (x : Par × Par), sortParPair (sortParPair x) = sortParPair x
    | (a, b) => by
        have ha := sortPar_idempotent a
        have hb := sortPar_idempotent b
        simp [sortParPair, ha, hb]
  termination_by x => sizeOf x
end

/-! ## Law 1 (canonicalization) — commutativity -/

theorem sortPar_comm (p q : Par) : sortPar (parMerge p q) = sortPar (parMerge q p) := by
  simp [parMerge, sortPar, sortList_append_comm]


end Rchain
