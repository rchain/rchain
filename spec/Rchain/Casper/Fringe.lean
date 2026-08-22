import Rchain.Casper.Stake

/-!
# Laws 14 & 15 — the fringe

The finalized fringe is an antichain of one message per bonded validator (Law 14); it is monotone by
height, and each message's seen-set is monotone (no regression) (Law 15). The Scala oracle is
`block-storage/dag/Finalizer.scala:76,133` + `MessageMapSyntax.scala:33` + `casper/Validate.scala:285`;
the Rust realization is `block-storage::dag::Message` (`BlockHeight`/`SeqNum`).
-/

namespace Rchain

/-- A DAG message: id, block height, sender, sequence number, justification (parent) ids, and the
    transitive seen set. -/
structure Message where
  id : Nat
  height : Nat
  sender : Nat
  seqNum : Nat
  parents : List Nat
  seen : List Nat

/-- A fringe: a set of messages (one per bonded validator). -/
structure Fringe where
  messages : List Message

/-- Law 14: a fringe is an antichain — at most one message per bonded validator. -/
axiom fringe_antichain (f : Fringe) :
  ∀ m ∈ f.messages, ∀ n ∈ f.messages, m.sender = n.sender → m.id = n.id

/-- Law 15: the fringe is monotone by height — finalized messages never decrease in height. -/
axiom fringe_monotone (f g : Fringe) :
  (∀ m ∈ f.messages, ∀ n ∈ g.messages, m.height ≤ n.height) ∨
  (∀ n ∈ g.messages, ∀ m ∈ f.messages, n.height ≤ m.height)

/-- Law 15: the seen-set is monotone — if `b` sees `a`, then `b` sees everything `a` sees
    (transitive closure, no regression). -/
axiom seen_monotone (a b : Message) : a.id ∈ b.seen → ∀ x, x ∈ a.seen → x ∈ b.seen

end Rchain
