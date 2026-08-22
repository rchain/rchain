/-!
# Law 9 — merge is a monoid; non-conflicting logs commute

State-channel changes merge associatively, and merging two non-conflicting change logs is
commutative. The Scala oracle is `merger/{StateChange,ChannelChange,EventLogMergingLogic}.scala`; the
Rust realization is the module `rspace/src/merger/state_change_merger.rs` (`compute_trie_actions`).
-/

namespace Rchain

/-- A state change (a map of channel → change). -/
structure StateChange where
  id : Nat

/-- Merge two state changes. -/
axiom mergeChanges : StateChange → StateChange → StateChange

/-- Law 9a: merge is associative. -/
axiom mergeChanges_assoc (a b c : StateChange) :
  mergeChanges (mergeChanges a b) c = mergeChanges a (mergeChanges b c)

/-- Two changes are non-conflicting when they touch disjoint channels. -/
axiom NonConflicting : StateChange → StateChange → Prop

/-- Law 9b: merging non-conflicting changes commutes. -/
axiom mergeChanges_comm (a b : StateChange) :
  NonConflicting a b → mergeChanges a b = mergeChanges b a

end Rchain
