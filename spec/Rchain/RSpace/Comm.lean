/-!
# Laws 8 & 11 — deterministic COMM, replay determinism

A COMM event is content-addressed by the sorted produce refs (Law 8); replay recomputes COMM events
from the recorded trace and must be a subset of it (Law 11). The Scala oracle is
`trace/Event.scala:35-39` + `ReplayRSpace.scala:68-71`; the Rust realization is `rspace::rspace`
(sorted produce) + `rspace::ReplayRSpace`.
-/

namespace Rchain

/-- A COMM event (produce matched against a consume), identified by its content. -/
structure Comm where
  id : Nat

/-- The produce refs of a COMM, in canonical (sorted) order. -/
axiom produceRefs : Comm → List Nat

/-- Law 8: a COMM is content-addressed — equal refs give equal events (deterministic). -/
axiom comm_content_addressed (a b : Comm) :
  produceRefs a = produceRefs b → a.id = b.id

/-- A recorded trace of COMM events. -/
structure Trace where
  events : List Comm

/-- Replay recomputes COMM events from the recorded trace. -/
axiom replayEvents : Trace → List Comm

/-- Law 11: recomputed COMM ⊆ recorded trace (replay never invents a COMM). -/
axiom replay_comm_subset (t : Trace) :
  ∀ e ∈ replayEvents t, e ∈ t.events

end Rchain
