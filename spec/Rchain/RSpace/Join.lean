/-!
# Law 7 — join commutativity

A join (multi-channel consume) is keyed by the hash of its channels taken **in sorted order**, so the
key is invariant under channel permutation. The Scala oracle is `StableHashProvider.scala:18-22`; the
Rust realization is `rspace::hashing::StableHashProvider::hash_seq` (sorted).
-/

namespace Rchain

/-- A channel, identified by its Blake2b256 key. -/
structure Channel where
  key : Nat

/-- The join key: the hash of a sorted list of channel keys. -/
axiom joinKey : List Channel → Nat

/-- Law 7: the join key is invariant under permutation (channels hashed in sorted order). -/
axiom joinKey_perm (cs ds : List Channel) : List.Perm cs ds → joinKey cs = joinKey ds

end Rchain
