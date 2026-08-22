/-!
# Law 19 — `Blake2b512Random` (associative splittable merge)

`Blake2b512Random` is a splittable PRNG whose merge is associative and commutative. The Scala oracle
is `crypto/.../Blake2b512Random`; the Rust realization is `crypto::hash::blake2b512_random`
(`split_byte`/`merge`). The primitive is **axiomatized by design** (a cryptographic hash), not proven.
-/

namespace Rchain

/-- A split random generator state. -/
structure Random where
  state : Nat

/-- Merge two split random states. -/
axiom mergeRandom : Random → Random → Random

/-- Law 19: merge is associative. -/
axiom mergeRandom_assoc (a b c : Random) :
  mergeRandom (mergeRandom a b) c = mergeRandom a (mergeRandom b c)

/-- Law 19: merge is commutative. -/
axiom mergeRandom_comm (a b : Random) : mergeRandom a b = mergeRandom b a

end Rchain
