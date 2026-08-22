/-!
# Law 19 — Blake2b256, signatures, Curve25519

`Blake2b256` is a canonical (deterministic, collision-free) hash; signatures verify (verify∘sign is
identity); Curve25519 key agreement round-trips. The Scala oracle is `crypto/...` (`Blake2b256`,
`Secp256k1`, `Ed25519`, `Curve25519`); the Rust realization is `crypto::hash::blake2b256_hash` +
`crypto::signatures`. The primitives are **axiomatized by design**, not proven.
-/

namespace Rchain

/-- A message (byte string), abstracted as a natural. -/
structure Msg where
  id : Nat

/-- A Blake2b256 hash (32 bytes). -/
structure Hash where
  bytes : Nat

/-- A signature. -/
structure Signature where
  bytes : Nat

/-- A public/private keypair. -/
structure KeyPair where
  public : Nat
  secret : Nat

/-- Blake2b256 hashing. -/
axiom blake2b256 : Msg → Hash

/-- Law 19: the hash is canonical (deterministic) and collision-free. -/
axiom blake2b256_collision_free (a b : Msg) : blake2b256 a = blake2b256 b → a = b

/-- Sign and verify. -/
axiom sign : KeyPair → Msg → Signature
axiom verify : Nat → Msg → Signature → Prop

/-- Law 19: `verify pk m (sign sk m)` holds when `pk` is `sk`'s public half (sign/verify round-trip). -/
axiom sign_verify_roundtrip (kp : KeyPair) (m : Msg) : verify kp.public m (sign kp m)

/-- Curve25519 shared-secret derivation. -/
axiom sharedSecret : Nat → Nat → Nat

/-- Law 19: Curve25519 key agreement round-trips (both sides derive the same shared secret). -/
axiom curve25519_roundtrip (a_priv b_pub b_priv a_pub : Nat) :
  sharedSecret a_priv b_pub = sharedSecret b_priv a_pub

end Rchain
