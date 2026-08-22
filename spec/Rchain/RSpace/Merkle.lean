/-!
# Law 10 — Merkle determinism

The history is a content-addressed radix trie: a node's hash determines its contents (collision-free),
and the empty state has a fixed root. The Scala oracle is `history/RadixTree.scala:50-68`; the Rust
realization is `rspace::history::RadixTreeImpl` (`Node = [Item; 256]`).
-/

namespace Rchain

/-- A trie node hash (Blake2b256). -/
structure NodeHash where
  hash : Nat

/-- The radix-trie root of a history. -/
axiom trieRoot : NodeHash

/-- Law 10: content addressing — equal hashes give equal contents (collision-free). -/
axiom trie_collision_free (a b : NodeHash) : a.hash = b.hash → a = b

/-- The empty history has a canonical (fixed) empty root. -/
axiom emptyRoot : NodeHash

/-- Law 10: the empty root is a fixed point of every operation that leaves the trie empty. -/
axiom trie_empty_root (h : NodeHash) : h.hash = emptyRoot.hash ↔ h = emptyRoot

end Rchain
