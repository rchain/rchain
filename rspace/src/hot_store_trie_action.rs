//! Hash-addressed trie actions (consumed by the history repository at checkpoint).
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/HotStoreTrieAction.scala`.

use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;

use crate::internal::{Datum, WaitingContinuation};

/// A hash-addressed trie mutation (port of `HotStoreTrieAction`).
#[derive(Clone, Debug, PartialEq)]
pub enum HotStoreTrieAction<C, P, A, K> {
    TrieInsertProduce(Blake2b256Hash, Vec<Datum<A>>),
    TrieInsertJoins(Blake2b256Hash, Vec<Vec<C>>),
    TrieInsertConsume(Blake2b256Hash, Vec<WaitingContinuation<P, K>>),
    TrieInsertBinaryProduce(Blake2b256Hash, Vec<Vec<u8>>),
    TrieInsertBinaryJoins(Blake2b256Hash, Vec<Vec<u8>>),
    TrieInsertBinaryConsume(Blake2b256Hash, Vec<Vec<u8>>),
    TrieDeleteProduce(Blake2b256Hash),
    TrieDeleteJoins(Blake2b256Hash),
    TrieDeleteConsume(Blake2b256Hash),
}
