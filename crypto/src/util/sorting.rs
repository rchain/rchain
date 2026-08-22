//! Orderings.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/util/Sorting.scala`.

use std::cmp::Ordering;

/// Compare two bytes as **signed** bytes (Scala `Byte` is signed; `Ordering.by(Array[Byte].toIterable)`
/// orders `0x80..0xFF` *before* `0x00..0x7F`).
pub fn cmp_signed_byte(a: u8, b: u8) -> Ordering {
    (a as i8).cmp(&(b as i8))
}

/// Lexicographic ordering over signed bytes (mirrors Scala `Ordering.by((_: Array[Byte]).toIterable)`).
pub fn compare_byte_arrays(a: &[u8], b: &[u8]) -> Ordering {
    for (x, y) in a.iter().zip(b.iter()) {
        let ord = cmp_signed_byte(*x, *y);
        if ord != Ordering::Equal {
            return ord;
        }
    }
    a.len().cmp(&b.len())
}
