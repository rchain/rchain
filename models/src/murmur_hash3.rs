//! Scala's `MurmurHash3.productHash` (port of the pure half of `models/HashM.scala`).
//!
//! The Magnolia-derived `HashM[A]` typeclass is deferred; only the product-hash algorithm it uses
//! (Scala's `scala.util.hashing.MurmurHash3` product hash, plus Java `String.hashCode`) is ported.

/// The Scala `MurmurHash3.productSeed`.
const PRODUCT_SEED: i32 = 0xcafebabeu32 as i32;

/// Java `String.hashCode` (used by `product_hash` for the empty-elements case).
pub fn string_hash_code(s: &str) -> i32 {
    s.chars()
        .fold(0i32, |h, c| h.wrapping_mul(31).wrapping_add(c as i32))
}

/// Java `Long.hashCode` (used by `HashM.LongHash` via `.##`).
pub fn long_hash_code(value: i64) -> i32 {
    (value ^ (value as u64 >> 32) as i64) as i32
}

/// Java `Boolean.hashCode` (used by `HashM.BooleanHash` via `.##`).
pub fn boolean_hash_code(value: bool) -> i32 {
    if value {
        1231
    } else {
        1237
    }
}

/// Scala's `MurmurHash3.productHash` for case classes (port of `HashMDerivation.productHash`).
///
/// `elements` are the already-computed field hashes; `prefix` is the case-class name, used only
/// when there are no fields (case objects).
pub fn product_hash(prefix: &str, elements: &[i32]) -> i32 {
    if elements.is_empty() {
        string_hash_code(prefix)
    } else {
        let mut h = PRODUCT_SEED;
        for &e in elements {
            h = mix(h, e);
        }
        finalize_hash(h, elements.len() as i32)
    }
}

fn mix(hash: i32, data: i32) -> i32 {
    let h = mix_last(hash, data);
    let h = h.rotate_left(13);
    h.wrapping_mul(5).wrapping_add(0xe6546b64u32 as i32)
}

fn mix_last(hash: i32, data: i32) -> i32 {
    let k = data.wrapping_mul(0xcc9e2d51u32 as i32);
    let k = k.rotate_left(15);
    let k = k.wrapping_mul(0x1b873593);
    hash ^ k
}

fn finalize_hash(hash: i32, length: i32) -> i32 {
    avalanche(hash ^ length)
}

fn avalanche(hash: i32) -> i32 {
    let mut h = hash;
    h ^= (h as u32 >> 16) as i32;
    h = h.wrapping_mul(0x85ebca6bu32 as i32);
    h ^= (h as u32 >> 13) as i32;
    h = h.wrapping_mul(0xc2b2ae35u32 as i32);
    h ^= (h as u32 >> 16) as i32;
    h
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_hash_code_matches_java() {
        assert_eq!(string_hash_code(""), 0);
        assert_eq!(string_hash_code("a"), 97);
        assert_eq!(string_hash_code("ab"), 3105);
        assert_eq!(string_hash_code("abc"), 96354);
    }

    #[test]
    fn product_hash_empty_uses_prefix_hash_code() {
        assert_eq!(product_hash("abc", &[]), string_hash_code("abc"));
        assert_eq!(product_hash("abc", &[]), 96354);
    }

    #[test]
    fn product_hash_is_deterministic() {
        assert_eq!(product_hash("Par", &[1, 2, 3]), product_hash("Par", &[1, 2, 3]));
        assert_ne!(product_hash("Par", &[1, 2, 3]), product_hash("Par", &[1, 2, 4]));
    }

    #[test]
    fn long_hash_code_matches_java() {
        assert_eq!(long_hash_code(42), 42);
        assert_eq!(long_hash_code(0), 0);
        assert_eq!(long_hash_code(-1), 0);
        assert_eq!(long_hash_code(1i64 << 32), 1);
    }

    #[test]
    fn boolean_hash_code_matches_java() {
        assert_eq!(boolean_hash_code(true), 1231);
        assert_eq!(boolean_hash_code(false), 1237);
    }
}
