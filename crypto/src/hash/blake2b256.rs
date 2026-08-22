//! Blake2b256 hashing.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/hash/Blake2b256.scala`.

use blake2::digest::consts::U32;
use blake2::{Blake2b, Digest};

type Blake2b256 = Blake2b<U32>;

/// The length of a Blake2b256 hash in bytes.
pub const HASH_LENGTH: usize = 32;

/// Hash a single byte slice.
pub fn hash(input: &[u8]) -> Vec<u8> {
    Blake2b256::digest(input).to_vec()
}

/// Hash the concatenation of several byte slices (the Scala `hash(ByteVector*)` overload).
pub fn hash_many(inputs: &[&[u8]]) -> Vec<u8> {
    let mut digest = Blake2b256::new();
    for input in inputs {
        digest.update(input);
    }
    digest.finalize().to_vec()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::base16;

    #[test]
    fn encodes_empty() {
        let result = base16::encode(&hash(b""));
        assert_eq!(
            result,
            "0e5751c026e543b2e8ab2eb06099daa1d1e5df47778f7787faab45cdf12fe3a8"
        );
    }

    #[test]
    fn encodes_data() {
        let result = base16::encode(&hash(b"abc"));
        assert_eq!(
            result,
            "bddd813c634239723171ef3fee98579b94964e3bb1cb3e427262c8c068d52319"
        );
    }

    #[test]
    fn hash_many_matches_concatenation() {
        assert_eq!(hash_many(&[b"ab", b"c"]), hash(b"abc"));
        assert_eq!(hash_many(&[]), hash(b""));
    }
}
