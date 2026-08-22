//! Keccak256 hashing.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/hash/Keccak256.scala`. Uses the original
//! Keccak (not the standardized SHA-3 variant) to match BouncyCastle's `KeccakDigest`.

use sha3::{Digest, Keccak256};

/// Hash a single byte slice with Keccak-256.
pub fn hash(input: &[u8]) -> Vec<u8> {
    Keccak256::digest(input).to_vec()
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
            "c5d2460186f7233c927e7db2dcc703c0e500b653ca82273b7bfad8045d85a470"
        );
    }

    #[test]
    fn encodes_data() {
        let result = base16::encode(&hash(b"abc"));
        assert_eq!(
            result,
            "4e03657aea45a94fc7d47ba826c8d667c0d1e6e33a64a036ec44f58fa12d6c45"
        );
    }
}
