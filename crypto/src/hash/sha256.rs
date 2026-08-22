//! Sha256 hashing.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/hash/Sha256.scala`.

use sha2::{Digest, Sha256};

/// Hash a single byte slice with SHA-256.
pub fn hash(input: &[u8]) -> Vec<u8> {
    Sha256::digest(input).to_vec()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::base16;

    #[test]
    fn encodes() {
        assert_eq!(
            base16::encode(&hash(b"")),
            "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
        );
        assert_eq!(
            base16::encode(&hash(b"abc")),
            "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"
        );
        assert_eq!(
            base16::encode(&hash(b"hello world")),
            "b94d27b9934d3e08a52e52d7da7dabfac484efe37a5380ee9088f7ace2efcde9"
        );
        assert_eq!(
            base16::encode(&hash(b"abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq")),
            "248d6a61d20638b8e5c026930c3e6039a33ce45964ff2167f6ecedd419db06c1"
        );
    }
}
