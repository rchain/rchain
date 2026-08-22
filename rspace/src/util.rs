//! RSpace byte-vector ordering helpers.
//!
//! Mirrors `rspace/src/main/scala/coop/rchain/rspace/util/package.scala`.

use std::cmp::Ordering;
use std::fmt;

/// A replay verification failure (port of `util/ReplayException.scala`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReplayException(pub String);

impl fmt::Display for ReplayException {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ReplayException: {}", self.0)
    }
}

impl std::error::Error for ReplayException {}

/// Compare byte vectors length-first, then unsigned bytewise (port of `veccmp`).
pub fn veccmp(a: &[u8], b: &[u8]) -> Ordering {
    a.len().cmp(&b.len()).then_with(|| a.cmp(b))
}

/// The canonical byte-vector ordering (port of `ordByteVector`).
pub fn ord_byte_vector(a: &[u8], b: &[u8]) -> Ordering {
    veccmp(a, b)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn shorter_comes_first() {
        assert_eq!(veccmp(&[0xff], &[0x00, 0x00]), Ordering::Less);
    }

    #[test]
    fn equal_length_compares_unsigned() {
        // unsigned: 0x80 > 0x7f
        assert_eq!(veccmp(&[0x80], &[0x7f]), Ordering::Greater);
        assert_eq!(veccmp(&[0x01, 0x00], &[0x00, 0xff]), Ordering::Greater);
    }
}
