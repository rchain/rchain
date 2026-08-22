//! String/byte hex-conversion syntax (port of `StringSyntax.scala`, `ByteArraySyntax.scala`, and
//! the portable half of `ByteStringSyntax.scala`).
//!
//! `ByteStringSyntax.toDirectByteBuffer` (Java NIO), `toByteVector` (scodec), and
//! `toBlake2b256Hash` (rspace) are deferred.

use rchain_shared::base16;

/// Extensions on `str` (port of `StringSyntax`).
pub trait StringSyntax {
    /// Decode hex, or `None` on non-hex input (port of `decodeHex`).
    fn decode_hex(&self) -> Option<Vec<u8>>;

    /// Decode hex, ignoring non-hex characters (port of `unsafeDecodeHex`).
    fn unsafe_decode_hex(&self) -> Vec<u8>;

    /// Decode hex to bytes, or `None` (port of `hexToByteString`).
    fn hex_to_byte_string(&self) -> Option<Vec<u8>>;

    /// Decode hex to bytes, ignoring non-hex (port of `unsafeHexToByteString`).
    fn unsafe_hex_to_byte_string(&self) -> Vec<u8>;

    /// Whether the string is pure ASCII (port of `onlyAscii`).
    fn only_ascii(&self) -> bool;
}

impl StringSyntax for str {
    fn decode_hex(&self) -> Option<Vec<u8>> {
        base16::decode(self)
    }

    fn unsafe_decode_hex(&self) -> Vec<u8> {
        base16::unsafe_decode(self)
    }

    fn hex_to_byte_string(&self) -> Option<Vec<u8>> {
        base16::decode(self)
    }

    fn unsafe_hex_to_byte_string(&self) -> Vec<u8> {
        base16::unsafe_decode(self)
    }

    fn only_ascii(&self) -> bool {
        self.is_ascii()
    }
}

/// Extensions on byte slices (port of `ByteArraySyntax` and the portable `ByteStringSyntax`).
pub trait ByteArraySyntax {
    /// Copy to an owned byte vector (port of `toByteString`).
    fn to_byte_string(&self) -> Vec<u8>;

    /// Encode as lowercase hex (port of `toHexString`).
    fn to_hex_string(&self) -> String;
}

impl ByteArraySyntax for [u8] {
    fn to_byte_string(&self) -> Vec<u8> {
        self.to_vec()
    }

    fn to_hex_string(&self) -> String {
        base16::encode(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_hex_decode() {
        assert_eq!("0f".decode_hex(), Some(vec![0x0f]));
        assert_eq!("zz".decode_hex(), None);
        assert_eq!("f".decode_hex(), None);
        assert_eq!("z1z2z".unsafe_decode_hex(), vec![0x12]);
    }

    #[test]
    fn string_hex_to_byte_string() {
        assert_eq!("0f".hex_to_byte_string(), Some(vec![0x0f]));
        assert_eq!("zz".hex_to_byte_string(), None);
        assert_eq!("z1z2z".unsafe_hex_to_byte_string(), vec![0x12]);
    }

    #[test]
    fn only_ascii_detects_non_ascii() {
        assert!("abc".only_ascii());
        assert!(!"héllo".only_ascii());
    }

    #[test]
    fn byte_array_hex_round_trip() {
        let bytes = vec![0x12, 0x34, 0xde, 0xf0];
        assert_eq!(bytes.to_hex_string(), "1234def0");
        assert_eq!(bytes.to_byte_string(), bytes);
    }
}
