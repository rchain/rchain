//! Hex encoding/decoding.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/shared/Base16.scala`.

/// Encode bytes as lowercase hex (two characters per byte).
pub fn encode(input: &[u8]) -> String {
    input.iter().map(|b| format!("{:02x}", b)).collect()
}

/// Decode a hex string, failing on any non-hex character or odd-length input.
///
/// Odd-length input is rejected rather than left-padded, so a truncated hash can never silently
/// decode to a different (valid-looking) value.
pub fn decode(input: &str) -> Option<Vec<u8>> {
    if input.chars().any(|c| !c.is_ascii_hexdigit()) {
        return None;
    }
    if input.len() % 2 != 0 {
        return None;
    }
    parse_hex_padded(input)
}

/// Decode a hex string, ignoring any non-hex characters. Always succeeds.
///
/// **Lax**: non-hex characters are silently dropped, so malformed input is silently corrupted
/// rather than rejected. Use [`try_decode`] at untrusted boundaries.
pub fn unsafe_decode(input: &str) -> Vec<u8> {
    let digits: String = input.chars().filter(|c| c.is_ascii_hexdigit()).collect();
    // `digits` contains only ASCII hex digits, so `parse_hex_padded` cannot return `None`.
    parse_hex_padded(&digits).unwrap_or_default()
}

/// Decode a hex string, failing on non-hex input (the validated counterpart of [`unsafe_decode`]).
pub fn try_decode(input: &str) -> Result<Vec<u8>, String> {
    decode(input).ok_or_else(|| format!("invalid hex input: {input}"))
}

fn parse_hex_padded(digits: &str) -> Option<Vec<u8>> {
    let padded: String = if digits.len() % 2 == 0 {
        digits.to_string()
    } else {
        format!("0{digits}")
    };
    let bytes = padded.as_bytes();
    let mut out = Vec::with_capacity(bytes.len() / 2);
    for chunk in bytes.chunks(2) {
        let hi = (chunk[0] as char).to_digit(16)? as u8;
        let lo = (chunk[1] as char).to_digit(16)? as u8;
        out.push((hi << 4) | lo);
    }
    Some(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips_every_byte_value() {
        let input: Vec<u8> = (0u8..=255).collect();
        let encoded = encode(&input);
        assert_eq!(encoded.len(), input.len() * 2);
        assert_eq!(decode(&encoded).unwrap(), input);
    }

    #[test]
    fn round_trips_sample_arrays() {
        for input in [&[][..], &[0x00][..], &[0xab][..], &[0x12, 0x34, 0xde, 0xf0][..]] {
            assert_eq!(decode(&encode(input)).unwrap(), input);
        }
    }

    #[test]
    fn decode_fails_on_non_hex() {
        assert!(decode("xyz").is_none());
        assert!(decode("12z4").is_none());
    }

    #[test]
    fn decode_rejects_odd_length() {
        assert!(decode("f").is_none());
        assert!(decode("abc").is_none());
    }

    #[test]
    fn unsafe_decode_strips_non_hex() {
        assert_eq!(unsafe_decode("z1z2z"), vec![0x12]);
        assert_eq!(unsafe_decode("f"), vec![0x0f]);
        assert_eq!(unsafe_decode(""), Vec::<u8>::new());
    }
}
