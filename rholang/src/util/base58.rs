//! Base58 (Bitcoin alphabet) encoding/decoding (port of `interpreter/util/codec/Base58.scala`).

use num_bigint::BigUint;
use num_traits::Zero;

const ALPHABET: &str = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz";

/// The base-58 representation of `input` (interpreting bytes as an unsigned big-endian integer).
pub fn encode(input: &[u8]) -> String {
    if input.is_empty() {
        return String::new();
    }
    let fifty_eight = BigUint::from(58u8);
    let mut n = BigUint::from_bytes_be(input);
    let mut digits: Vec<char> = Vec::new();
    while !n.is_zero() {
        let rem = &n % &fifty_eight;
        let d = rem.to_bytes_be().last().copied().unwrap_or(0) as usize;
        digits.push(ALPHABET.as_bytes()[d] as char);
        n /= &fifty_eight;
    }
    for _ in input.iter().take_while(|&&b| b == 0) {
        digits.push(ALPHABET.as_bytes()[0] as char);
    }
    digits.iter().rev().collect()
}

/// Decode a base-58 string, returning `None` on an invalid character.
pub fn decode(input: &str) -> Option<Vec<u8>> {
    let zero_count = input.chars().take_while(|&c| c == ALPHABET.as_bytes()[0] as char).count();
    let rest: Vec<char> = input.chars().skip(zero_count).collect();
    if rest.is_empty() {
        return Some(vec![0u8; zero_count]);
    }

    let fifty_eight = BigUint::from(58u8);
    let mut n = BigUint::zero();
    for c in rest {
        let digit = digit_value(c)?;
        n = n * &fifty_eight + BigUint::from(digit);
    }

    let mut out = vec![0u8; zero_count];
    out.extend_from_slice(&n.to_bytes_be());
    Some(out)
}

fn digit_value(c: char) -> Option<u8> {
    ALPHABET.chars().position(|x| x == c).map(|i| i as u8)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn encodes_empty() {
        assert_eq!(encode(&[]), "");
        assert_eq!(decode("").unwrap(), Vec::<u8>::new());
    }

    #[test]
    fn round_trips_samples() {
        for input in [&[0u8][..], &[0u8, 0u8][..], &[1u8][..], &[0xab, 0xcd][..], &[0xff, 0x00, 0x01][..]] {
            assert_eq!(decode(&encode(input)).unwrap(), input);
        }
    }

    #[test]
    fn decodes_leading_zeroes() {
        assert_eq!(decode("111").unwrap(), vec![0u8, 0u8, 0u8]);
    }

    #[test]
    fn decode_fails_on_invalid_char() {
        assert!(decode("0").is_none());
        assert!(decode("I").is_none());
        assert!(decode("l").is_none());
    }
}
