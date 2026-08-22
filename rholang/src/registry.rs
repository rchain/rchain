//! Registry URI derivation and the registry bootstrap AST.
//!
//! `build_uri` is a Rust-first, self-consistent URI: `rho:id:` + z-base-32 of the 32-byte
//! blake2b256 hash. The Scala oracle's CRC14 + `org.lightningj.util.ZBase32` 270-bit encoding is
//! deliberately dropped (a Scala-specific legacy whose exact bit order is not reproducible), so
//! the genesis `shorthands` map is regenerated from this encoding instead.
//! `RegistryBootstrap` mirrors `RegistryBootstrap.scala` — the genesis AST that installs the
//! registry contracts on the fixed REG_* channels.

use std::collections::BTreeMap;

use rchain_models::ast::{AlwaysEqual, Expr, New, Par, Receive, ReceiveBind, Send, Var};
use rchain_models::par_ops::from_expr;
use rchain_models::types::{count_free_vars, FreeCount};

use crate::system_processes::FixedChannels;

/// The z-base-32 alphabet (standard z-base-32, no padding).
const ZBASE32_ALPHABET: &[u8; 32] = b"ybndrfg8ejkmcpqxot1uwisza345h769";

/// Encode the first `bit_length` bits of `data` (MSB-first) as z-base-32, zero-padding the final
/// partial 5-bit group (the standard z-base-32 encoding, e.g. libzbase32).
fn zbase32_encode(data: &[u8], bit_length: usize) -> String {
    let mut out = String::with_capacity((bit_length + 4) / 5);
    for p in (0..bit_length).step_by(5) {
        let mut v = 0u32;
        for k in 0..5 {
            let idx = p + k;
            v <<= 1;
            if idx < bit_length {
                let byte = data[idx / 8];
                let bit = (byte >> (7 - (idx % 8))) & 1;
                v |= bit as u32;
            }
        }
        out.push(ZBASE32_ALPHABET[v as usize] as char);
    }
    out
}

/// Build a registry URI from a 32-byte hash. Rust-first: `rho:id:` + z-base-32 of the hash (no
/// Scala CRC14 / 270-bit `ZBase32` legacy). The genesis `shorthands` map is derived from this.
pub fn build_uri(arr: &[u8]) -> String {
    format!("rho:id:{}", zbase32_encode(arr, arr.len() * 8))
}

/// The registry bootstrap AST (port of `RegistryBootstrap.AST`).
pub fn registry_bootstrap_ast() -> Par {
    Par {
        news: vec![
            bootstrap(&FixedChannels::reg_lookup()),
            bootstrap(&FixedChannels::reg_insert_random()),
            bootstrap(&FixedChannels::reg_insert_signed()),
        ],
        ..Default::default()
    }
}

/// A single registry bootstrap contract: `new { for (x <- channel) { x!(channel) } }` (port of
/// `RegistryBootstrap.bootstrap`).
fn bootstrap(channel: &Par) -> New {
    let pattern = from_expr(Expr::EVar(Box::new(Var::FreeVar(0)))).quote();
    New {
        bind_count: 1,
        p: Box::new(Par {
            receives: vec![Receive {
                binds: vec![ReceiveBind {
                    patterns: vec![pattern.clone()],
                    source: Box::new(channel.clone().quote()),
                    remainder: None,
                    free_count: FreeCount::from_nonneg(count_free_vars(&pattern)),
                }],
                body: Box::new(Par {
                    sends: vec![Send {
                        chan: Box::new(from_expr(Expr::EVar(Box::new(Var::BoundVar(0)))).quote()),
                        data: vec![channel.clone().quote()],
                        persistent: false,
                        locally_free: AlwaysEqual(vec![]),
                        connective_used: false,
                    }],
                    ..Default::default()
                }),
                persistent: false,
                peek: false,
                bind_count: 1,
                locally_free: AlwaysEqual(vec![]),
                connective_used: false,
            }],
            ..Default::default()
        }),
        uri: vec![],
        injections: BTreeMap::new(),
        locally_free: AlwaysEqual(vec![]),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn build_uri_produces_prefixed_identifier() {
        let uri = build_uri(&[0u8; 32]);
        assert!(uri.starts_with("rho:id:"));
        // 256 bits -> ceil(256/5) = 52 z-base-32 chars (zero-padded final group).
        assert_eq!(uri.len(), "rho:id:".len() + 52);
        // An all-zero hash encodes to the alphabet's first char ('y'), repeated.
        assert_eq!(uri, format!("rho:id:{}", "y".repeat(52)));
    }

    #[test]
    fn build_uri_is_deterministic() {
        assert_eq!(build_uri(&[0xab; 32]), build_uri(&[0xab; 32]));
        assert_ne!(build_uri(&[0xab; 32]), build_uri(&[0xcd; 32]));
    }
}
