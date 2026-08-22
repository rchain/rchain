//! Block-oriented Blake2b512 with online tree hashing.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/hash/Blake2b512Block.scala`, which is an
//! abbreviated version of BouncyCastle's `Blake2bDigest`. This is a hand-port: no crates.io crate
//! exposes the custom parameter-block + `peekFinalRoot`/`finalizeInternal` API. The `compress`
//! core, `IV`, `SIGMA`, and `ROUNDS` are copied verbatim.

const CHAIN_VALUE_LENGTH: usize = 8;
const BLOCK_LENGTH_BYTES: usize = 128;
const BLOCK_LENGTH_LONGS: usize = 16;
// Depth = 255, Fanout = ??, Keylength = 0, Digest length = 64 bytes.
const PARAM_VALUE_0: u64 = 0xFF00_0040;
// Inner length = 32 bytes.
const PARAM_VALUE_2: u64 = 0x2000;

/// Produced from the square roots of primes 2, 3, 5, 7, 11, 13, 17, 19 (same as SHA-512 IV).
const IV: [u64; 8] = [
    0x6A09_E667_F3BC_C908,
    0xBB67_AE85_84CA_A73B,
    0x3C6E_F372_FE94_F82B,
    0xA54F_F53A_5F1D_36F1,
    0x510E_527F_ADE6_82D1,
    0x9B05_688C_2B3E_6C1F,
    0x1F83_D9AB_FB41_BD6B,
    0x5BE0_CD19_137E_2179,
];

/// Message word permutations.
const SIGMA: [[u8; 16]; 12] = [
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3],
    [11, 8, 12, 0, 5, 2, 15, 13, 10, 14, 3, 6, 7, 1, 9, 4],
    [7, 9, 3, 1, 13, 12, 11, 14, 2, 6, 5, 10, 4, 0, 15, 8],
    [9, 0, 5, 7, 2, 4, 10, 15, 14, 1, 11, 12, 6, 8, 3, 13],
    [2, 12, 6, 10, 0, 11, 8, 3, 4, 13, 7, 5, 15, 14, 1, 9],
    [12, 5, 1, 15, 14, 13, 4, 10, 0, 7, 6, 3, 9, 2, 8, 11],
    [13, 11, 7, 14, 12, 1, 3, 9, 5, 0, 15, 4, 8, 6, 2, 10],
    [6, 15, 14, 9, 11, 3, 0, 8, 12, 2, 13, 7, 1, 4, 10, 5],
    [10, 2, 8, 4, 7, 6, 1, 5, 15, 11, 9, 14, 3, 12, 13, 0],
    [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15],
    [14, 10, 4, 8, 9, 15, 13, 6, 1, 12, 0, 2, 11, 7, 5, 3],
];

const ROUNDS: usize = 12;

use crate::errors::CryptoError;

/// Read a little-endian `u64` from `bytes[start..start + 8]`. The caller guarantees the 8-byte
/// window is in bounds (a full 128-byte message block or a length-checked serialization).
pub(crate) fn u64_from_le_at(bytes: &[u8], start: usize) -> u64 {
    let mut arr = [0u8; 8];
    arr.copy_from_slice(&bytes[start..start + 8]);
    u64::from_le_bytes(arr)
}

/// A Blake2b512 block hasher with a configurable fanout.
#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Blake2b512Block {
    chain_value: [u64; CHAIN_VALUE_LENGTH],
    t0: u64,
    t1: u64,
}

impl Blake2b512Block {
    /// Create a block hasher with the given fanout encoded into the parameter block.
    pub fn new(fanout: u8) -> Self {
        let param0_with_fanout = PARAM_VALUE_0 | ((fanout as u64 & 0xff) << 16);
        let mut chain_value = IV;
        chain_value[0] ^= param0_with_fanout;
        chain_value[2] ^= PARAM_VALUE_2;
        Self {
            chain_value,
            t0: 0,
            t1: 0,
        }
    }

    /// Copy constructor (the Scala `apply(src: Blake2b512Block)`).
    pub fn from_block(src: &Self) -> Self {
        Self {
            chain_value: src.chain_value,
            t0: src.t0,
            t1: src.t1,
        }
    }

    /// Compress a 128-byte block into the running state (the Scala `update(block, offset)`).
    pub fn update(&mut self, block: &[u8], offset: usize) {
        let (new_t0, new_t1) = self.new_t0_t1();
        let mut out = [0u64; CHAIN_VALUE_LENGTH];
        self.compress_into(block, offset, &mut out, false, false);
        self.chain_value = out;
        self.t0 = new_t0;
        self.t1 = new_t1;
    }

    /// Compute the final root as if `block` were the last block, without mutating state.
    pub fn peek_final_root(
        &self,
        block: &[u8],
        in_offset: usize,
        output: &mut [u8],
        out_offset: usize,
    ) {
        let mut temp = [0u64; CHAIN_VALUE_LENGTH];
        self.compress_into(block, in_offset, &mut temp, true, true);
        write_le_long(&temp, output, out_offset);
    }

    /// Compute the internal (32-byte) hash as if `block` were the last block, without mutating state.
    pub fn finalize_internal(
        &self,
        block: &[u8],
        in_offset: usize,
        output: &mut [u8],
        out_offset: usize,
    ) {
        let mut temp = [0u64; CHAIN_VALUE_LENGTH];
        self.compress_into(block, in_offset, &mut temp, true, false);
        write_le_long(&temp[..4], output, out_offset);
    }

    fn new_t0_t1(&self) -> (u64, u64) {
        let new_t0 = self.t0.wrapping_add(BLOCK_LENGTH_BYTES as u64);
        let new_t1 = if new_t0 == 0 {
            self.t1.wrapping_add(1)
        } else {
            self.t1
        };
        (new_t0, new_t1)
    }

    fn compress_into(
        &self,
        msg: &[u8],
        offset: usize,
        out: &mut [u64; CHAIN_VALUE_LENGTH],
        finalize: bool,
        root_finalize: bool,
    ) {
        let mut internal_state = [0u64; BLOCK_LENGTH_LONGS];
        let (new_t0, new_t1) = self.new_t0_t1();

        fn g(
            internal_state: &mut [u64; BLOCK_LENGTH_LONGS],
            m1: u64,
            m2: u64,
            pos_a: usize,
            pos_b: usize,
            pos_c: usize,
            pos_d: usize,
        ) {
            internal_state[pos_a] = internal_state[pos_a]
                .wrapping_add(internal_state[pos_b])
                .wrapping_add(m1);
            internal_state[pos_d] = (internal_state[pos_d] ^ internal_state[pos_a]).rotate_right(32);
            internal_state[pos_c] = internal_state[pos_c].wrapping_add(internal_state[pos_d]);
            internal_state[pos_b] = (internal_state[pos_b] ^ internal_state[pos_c]).rotate_right(24);
            internal_state[pos_a] = internal_state[pos_a]
                .wrapping_add(internal_state[pos_b])
                .wrapping_add(m2);
            internal_state[pos_d] = (internal_state[pos_d] ^ internal_state[pos_a]).rotate_right(16);
            internal_state[pos_c] = internal_state[pos_c].wrapping_add(internal_state[pos_d]);
            internal_state[pos_b] = (internal_state[pos_b] ^ internal_state[pos_c]).rotate_right(63);
        }

        // init
        internal_state[..CHAIN_VALUE_LENGTH].copy_from_slice(&self.chain_value);
        internal_state[CHAIN_VALUE_LENGTH..12].copy_from_slice(&IV[..4]);
        let f0: u64 = if finalize { u64::MAX } else { 0 };
        let f1: u64 = if root_finalize { u64::MAX } else { 0 };
        internal_state[12] = new_t0 ^ IV[4];
        internal_state[13] = new_t1 ^ IV[5];
        internal_state[14] = f0 ^ IV[6];
        internal_state[15] = f1 ^ IV[7];

        // message words
        let mut m = [0u64; BLOCK_LENGTH_LONGS];
        for (i, mi) in m.iter_mut().enumerate() {
            let start = offset + i * 8;
            *mi = u64_from_le_at(msg, start);
        }

        for round in 0..ROUNDS {
            let s = &SIGMA[round];
            g(&mut internal_state, m[s[0] as usize], m[s[1] as usize], 0, 4, 8, 12);
            g(&mut internal_state, m[s[2] as usize], m[s[3] as usize], 1, 5, 9, 13);
            g(&mut internal_state, m[s[4] as usize], m[s[5] as usize], 2, 6, 10, 14);
            g(&mut internal_state, m[s[6] as usize], m[s[7] as usize], 3, 7, 11, 15);
            g(&mut internal_state, m[s[8] as usize], m[s[9] as usize], 0, 5, 10, 15);
            g(&mut internal_state, m[s[10] as usize], m[s[11] as usize], 1, 6, 11, 12);
            g(&mut internal_state, m[s[12] as usize], m[s[13] as usize], 2, 7, 8, 13);
            g(&mut internal_state, m[s[14] as usize], m[s[15] as usize], 3, 4, 9, 14);
        }

        for i in 0..CHAIN_VALUE_LENGTH {
            out[i] = self.chain_value[i] ^ internal_state[i] ^ internal_state[i + 8];
        }
    }

    /// Serialize to 80 bytes: chain value (8 LE longs) + t0 + t1.
    pub fn to_bytes(&self) -> Vec<u8> {
        let mut out = Vec::with_capacity(80);
        for v in &self.chain_value {
            out.extend_from_slice(&v.to_le_bytes());
        }
        out.extend_from_slice(&self.t0.to_le_bytes());
        out.extend_from_slice(&self.t1.to_le_bytes());
        out
    }

    /// Deserialize from 80 bytes (the Scala `typeMapper.toCustom`).
    pub fn from_bytes(bytes: &[u8]) -> Result<Self, CryptoError> {
        if bytes.len() < 80 {
            return Err(CryptoError::InvalidLength {
                expected: 80,
                actual: bytes.len(),
            });
        }
        let mut chain_value = [0u64; CHAIN_VALUE_LENGTH];
        for (i, cv) in chain_value.iter_mut().enumerate() {
            *cv = u64_from_le_at(bytes, i * 8);
        }
        let t0 = u64_from_le_at(bytes, 64);
        let t1 = u64_from_le_at(bytes, 72);
        Ok(Self { chain_value, t0, t1 })
    }

    /// For testing only — will give invalid results otherwise.
    pub fn tweak_t0(&mut self) {
        self.t0 = u64::MAX;
    }

    /// Diagnostic string (mirrors `Blake2b512Block.debugStr`).
    pub fn debug_str(&self) -> String {
        let cv = self
            .chain_value
            .iter()
            .map(|v| v.to_string())
            .collect::<Vec<_>>()
            .join(", ");
        format!("chainValue: {cv}\nt0: {}\nt1: {}\n", self.t0, self.t1)
    }
}

fn write_le_long(values: &[u64], output: &mut [u8], out_offset: usize) {
    for (i, v) in values.iter().enumerate() {
        let bytes = v.to_le_bytes();
        output[out_offset + i * 8..out_offset + i * 8 + 8].copy_from_slice(&bytes);
    }
}
