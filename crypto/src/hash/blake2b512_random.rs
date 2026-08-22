//! Blake2b512-based splittable and mergeable random number generator.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/hash/Blake2b512Random.scala`. This is a
//! hand-port (no crates.io crate provides this construction); it is pinned by the known-answer
//! test vectors ported from `Blake2b512RandomTest.scala`.

use super::blake2b512_block::{u64_from_le_at, Blake2b512Block};
use crate::errors::CryptoError;
use rand::RngCore;

/// The path region holds 112 bytes; the following 16 bytes hold the 128-bit counter (little-endian).
const PATH_CAPACITY: usize = 112;

/// A splittable/mergeable RNG state specialized for generating 256-bit unforgeable names.
#[derive(Clone, Debug)]
pub struct Blake2b512Random {
    digest: Blake2b512Block,
    last_block: [u8; 128],
    path_position: usize,
    hash_array: [u8; 64],
    position: usize,
}

/// A well-formed serialized `Blake2b512Random` state. `TryFrom<&[u8]>` validates the layout (length,
/// `path_position`/`remainder_position` bounds, and slice extents); [`Blake2b512Random::from_bytes`]
/// is then *total* on this refinement.
pub struct SerializedRandom<'a>(&'a [u8]);

impl<'a> TryFrom<&'a [u8]> for SerializedRandom<'a> {
    type Error = CryptoError;

    fn try_from(bytes: &'a [u8]) -> Result<Self, CryptoError> {
        if bytes.is_empty() {
            return Ok(SerializedRandom(bytes));
        }
        if bytes.len() < 98 {
            return Err(CryptoError::InvalidLength {
                expected: 98,
                actual: bytes.len(),
            });
        }
        let path_position = bytes[96] as usize;
        let remainder_position = bytes[97] as usize;
        if path_position > PATH_CAPACITY {
            return Err(CryptoError::InvalidLength {
                expected: PATH_CAPACITY,
                actual: path_position,
            });
        }
        if remainder_position > 64 {
            return Err(CryptoError::InvalidLength {
                expected: 64,
                actual: remainder_position,
            });
        }
        let path_end = 98 + path_position;
        if path_end > bytes.len() {
            return Err(CryptoError::InvalidLength {
                expected: path_end,
                actual: bytes.len(),
            });
        }
        if remainder_position != 0 {
            let rem_len = 64 - remainder_position;
            if path_end + rem_len > bytes.len() {
                return Err(CryptoError::InvalidLength {
                    expected: path_end + rem_len,
                    actual: bytes.len(),
                });
            }
        }
        Ok(SerializedRandom(bytes))
    }
}

impl Blake2b512Random {
    /// A fresh state with the given fanout and an empty block (the internal merge constructor).
    fn new_with_fanout(fanout: u8) -> Self {
        Self {
            digest: Blake2b512Block::new(fanout),
            last_block: [0u8; 128],
            path_position: 0,
            hash_array: [0u8; 64],
            position: 0,
        }
    }

    /// Hash `init[offset..offset + length]` as a sequence of zero-padded 128-byte blocks.
    fn from_bytes_at(init: &[u8], offset: usize, length: usize) -> Self {
        let mut result = Self::new_with_fanout(0);
        let mut base = offset;
        while base + 128 <= offset + length {
            result.digest.update(init, base);
            base += 128;
        }
        if base != offset + length {
            let mut padded = [0u8; 128];
            let n = offset + length - base;
            padded[..n].copy_from_slice(&init[base..base + n]);
            result.digest.update(&padded, 0);
        }
        result
    }

    /// Hash `init` as the initial value (the Scala `apply(init)`).
    pub fn from_init(init: &[u8]) -> Self {
        Self::from_bytes_at(init, 0, init.len())
    }

    /// Generate `length` random bytes and use them as the initial value (the Scala `apply(length)`).
    pub fn new_random(length: usize) -> Self {
        let mut bytes = vec![0u8; length];
        rand::rngs::OsRng.fill_bytes(&mut bytes);
        Self::from_init(&bytes)
    }

    /// The default 128-byte random generator (the Scala `defaultRandom`).
    pub fn default_random() -> Self {
        Self::new_random(128)
    }

    /// Current `position` (0 or 32) — the Scala `getPosition`.
    pub fn position(&self) -> usize {
        self.position
    }

    /// Copy the state (the Scala `copy()`); `position` and the buffered hash are reset.
    pub fn copy(&self) -> Self {
        Self {
            digest: Blake2b512Block::from_block(&self.digest),
            last_block: self.last_block,
            path_position: self.path_position,
            hash_array: [0u8; 64],
            position: 0,
        }
    }

    fn add_byte(&mut self, index: u8) {
        if self.path_position == PATH_CAPACITY {
            let block = self.last_block;
            self.digest.update(&block, 0);
            self.last_block = [0u8; 128];
            self.path_position = 0;
        }
        self.last_block[self.path_position] = index;
        self.path_position += 1;
    }

    fn hash(&mut self) {
        self.digest
            .peek_final_root(&self.last_block, 0, &mut self.hash_array, 0);
        let low = u64_from_le_at(&self.last_block, 112);
        if low == u64::MAX {
            let high = u64_from_le_at(&self.last_block, 120);
            self.last_block[112..120].copy_from_slice(&0u64.to_le_bytes());
            self.last_block[120..128].copy_from_slice(&high.wrapping_add(1).to_le_bytes());
        } else {
            self.last_block[112..120].copy_from_slice(&low.wrapping_add(1).to_le_bytes());
        }
    }

    /// Produce the next 32 bytes of the (64-byte) hash output.
    pub fn next(&mut self) -> Vec<u8> {
        if self.position == 0 {
            self.hash();
            self.position = 32;
            self.hash_array[0..32].to_vec()
        } else {
            self.position = 0;
            self.hash_array[32..64].to_vec()
        }
    }

    /// Split the state with a single byte (the Scala `splitByte`).
    pub fn split_byte(&self, index: u8) -> Self {
        let mut split = self.copy();
        split.add_byte(index);
        split
    }

    /// Split the state with a 16-bit little-endian value (the Scala `splitShort`).
    pub fn split_short(&self, index: u16) -> Self {
        let mut split = self.copy();
        let packed = index.to_le_bytes();
        split.add_byte(packed[0]);
        split.add_byte(packed[1]);
        split
    }

    /// Merge two or more states (the Scala `merge`).
    pub fn merge(children: &[Self]) -> Self {
        assert!(
            children.len() >= 2,
            "Blake2b512Random should have at least 2 inputs to merge, received {}.",
            children.len()
        );
        internal_merge(children)
    }

    /// For testing only — force the low counter to wrap around on the next `hash`.
    pub fn tweak_length0(&mut self) {
        self.last_block[112..120].copy_from_slice(&u64::MAX.to_le_bytes());
    }

    /// Serialize to bytes (the Scala `typeMapper.toBase`).
    pub fn to_bytes(&self) -> Vec<u8> {
        let remainder_size = if self.position == 0 {
            0
        } else {
            64 - self.position
        };
        let total = 16 + 80 + 2 + self.path_position + remainder_size;
        let mut result = Vec::with_capacity(total);
        // 128-bit counter (little-endian).
        result.extend_from_slice(&self.last_block[112..120]);
        result.extend_from_slice(&self.last_block[120..128]);
        // digest.
        result.extend_from_slice(&self.digest.to_bytes());
        // two positions.
        result.push(self.path_position as u8);
        result.push(self.position as u8);
        // partial path.
        result.extend_from_slice(&self.last_block[..self.path_position]);
        // remainder.
        if remainder_size != 0 {
            result.extend_from_slice(&self.hash_array[self.position..64]);
        }
        result
    }

    /// Deserialize from a validated serialized state (total; the Scala `typeMapper.toCustom`).
    ///
    /// The layout is guaranteed by [`SerializedRandom`], so the slicing below cannot go out of
    /// bounds and the 80-byte digest block is always present.
    pub fn from_bytes(serialized: &SerializedRandom) -> Result<Self, CryptoError> {
        let bytes = serialized.0;
        if bytes.is_empty() {
            return Ok(Self::from_init(&[]));
        }
        let path_position = bytes[96] as usize;
        let remainder_position = bytes[97] as usize;
        let path_end = 98 + path_position;
        let digest = Blake2b512Block::from_bytes(&bytes[16..96])?;
        let mut result = Self {
            digest,
            last_block: [0u8; 128],
            path_position: 0,
            hash_array: [0u8; 64],
            position: 0,
        };
        result.last_block[112..120].copy_from_slice(&bytes[0..8]);
        result.last_block[120..128].copy_from_slice(&bytes[8..16]);
        result.last_block[..path_position].copy_from_slice(&bytes[98..path_end]);
        result.path_position = path_position;
        if remainder_position != 0 {
            let rem_len = 64 - remainder_position;
            result.hash_array[remainder_position..64]
                .copy_from_slice(&bytes[path_end..path_end + rem_len]);
        }
        result.position = remainder_position;
        Ok(result)
    }

    /// Diagnostic string (mirrors `Blake2b512Random.debugStr`).
    pub fn debug_str(&self) -> String {
        let rot_position = ((self.position.wrapping_sub(1)) & 0x3f) + 1;
        format!(
            "digest: {}lastBlock: {:?}\npathPosition: {}\nposition: {}\nrotPosition: {}\nremainder: {:?}\n",
            self.digest.debug_str(),
            self.last_block,
            self.path_position,
            self.position,
            rot_position,
            &self.hash_array[rot_position..64]
        )
    }
}

impl PartialEq for Blake2b512Random {
    fn eq(&self, other: &Self) -> bool {
        self.digest == other.digest
            && self.path_position == other.path_position
            && self.position == other.position
            && self.last_block == other.last_block
            && (self.position == 0 || self.hash_array[self.position..] == other.hash_array[self.position..])
    }
}

impl Eq for Blake2b512Random {}

// `Blake2b512Random` serializes as JSON unit (`null`) and deserializes to `defaultRandom`, matching
// the Scala `encodeBlake2b512Random`/`decodeDummyBlake2b512Random` (`Encoder.encodeUnit` /
// `Decoder.decodeUnit`).
impl serde::Serialize for Blake2b512Random {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_unit()
    }
}

impl<'de> serde::Deserialize<'de> for Blake2b512Random {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let () = <() as serde::Deserialize>::deserialize(deserializer)?;
        Ok(Blake2b512Random::default_random())
    }
}

fn internal_merge(children: &[Blake2b512Random]) -> Blake2b512Random {
    let mut squashed = Vec::new();
    let mut chain_block = [0u8; 128];
    for slice in children.chunks(255) {
        let mut result = Blake2b512Random::new_with_fanout(slice.len() as u8);
        for quad in slice.chunks(4) {
            for (i, child) in quad.iter().enumerate() {
                child
                    .digest
                    .finalize_internal(&child.last_block, 0, &mut chain_block, i * 32);
            }
            if quad.len() != 4 {
                chain_block[quad.len() * 32..].fill(0);
            }
            result.digest.update(&chain_block, 0);
        }
        squashed.push(result);
    }
    if squashed.len() == 1 {
        squashed.swap_remove(0)
    } else {
        internal_merge(&squashed)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::base16;

    fn next_pair(rng: &mut Blake2b512Random) -> (String, String) {
        (base16::encode(&rng.next()), base16::encode(&rng.next()))
    }

    #[test]
    fn empty_gives_a_predictable_result() {
        let mut rng = Blake2b512Random::from_init(&[]);
        let (res1, res2) = next_pair(&mut rng);
        assert_eq!(
            res1,
            "52884e9cfaf738709d271e9c0268f05964395678d9ccd61b187d67224a464230"
        );
        assert_eq!(
            res2,
            "cfa2ebb91185e9764a5aef6c7f5a6756cfe0f48a33c5bfabffdacac55d32f24a"
        );
    }

    #[test]
    fn handles_split_short() {
        let mut split = Blake2b512Random::from_init(&[]).split_short(0x6487);
        assert_eq!(
            base16::encode(&split.next()),
            "745ce0f59aa7ebadc31c097126ac85870c3364b561d1d81935eb01ef5968d4b3"
        );
        assert_eq!(
            base16::encode(&split.next()),
            "2d7bd219e4ce1e18e38c06eecdf17098ed49d66890086d19543a84fe88d80d67"
        );
    }

    #[test]
    fn implements_wraparound() {
        let mut rng = Blake2b512Random::from_init(&[]);
        rng.tweak_length0();
        let expected = [
            "b63ea0e23d853977e02707364c753bd414c4828e294c1b0c39d046bacf18f5cf",
            "4b850fc7d0a930cd89a8907ccee22f41941bd896127e71301eba137a347b131f",
            "a913716961120edbbb9f08cc513a40321de334aa99a6991f97eff93b799f9cab",
            "be14efe796e8a10a8bbc55e4691f8eeb71df5dc37b0b0b79133150b8cc90533a",
        ];
        for exp in expected {
            assert_eq!(base16::encode(&rng.next()), exp);
        }
    }

    #[test]
    fn rolls_over_after_enough_byte_splits() {
        let mut rollover = Blake2b512Random::from_init(&[]);
        for n in 0..=112u8 {
            rollover = rollover.split_byte(n);
        }
        assert_eq!(
            base16::encode(&rollover.next()),
            "0f6ccee70daf946d23361a92e672515898a287456c38517bd92bb0925ee18103"
        );
        assert_eq!(
            base16::encode(&rollover.next()),
            "7d9a1831ce7f42b818c61223f709d55dd9cf310b01e19e2526d2d13e1b9ed61c"
        );
    }

    #[test]
    fn handles_next_then_rollover() {
        let mut rng = Blake2b512Random::from_init(&[]);
        rng.next();
        let mut rollover = rng;
        for n in 0..=112u8 {
            rollover = rollover.split_byte(n);
        }
        assert_eq!(
            base16::encode(&rollover.next()),
            "1fa2af2fdc0521dacc1b06d0dc9ee729075283c7e2ba8df7b637bd05134e2d30"
        );
        assert_eq!(
            base16::encode(&rollover.next()),
            "9c7a9b1f04907c73263a81178540a5d7e3102fe260a9f15b80ff91f87de2039b"
        );
    }

    #[test]
    fn partial_gives_a_predictable_result() {
        let mut rng = Blake2b512Random::from_init(b"Hello, World!");
        assert_eq!(
            base16::encode(&rng.next()),
            "34c06b6f6907595709c44a1c2f4940210d99b04302937a88e14a5c5e2d439221"
        );
        assert_eq!(
            base16::encode(&rng.next()),
            "7c3c57f0220fa003ad9c10fd785001c11f2b626f0d5da8367499200e10166276"
        );
    }

    #[test]
    fn single_block_prefix() {
        let msg = "Sed ut perspiciatis unde omnis iste natus error sit voluptatem accusantium doloremque laudantium, totam rem aperiam, eaque ipsa ";
        let mut rng = Blake2b512Random::from_init(msg.as_bytes());
        assert_eq!(
            base16::encode(&rng.next()),
            "459691c149f10c8cf45a4f84421d89e97228b91e046f7afbcf3a4131216c538b"
        );
        assert_eq!(
            base16::encode(&rng.next()),
            "1e676c4ad57a46408312a5209e8498d43023e43ab0bfabfc57a535663dfa3918"
        );
    }

    #[test]
    fn single_block_and_partial_prefix() {
        let msg = "quae ab illo inventore veritatis et quasi architecto beatae vitae dicta sunt explicabo. Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit aut fugit, sed quia consequuntur magni dolores ";
        let mut rng = Blake2b512Random::from_init(msg.as_bytes());
        assert_eq!(
            base16::encode(&rng.next()),
            "c27d88a63898e9f593ae34439112572feedd241c4223e6c62e997e45267b285d"
        );
        assert_eq!(
            base16::encode(&rng.next()),
            "8c8d36972e4bfa65fdde555c1247ee221ff7c0031e92ec790aa01549321e7c86"
        );
    }

    #[test]
    fn multi_block_prefix() {
        let msg = "eos qui ratione voluptatem sequi nesciunt. Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, consectetur, adipisci velit, sed quia non numquam eius modi tempora incidunt ut labore et dolore magnam aliquam quaerat voluptatem. Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? Quis autem ";
        let mut rng = Blake2b512Random::from_init(msg.as_bytes());
        assert_eq!(
            base16::encode(&rng.next()),
            "07ac715093bb984b8f9364b6ccdf89ca63dbdcc164000d115ee333d6566b3e87"
        );
        assert_eq!(
            base16::encode(&rng.next()),
            "1235bb1ec4ba9bf58f6f0aa10aaf53e373191a1c6a849fbc7b8a1d31a0affc61"
        );
    }

    #[test]
    fn multi_block_and_partial_prefix() {
        let msg = "Quis autem vel eum iure reprehenderit qui in ea voluptate velit esse quam nihil molestiae consequatur, vel illum qui dolorem eum fugiat quo voluptas nulla pariatur?\nAt vero eos et accusamus et iusto odio dignissimos ducimus qui blanditiis praesentium voluptatum deleniti atque corrupti quos dolores et quas molestias excepturi sint occaecati cupiditate non provident, similique sunt in culpa qui officia deserunt mollitia animi, id est laborum et dolorum fuga.";
        let mut rng = Blake2b512Random::from_init(msg.as_bytes());
        assert_eq!(
            base16::encode(&rng.next()),
            "828f766bc845c41944f1a9933b0835afabf636abd93f8bd986db7a73c7de056c"
        );
        assert_eq!(
            base16::encode(&rng.next()),
            "890399ff51fd6f02c09ad76d69c51445805252c60d7511a767e2e01dce0c45cd"
        );
    }

    #[test]
    fn equal_instances_with_same_seed() {
        let rnd1 = Blake2b512Random::from_init(&[]).split_byte(42);
        let rnd2 = Blake2b512Random::from_init(&[]).split_byte(42);
        assert_eq!(rnd1, rnd2);
    }

    #[test]
    #[should_panic]
    fn merge_with_empty_children_throws() {
        let _ = Blake2b512Random::merge(&[]);
    }

    #[test]
    #[should_panic]
    fn merge_with_single_child_throws() {
        let rnd = Blake2b512Random::from_init(&[]).split_byte(0);
        let _ = Blake2b512Random::merge(std::slice::from_ref(&rnd));
    }

    #[test]
    fn merge_with_two_children() {
        let base = Blake2b512Random::from_init(&[]);
        let b0 = base.split_byte(0);
        let b1 = base.split_byte(1);
        let mut merged = Blake2b512Random::merge(&[b0, b1]);
        assert_eq!(
            base16::encode(&merged.next()),
            "ce190f4283d4b11653cb78ee8fbc68a5b8cb62511a1f2ed3e836400e62144fa9"
        );
        assert_eq!(
            base16::encode(&merged.next()),
            "460e913fb6f2250fb1ae2cd6ceeb5501b0d83b29abd538d3508ec6845904342d"
        );
    }

    #[test]
    fn merge_with_many_children() {
        let mut builder = Vec::new();
        let base = Blake2b512Random::from_init(&[]);
        for i in (0..20).step_by(5) {
            let split_once = base.split_byte(i as u8);
            for j in (0..255).step_by(5) {
                let split_twice = split_once.split_byte(j as u8);
                for k in 0..255u8 {
                    builder.push(split_twice.split_byte(k));
                }
            }
        }
        let mut merged = Blake2b512Random::merge(&builder);
        assert_eq!(
            base16::encode(&merged.next()),
            "ceff4f6065e6b508b46f4c7b687c3b67eb3bcdcbb52a4ad098e481876b745156"
        );
        assert_eq!(
            base16::encode(&merged.next()),
            "d30832a104feffed4502542768e8f3b05d12593ba29aacdc086c4d1db405e4e6"
        );
    }

    #[test]
    fn merge_is_order_sensitive() {
        let rnd = Blake2b512Random::from_init(&[]);
        let rnd1 = rnd.split_byte(1);
        let rnd2 = rnd.split_byte(2);
        let merged12 = Blake2b512Random::merge(&[rnd1.clone(), rnd2.clone()]);
        let merged21 = Blake2b512Random::merge(&[rnd2, rnd1]);
        assert_eq!(merged12, merged12);
        assert_ne!(merged12, merged21);
    }

    #[test]
    fn serialization_round_trips() {
        // Representative states: empty, split, next (position 32), and a rollover state.
        let mut states = Vec::new();
        states.push(Blake2b512Random::from_init(&[]));
        states.push(Blake2b512Random::from_init(b"abc").split_short(0x1234));
        let mut after_next = Blake2b512Random::from_init(b"abc");
        after_next.next();
        states.push(after_next);
        let mut rollover = Blake2b512Random::from_init(&[]);
        for n in 0..112u8 {
            rollover = rollover.split_byte(n);
        }
        states.push(rollover);

        for state in &states {
            let bytes = state.to_bytes();
            let serialized =
                SerializedRandom::try_from(bytes.as_slice()).expect("valid serialized state");
            let restored = Blake2b512Random::from_bytes(&serialized).expect("deserialize state");
            assert_eq!(restored, *state, "round-trip mismatch");
            assert_eq!(restored.to_bytes(), bytes, "encoding not idempotent");
            // A round-tripped state must produce the same next value.
            let mut a = state.clone_like();
            let mut b = restored;
            assert_eq!(a.next(), b.next(), "next mismatch after round-trip");
        }
    }
}

#[cfg(test)]
impl Blake2b512Random {
    /// Clone preserving `position` and the buffered hash (unlike `copy`, which resets them).
    fn clone_like(&self) -> Self {
        Self {
            digest: Blake2b512Block::from_block(&self.digest),
            last_block: self.last_block,
            path_position: self.path_position,
            hash_array: self.hash_array,
            position: self.position,
        }
    }
}
