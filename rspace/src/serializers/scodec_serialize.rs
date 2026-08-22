//! Bit-exact serializers matching scodec (the serialization foundation for Laws 7/8/10).
//!
//! Mirrors `ScodecSerialize.scala` and `Serialize.codecByteVector`. The encodings are:
//! - `variableSizeBytesLong(int64, bytes)` = 8-byte big-endian length + raw bytes.
//! - `seqOfN(int32, codec)` = 4-byte big-endian count + per-element codec.
//! - `bool(8)` = a single byte (`0x01`/`0x00`); `bool` (1 bit) and `uint2` (2 bits) are bit-packed
//!   MSB-first via [`BitWriter`]/[`BitReader`].
//!
//! The bit/value conventions are pinned by the differential tests in `testdata/differential/
//! scodec.tsv` (Scala ground truth).

use rchain_shared::serialize::Serialize;

use crate::errors::RSpaceError;
use crate::internal::{Datum, WaitingContinuation};
use crate::trace::event::{Consume, Produce};

/// `int64` length prefix + raw bytes (port of `variableSizeBytesLong(int64, bytes)`).
pub fn size_head(bytes: &[u8]) -> Vec<u8> {
    let mut out = (bytes.len() as i64).to_be_bytes().to_vec();
    out.extend_from_slice(bytes);
    out
}

/// Encode a sequence of byte vectors as `int32` count + `int64`-length-prefixed elements (port of
/// `codecSeqByteVector`).
pub fn encode_seq_byte_vectors(elements: &[Vec<u8>]) -> Vec<u8> {
    let mut out = (elements.len() as i32).to_be_bytes().to_vec();
    for element in elements {
        out.extend_from_slice(&(element.len() as i64).to_be_bytes());
        out.extend_from_slice(element);
    }
    out
}

/// Encode a boolean as an 8-bit value (port of `bool(8)`).
pub fn bool8(value: bool) -> Vec<u8> {
    vec![u8::from(value)]
}

/// Encode each element with its `Serialize` instance and sort by `ordByteVector` (port of
/// `toOrderedByteVectors`).
pub fn to_ordered_byte_vectors<A>(elements: &[A]) -> Vec<Vec<u8>>
where
    A: Serialize<A>,
{
    let mut encoded: Vec<Vec<u8>> = elements
        .iter()
        .map(|e| <A as Serialize<A>>::encode(e))
        .collect();
    encoded.sort_by(|a, b| crate::util::veccmp(a, b));
    encoded
}

/// A minimal MSB-first bit writer (for scodec's bit-level `bool`/`uint2`).
pub struct BitWriter {
    bytes: Vec<u8>,
    bit_len: usize,
}

impl BitWriter {
    pub fn new() -> Self {
        BitWriter {
            bytes: Vec::new(),
            bit_len: 0,
        }
    }

    pub fn write_bit(&mut self, bit: u8) {
        let byte_idx = self.bit_len / 8;
        if byte_idx == self.bytes.len() {
            self.bytes.push(0);
        }
        self.bytes[byte_idx] |= (bit & 1) << (7 - (self.bit_len % 8));
        self.bit_len += 1;
    }

    pub fn write_bits(&mut self, value: u64, n: usize) {
        for i in (0..n).rev() {
            self.write_bit(((value >> i) & 1) as u8);
        }
    }

    pub fn write_bytes(&mut self, data: &[u8]) {
        for &b in data {
            self.write_bits(b as u64, 8);
        }
    }

    pub fn finish(self) -> Vec<u8> {
        self.bytes
    }
}

/// A minimal MSB-first bit reader.
pub struct BitReader<'a> {
    bytes: &'a [u8],
    bit_pos: usize,
}

impl<'a> BitReader<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        BitReader { bytes, bit_pos: 0 }
    }

    pub fn read_bit(&mut self) -> u8 {
        let byte = self.bytes[self.bit_pos / 8];
        let bit = (byte >> (7 - (self.bit_pos % 8))) & 1;
        self.bit_pos += 1;
        bit
    }

    pub fn read_bits(&mut self, n: usize) -> u64 {
        let mut v = 0u64;
        for _ in 0..n {
            v = (v << 1) | (self.read_bit() as u64);
        }
        v
    }

    pub fn read_bytes_bits(&mut self, n: usize) -> Vec<u8> {
        let mut out = Vec::with_capacity(n);
        for _ in 0..n {
            out.push(self.read_bits(8) as u8);
        }
        out
    }
}

fn write_size_head(w: &mut BitWriter, bytes: &[u8]) {
    w.write_bits(bytes.len() as u64, 64);
    w.write_bytes(bytes);
}

fn read_size_head(r: &mut BitReader) -> Vec<u8> {
    let len = r.read_bits(64) as usize;
    r.read_bytes_bits(len)
}

fn encode_produce(w: &mut BitWriter, produce: &Produce) {
    w.write_bytes(produce.channels_hash.as_bytes());
    w.write_bytes(produce.hash.as_bytes());
    w.write_bit(u8::from(produce.persistent));
}

fn encode_consume(w: &mut BitWriter, consume: &Consume) {
    w.write_bits(consume.channels_hashes.len() as u64, 32);
    for h in &consume.channels_hashes {
        w.write_bytes(h.as_bytes());
    }
    w.write_bytes(consume.hash.as_bytes());
    w.write_bit(u8::from(consume.persistent));
}

fn encode_datum<A>(w: &mut BitWriter, datum: &Datum<A>)
where
    A: Serialize<A>,
{
    let a = <A as Serialize<A>>::encode(&datum.a);
    write_size_head(w, &a);
    w.write_bit(u8::from(datum.persist));
    encode_produce(w, &datum.source);
}

fn encode_waiting_continuation<P, K>(w: &mut BitWriter, wc: &WaitingContinuation<P, K>)
where
    P: Serialize<P>,
    K: Serialize<K>,
{
    w.write_bits(wc.patterns.len() as u64, 32);
    for p in &wc.patterns {
        let pb = <P as Serialize<P>>::encode(p);
        write_size_head(w, &pb);
    }
    let kb = <K as Serialize<K>>::encode(&wc.continuation);
    write_size_head(w, &kb);
    w.write_bit(u8::from(wc.persist));
    w.write_bits(wc.peeks.len() as u64, 32);
    for peek in &wc.peeks {
        w.write_bits(*peek as u64, 8);
    }
    encode_consume(w, &wc.source);
}

fn read_produce(r: &mut BitReader) -> Produce {
    let channels_hash = rchain_crypto::hash::blake2b256_hash::Blake2b256Hash::from_byte_array(
        &r.read_bytes_bits(32),
    );
    let hash = rchain_crypto::hash::blake2b256_hash::Blake2b256Hash::from_byte_array(
        &r.read_bytes_bits(32),
    );
    let persistent = r.read_bit() != 0;
    Produce::from_hash(channels_hash, hash, persistent)
}

fn read_consume(r: &mut BitReader) -> Consume {
    let count = r.read_bits(32) as usize;
    let mut channels_hashes = Vec::with_capacity(count);
    for _ in 0..count {
        channels_hashes.push(
            rchain_crypto::hash::blake2b256_hash::Blake2b256Hash::from_byte_array(
                &r.read_bytes_bits(32),
            ),
        );
    }
    let hash = rchain_crypto::hash::blake2b256_hash::Blake2b256Hash::from_byte_array(
        &r.read_bytes_bits(32),
    );
    let persistent = r.read_bit() != 0;
    Consume::from_hash(channels_hashes, hash, persistent)
}

fn read_datum<A>(r: &mut BitReader) -> Result<Datum<A>, RSpaceError>
where
    A: Serialize<A>,
{
    let a = <A as Serialize<A>>::decode(&read_size_head(r))
        .map_err(|_| RSpaceError::Codec("datum"))?;
    let persist = r.read_bit() != 0;
    let source = read_produce(r);
    Ok(Datum { a, persist, source })
}

fn read_waiting_continuation<P, K>(
    r: &mut BitReader,
) -> Result<WaitingContinuation<P, K>, RSpaceError>
where
    P: Serialize<P>,
    K: Serialize<K>,
{
    let pattern_count = r.read_bits(32) as usize;
    let mut patterns = Vec::with_capacity(pattern_count);
    for _ in 0..pattern_count {
        patterns.push(
            <P as Serialize<P>>::decode(&read_size_head(r))
                .map_err(|_| RSpaceError::Codec("pattern"))?,
        );
    }
    let continuation = <K as Serialize<K>>::decode(&read_size_head(r))
        .map_err(|_| RSpaceError::Codec("continuation"))?;
    let persist = r.read_bit() != 0;
    let peek_count = r.read_bits(32) as usize;
    let mut peeks = std::collections::BTreeSet::new();
    for _ in 0..peek_count {
        peeks.insert(r.read_bits(8) as usize);
    }
    let source = read_consume(r);
    Ok(WaitingContinuation {
        patterns,
        continuation,
        persist,
        peeks,
        source,
    })
}

/// Encode a list of data (port of `encodeDatums`).
pub fn encode_datums<A>(datums: &[Datum<A>]) -> Vec<u8>
where
    A: Serialize<A>,
{
    let mut encoded: Vec<Vec<u8>> = datums
        .iter()
        .map(|d| {
            let mut w = BitWriter::new();
            encode_datum(&mut w, d);
            w.finish()
        })
        .collect();
    encoded.sort_by(|a, b| crate::util::veccmp(a, b));
    encode_seq_byte_vectors(&encoded)
}

/// Encode a list of continuations (port of `encodeContinuations`).
pub fn encode_continuations<P, K>(konts: &[WaitingContinuation<P, K>]) -> Vec<u8>
where
    P: Serialize<P>,
    K: Serialize<K>,
{
    let mut encoded: Vec<Vec<u8>> = konts
        .iter()
        .map(|wc| {
            let mut w = BitWriter::new();
            encode_waiting_continuation(&mut w, wc);
            w.finish()
        })
        .collect();
    encoded.sort_by(|a, b| crate::util::veccmp(a, b));
    encode_seq_byte_vectors(&encoded)
}

/// Encode a list of joins (port of `encodeJoins`).
pub fn encode_joins<C>(joins: &[Vec<C>]) -> Vec<u8>
where
    C: Serialize<C>,
{
    let mut encoded: Vec<Vec<u8>> = joins
        .iter()
        .map(|join| {
            let mut channels: Vec<Vec<u8>> =
                join.iter().map(|c| <C as Serialize<C>>::encode(c)).collect();
            channels.sort_by(|a, b| crate::util::veccmp(a, b));
            encode_seq_byte_vectors(&channels)
        })
        .collect();
    encoded.sort_by(|a, b| crate::util::veccmp(a, b));
    encode_seq_byte_vectors(&encoded)
}

/// Encode pre-serialized data (port of `encodeDatumsBinary`).
pub fn encode_datums_binary(datums: &[Vec<u8>]) -> Vec<u8> {
    let mut encoded = datums.to_vec();
    encoded.sort_by(|a, b| crate::util::veccmp(a, b));
    encode_seq_byte_vectors(&encoded)
}

/// Encode pre-serialized continuations (port of `encodeContinuationsBinary`).
pub fn encode_continuations_binary(konts: &[Vec<u8>]) -> Vec<u8> {
    let mut encoded = konts.to_vec();
    encoded.sort_by(|a, b| crate::util::veccmp(a, b));
    encode_seq_byte_vectors(&encoded)
}

/// Encode pre-serialized joins (port of `encodeJoinsBinary`).
pub fn encode_joins_binary(joins: &[Vec<u8>]) -> Vec<u8> {
    let mut encoded = joins.to_vec();
    encoded.sort_by(|a, b| crate::util::veccmp(a, b));
    encode_seq_byte_vectors(&encoded)
}

fn decode_seq_byte_vectors(bytes: &[u8]) -> Vec<Vec<u8>> {
    let mut r = BitReader::new(bytes);
    let count = r.read_bits(32) as usize;
    let mut out = Vec::with_capacity(count);
    for _ in 0..count {
        out.push(read_size_head(&mut r));
    }
    out
}

/// Decode a list of data (port of `decodeDatums`).
pub fn decode_datums<A>(bytes: &[u8]) -> Result<Vec<Datum<A>>, RSpaceError>
where
    A: Serialize<A>,
{
    decode_seq_byte_vectors(bytes)
        .into_iter()
        .map(|b| read_datum(&mut BitReader::new(&b)))
        .collect()
}

/// Decode a list of continuations (port of `decodeContinuations`).
pub fn decode_continuations<P, K>(bytes: &[u8]) -> Result<Vec<WaitingContinuation<P, K>>, RSpaceError>
where
    P: Serialize<P>,
    K: Serialize<K>,
{
    decode_seq_byte_vectors(bytes)
        .into_iter()
        .map(|b| read_waiting_continuation(&mut BitReader::new(&b)))
        .collect()
}

/// Decode a list of joins (port of `decodeJoins`).
pub fn decode_joins<C>(bytes: &[u8]) -> Result<Vec<Vec<C>>, RSpaceError>
where
    C: Serialize<C>,
{
    decode_seq_byte_vectors(bytes)
        .into_iter()
        .map(|join_bytes| {
            decode_seq_byte_vectors(&join_bytes)
                .into_iter()
                .map(|c| {
                    <C as Serialize<C>>::decode(&c).map_err(|_| RSpaceError::Codec("channel"))
                })
                .collect::<Result<Vec<C>, RSpaceError>>()
        })
        .collect()
}

/// Encode a single datum (port of `encodeDatum`).
pub fn encode_datum_bytes<A>(datum: &Datum<A>) -> Vec<u8>
where
    A: Serialize<A>,
{
    let mut w = BitWriter::new();
    encode_datum(&mut w, datum);
    w.finish()
}

/// Decode a single datum (port of `decodeDatum`).
pub fn decode_datum<A>(bytes: &[u8]) -> Result<Datum<A>, RSpaceError>
where
    A: Serialize<A>,
{
    read_datum(&mut BitReader::new(bytes))
}

/// Decode a list of raw data (port of `decodeDatumsBinary`).
pub fn decode_datums_binary<A>(bytes: &[u8]) -> Result<Vec<DatumB<A>>, RSpaceError>
where
    A: Serialize<A>,
{
    decode_seq_byte_vectors(bytes)
        .into_iter()
        .map(|raw| {
            let decoded = read_datum(&mut BitReader::new(&raw))?;
            Ok(DatumB { decoded, raw })
        })
        .collect()
}

/// Decode a list of raw continuations (port of `decodeContinuationsBinary`).
pub fn decode_continuations_binary<P, K>(
    bytes: &[u8],
) -> Result<Vec<WaitingContinuationB<P, K>>, RSpaceError>
where
    P: Serialize<P>,
    K: Serialize<K>,
{
    decode_seq_byte_vectors(bytes)
        .into_iter()
        .map(|raw| {
            let decoded = read_waiting_continuation(&mut BitReader::new(&raw))?;
            Ok(WaitingContinuationB { decoded, raw })
        })
        .collect()
}

/// Decode a list of raw joins (port of `decodeJoinsBinary`).
pub fn decode_joins_binary<C>(bytes: &[u8]) -> Result<Vec<JoinsB<C>>, RSpaceError>
where
    C: Serialize<C>,
{
    decode_seq_byte_vectors(bytes)
        .into_iter()
        .map(|raw| {
            let decoded = decode_seq_byte_vectors(&raw)
                .into_iter()
                .map(|c| <C as Serialize<C>>::decode(&c).map_err(|_| RSpaceError::Codec("channel")))
                .collect::<Result<Vec<C>, RSpaceError>>()?;
            Ok(JoinsB { decoded, raw })
        })
        .collect()
}

/// A datum with its raw encoded bytes (port of `DatumB`).
#[derive(Clone, Debug)]
pub struct DatumB<A> {
    pub decoded: Datum<A>,
    pub raw: Vec<u8>,
}

/// A waiting continuation with its raw encoded bytes (port of `WaitingContinuationB`).
#[derive(Clone, Debug)]
pub struct WaitingContinuationB<P, K> {
    pub decoded: WaitingContinuation<P, K>,
    pub raw: Vec<u8>,
}

/// A join with its raw encoded bytes (port of `JoinsB`).
#[derive(Clone, Debug)]
pub struct JoinsB<C> {
    pub decoded: Vec<C>,
    pub raw: Vec<u8>,
}

/// Differential tests against the Scala `ScodecSerialize` codecs. Golden vectors are captured in
/// `testdata/differential/scodec.tsv`. Both the byte-aligned codecs and the bit-packed
/// `encode_datums`/`encode_continuations` (scodec `bool`/`uint2` at bit granularity) are covered.
#[cfg(test)]
mod differential {
    use super::*;
    use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
    use rchain_shared::base16;
    use rchain_shared::serialize::Serialize;
    use std::collections::BTreeSet;

    #[derive(Clone, Debug, PartialEq)]
    struct ByteChannel(Vec<u8>);
    impl Serialize<ByteChannel> for ByteChannel {
        fn encode(a: &ByteChannel) -> Vec<u8> {
            a.0.clone()
        }
        fn decode(bytes: &[u8]) -> Result<ByteChannel, String> {
            Ok(ByteChannel(bytes.to_vec()))
        }
    }

    fn load(case: &str) -> String {
        let path = concat!(
            env!("CARGO_MANIFEST_DIR"),
            "/testdata/differential/scodec.tsv"
        );
        let data = std::fs::read_to_string(path).unwrap();
        for line in data.lines() {
            if let Some((id, hex)) = line.split_once('\t') {
                if id == case {
                    return hex.to_string();
                }
            }
        }
        panic!("missing differential case: {case}");
    }

    fn hex(bytes: &[u8]) -> String {
        base16::encode(bytes)
    }

    #[test]
    fn differential_size_head_and_bool() {
        assert_eq!(hex(&size_head(&[0x01, 0x02])), load("size_head_0102"));
        assert_eq!(hex(&size_head(&[])), load("size_head_empty"));
        assert_eq!(hex(&bool8(true)), load("bool8_true"));
        assert_eq!(hex(&bool8(false)), load("bool8_false"));
    }

    #[test]
    fn differential_seq_byte_vectors() {
        assert_eq!(
            hex(&encode_seq_byte_vectors(&[vec![0x01], vec![0x02, 0x03]])),
            load("seq_bv_2")
        );
        assert_eq!(hex(&encode_seq_byte_vectors(&[])), load("seq_bv_0"));
    }

    #[test]
    fn differential_binary_variants_sort() {
        let unsorted = [vec![0x02], vec![0x01]];
        assert_eq!(
            hex(&encode_datums_binary(&unsorted)),
            load("datums_binary_unsorted")
        );
        assert_eq!(
            hex(&encode_continuations_binary(&unsorted)),
            load("cont_binary_unsorted")
        );
        assert_eq!(
            hex(&encode_joins_binary(&unsorted)),
            load("joins_binary_unsorted")
        );
    }

    #[test]
    fn differential_joins() {
        let joins = vec![
            vec![ByteChannel(vec![0x02]), ByteChannel(vec![0x01])],
            vec![ByteChannel(vec![0x03])],
        ];
        assert_eq!(hex(&encode_joins(&joins)), load("joins_nested"));
    }

    fn h32(v: u8) -> Blake2b256Hash {
        Blake2b256Hash::from_bytes([v; 32])
    }

    #[test]
    fn differential_datums_round_trip_and_match_scala() {
        let datum = Datum {
            a: ByteChannel(vec![]),
            persist: false,
            source: Produce::from_hash(h32(0xaa), h32(0xbb), false),
        };
        let bytes = encode_datums(&[datum.clone()]);
        assert_eq!(hex(&bytes), load("datum_1"));
        assert_eq!(decode_datums::<ByteChannel>(&bytes).unwrap(), vec![datum]);

        let datum = Datum {
            a: ByteChannel(vec![0x01, 0x02]),
            persist: true,
            source: Produce::from_hash(h32(0xcc), h32(0xdd), true),
        };
        let bytes = encode_datums(&[datum.clone()]);
        assert_eq!(hex(&bytes), load("datum_2"));
        assert_eq!(decode_datums::<ByteChannel>(&bytes).unwrap(), vec![datum]);
    }

    #[test]
    fn differential_continuations_round_trip_and_match_scala() {
        let wc = WaitingContinuation {
            patterns: vec![ByteChannel(vec![0x03])],
            continuation: ByteChannel(vec![0x04]),
            persist: false,
            peeks: BTreeSet::new(),
            source: Consume::from_hash(vec![h32(0xee)], h32(0xff), false),
        };
        let bytes = encode_continuations(&[wc.clone()]);
        assert_eq!(hex(&bytes), load("cont_1"));
        assert_eq!(
            decode_continuations::<ByteChannel, ByteChannel>(&bytes).unwrap(),
            vec![wc]
        );

        let wc = WaitingContinuation {
            patterns: vec![ByteChannel(vec![0x03]), ByteChannel(vec![0x04])],
            continuation: ByteChannel(vec![0x05]),
            persist: true,
            peeks: [0usize, 2usize].into_iter().collect(),
            source: Consume::from_hash(vec![h32(0xee), h32(0xdd)], h32(0xff), true),
        };
        let bytes = encode_continuations(&[wc.clone()]);
        assert_eq!(hex(&bytes), load("cont_2"));
        assert_eq!(
            decode_continuations::<ByteChannel, ByteChannel>(&bytes).unwrap(),
            vec![wc]
        );
    }
}
