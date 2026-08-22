//! LZ4 compression.
//!
//! Mirrors `shared/src/main/scala/coop/rchain/shared/Compression.scala`: a raw LZ4 block codec
//! whose `decompress` takes the caller-supplied decompressed length (the Scala `LZ4Compressor`,
//! *not* the length-prefixed `LZ4CompressorWithLength` used by `BlockStore`). Compression bytes are
//! not required to match the JVM (the Scala uses `highCompressor(17)`); decompression is
//! deterministic, so only the round-trip is pinned.

/// Compress a byte slice to a raw LZ4 block (no length prefix).
pub fn compress(content: &[u8]) -> Vec<u8> {
    lz4_flex::block::compress(content)
}

/// Decompress a raw LZ4 block of exactly `decompressed_length` bytes, or `None` on failure.
pub fn decompress(compressed: &[u8], decompressed_length: usize) -> Option<Vec<u8>> {
    lz4_flex::block::decompress(compressed, decompressed_length).ok()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn round_trips() {
        let data = b"the quick brown fox jumps over the lazy dog".to_vec();
        let compressed = compress(&data);
        assert_eq!(decompress(&compressed, data.len()).as_deref(), Some(data.as_slice()));
    }

    #[test]
    fn empty_round_trips() {
        let data: Vec<u8> = Vec::new();
        let compressed = compress(&data);
        assert_eq!(decompress(&compressed, 0), Some(data));
    }
}
