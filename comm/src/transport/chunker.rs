//! Packet chunking.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/Chunker.scala`.

use rchain_models::comm::protocol::{chunk, Chunk, ChunkData, ChunkHeader, Packet};

use crate::peer_node::PeerNode;

/// A sender + packet to be chunked (port of the `Blob` used by `Chunker.chunkIt`).
#[derive(Clone, PartialEq)]
pub struct Blob {
    pub sender: PeerNode,
    pub packet: Packet,
}

/// Chunk a blob into a header chunk followed by data chunks (port of `Chunker.chunkIt`).
pub fn chunk_it(
    network_id: &str,
    blob: &Blob,
    max_message_size: usize,
) -> Result<Vec<Chunk>, String> {
    let raw = blob.packet.content.clone();
    let kb500 = 1024 * 500;
    let compress = raw.len() > kb500;
    let content = if compress {
        rchain_shared::compression::compress(&raw)
    } else {
        raw.clone()
    };

    let header = Chunk {
        content: Some(chunk::Content::Header(ChunkHeader {
            sender: Some(blob.sender.to_node()),
            type_id: blob.packet.type_id.clone(),
            compressed: compress,
            content_length: i32::try_from(raw.len()).map_err(|e| e.to_string())?,
            network_id: network_id.to_string(),
        })),
    };

    let buffer = 2 * 1024;
    let chunk_size = max_message_size
        .checked_sub(buffer)
        .ok_or_else(|| format!("max_message_size {max_message_size} is too small (must exceed {buffer})"))?;
    let mut chunks = vec![header];
    for data in content.chunks(chunk_size) {
        chunks.push(Chunk {
            content: Some(chunk::Content::Data(ChunkData {
                content_data: data.to_vec(),
            })),
        });
    }
    Ok(chunks)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::peer_node::{NodeIdentifier, PeerNode};
    use rchain_shared::refined::Port;

    fn blob() -> Blob {
        Blob {
            sender: PeerNode::from(
                NodeIdentifier::new(vec![1]),
                "host".to_string(),
                Port::new(40400),
                Port::new(40404),
            ),
            packet: Packet {
                type_id: "BlockMessage".to_string(),
                content: vec![1, 2, 3],
            },
        }
    }

    #[test]
    fn chunk_it_rejects_too_small_max_message_size() {
        // `max_message_size < 2048` must `Err` (the `checked_sub` underflow guard), not panic (H-1).
        assert!(chunk_it("testnet", &blob(), 1024).is_err());
        assert!(chunk_it("testnet", &blob(), 2047).is_err());
    }

    #[test]
    fn chunk_it_chunks_content() {
        let chunks = chunk_it("testnet", &blob(), 4096).unwrap();
        // header + ceil(3 / (4096 - 2048)) = header + 1 data chunk.
        assert_eq!(chunks.len(), 2);
        assert!(matches!(chunks[0].content.as_ref().unwrap(), chunk::Content::Header(_)));
        assert!(matches!(chunks[1].content.as_ref().unwrap(), chunk::Content::Data(_)));
    }
}
