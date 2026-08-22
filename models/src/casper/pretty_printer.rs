//! Casper message pretty-printing (port of `casper/PrettyPrinter.scala`).
//!
//! The Scala overloads `buildString` / `buildStringNoLimit` / `buildStringSig` become distinct
//! functions (Rust has no overloading). Byte strings are hex-encoded with `Base16`; the plain
//! `buildString(ByteString)` overload truncates to 10 hex characters.

use rchain_shared::base16;

use crate::block_hash::BlockHash;
use crate::casper::protocol::casper_message::{
    BlockHashMessage, BlockMessage, CasperMessage, DeployData, ProcessedDeploy, SignedDeployData,
};

/// Hex-encode without a length limit (port of `buildStringNoLimit`).
pub fn build_string_no_limit(bytes: &[u8]) -> String {
    base16::encode(bytes)
}

/// Render a consensus-protocol message (port of `buildString(CasperMessage, short)`).
pub fn build_string_casper_message(message: &CasperMessage, short: bool) -> String {
    match message {
        CasperMessage::BlockMessage(b) => build_string_block(b, short),
        _ => "Unknown consensus protocol message".to_string(),
    }
}

/// Render a block-hash message (port of `buildString(BlockHashMessage)`).
pub fn build_string_block_hash_message(bh: &BlockHashMessage) -> String {
    format!("Block hash: {}", build_string_bytes(bh.block_hash.as_bytes()))
}

/// Render a processed deploy (port of `buildString(ProcessedDeploy)`).
pub fn build_string_processed_deploy(d: &ProcessedDeploy) -> String {
    format!(
        "User: {}, Cost: {} {}",
        build_string_no_limit(&d.deploy.deployer),
        d.cost.cost,
        build_string_signed_deploy(&d.deploy),
    )
}

/// Hex-encode bytes, limited to 10 characters (port of `buildString(ByteString)`).
pub fn build_string_bytes(bytes: &[u8]) -> String {
    limit(&base16::encode(bytes), 10)
}

/// Hex-encode with the first and last 10 bytes joined by `...` (port of `buildStringSig`).
pub fn build_string_sig(bytes: &[u8]) -> String {
    let first = base16::encode(&bytes[..bytes.len().min(10)]);
    let last = base16::encode(&bytes[bytes.len().saturating_sub(10)..]);
    format!("{first}...{last}")
}

/// Render a signed deploy (port of `buildString(Signed[DeployData])`).
pub fn build_string_signed_deploy(sd: &SignedDeployData) -> String {
    format!(
        "{}, Sig: {}, SigAlgorithm: {}, ValidAfterBlockNumber: {}",
        build_string_deploy(&sd.data),
        build_string_sig(&sd.sig),
        sd.sig_algorithm,
        sd.data.valid_after_block_number,
    )
}

/// Render a deploy (port of `buildString(DeployData)`).
pub fn build_string_deploy(d: &DeployData) -> String {
    format!("DeployData #{} -- {}", d.timestamp, d.term)
}

/// Render a list of block hashes (port of `buildString(Traversable[BlockHash])`).
pub fn build_string_hashes(hashes: &[BlockHash]) -> String {
    let inner = hashes
        .iter()
        .map(|h| build_string_bytes(h.as_bytes()))
        .collect::<Vec<_>>()
        .join(" ");
    format!("[{inner}]")
}

fn build_string_block(b: &BlockMessage, short: bool) -> String {
    let hash = build_string_bytes(b.block_hash.as_bytes());
    let sender = build_string_bytes(b.sender.as_bytes());
    if short {
        format!("#{} {} by {}", i64::from(b.block_number), hash, sender)
    } else {
        format!(
            "#{} {} sender: {}, state: {}, shard: {}, justifications: {}",
            i64::from(b.block_number),
            hash,
            sender,
            build_string_bytes(b.post_state_hash.as_bytes()),
            limit(&b.shard_id, 10),
            build_string_hashes(&b.justifications),
        )
    }
}

/// Truncate a string to `max_length` characters, appending `...` (port of `limit`).
fn limit(s: &str, max_length: usize) -> String {
    if s.chars().count() > max_length {
        let truncated: String = s.chars().take(max_length).collect();
        format!("{truncated}...")
    } else {
        s.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::casper::protocol::casper_message::{RholangState, StoreItemsMessage};
    use crate::validator::Validator;
    use std::collections::{BTreeMap, BTreeSet};

    fn sample_block() -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: BlockHash::from_slice(&[0xab; 32]),
            block_number: 42.try_into().unwrap(),
            sender: Validator::from_slice(&[0xcd; 65]),
            seq_num: 1.try_into().unwrap(),
            pre_state_hash: crate::block::state_hash::StateHash::new([0x11; 32]),
            post_state_hash: crate::block::state_hash::StateHash::new([0x22; 32]),
            justifications: vec![BlockHash::from_slice(&[0x33; 32])],
            bonds: BTreeMap::new(),
            rejected_deploys: BTreeSet::new(),
            rejected_blocks: BTreeSet::new(),
            rejected_senders: BTreeSet::new(),
            state: RholangState::default(),
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![0x44; 64],
        }
    }

    #[test]
    fn short_block_string() {
        let b = sample_block();
        let s = build_string_casper_message(&CasperMessage::BlockMessage(b), true);
        assert_eq!(s, "#42 ababababab... by cdcdcdcdcd...");
    }

    #[test]
    fn full_block_string() {
        let b = sample_block();
        let s = build_string_casper_message(&CasperMessage::BlockMessage(b), false);
        assert_eq!(
            s,
            "#42 ababababab... sender: cdcdcdcdcd..., state: 2222222222..., shard: root, justifications: [3333333333...]"
        );
    }

    #[test]
    fn unknown_message() {
        let msg = CasperMessage::StoreItemsMessage(StoreItemsMessage {
            start_path: vec![],
            last_path: vec![],
            history_items: vec![],
            data_items: vec![],
        });
        assert_eq!(
            build_string_casper_message(&msg, false),
            "Unknown consensus protocol message"
        );
    }

    #[test]
    fn deploy_string() {
        let d = DeployData {
            term: "new x in { x!(0) }".to_string(),
            timestamp: 1000,
            phlo_price: 1,
            phlo_limit: 100,
            valid_after_block_number: 5,
            shard_id: "root".to_string(),
        };
        assert_eq!(build_string_deploy(&d), "DeployData #1000 -- new x in { x!(0) }");
    }

    #[test]
    fn limit_truncates_long_strings() {
        assert_eq!(limit("abcdefghijklmnop", 10), "abcdefghij...");
        assert_eq!(limit("short", 10), "short");
    }

    #[test]
    fn sig_uses_first_and_last_10_bytes() {
        let bytes: Vec<u8> = (0..32).collect();
        assert_eq!(
            build_string_sig(&bytes),
            "00010203040506070809...161718191a1b1c1d1e1f"
        );
    }

    #[test]
    fn hashes_list_is_bracketed_and_truncated() {
        let hashes = vec![
            BlockHash::from_slice(&[0x01; 32]),
            BlockHash::from_slice(&[0x02; 32]),
        ];
        assert_eq!(build_string_hashes(&hashes), "[0101010101... 0202020202...]");
    }
}
