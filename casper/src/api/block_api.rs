//! Block API surface (port of `BlockApi.scala`): the `BlockApi` trait plus the block-info
//! constructors shared with `BlockApiImpl`.

use async_trait::async_trait;

use rchain_block_storage::dag::dag_storage::DeployId;
use rchain_models::ast::Par;
use rchain_shared::refined::NonNegI64;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{BlockMessage, SignedDeployData};
use rchain_models::casper::protocol::deploy_service::{
    BlockInfo, BondInfo, ContinuationsWithBlockInfo, DataWithBlockInfo, DeployExecStatus,
    LightBlockInfo, Status,
};
use rchain_models::validator::Validator;
use rchain_shared::base16;

/// A block-api error (the Scala `BlockApi.Error = String`).
pub type ApiErr<A> = Result<A, String>;

/// The block API (port of `BlockApi[F]`). Implementations read from the block store/DAG and drive
/// propose via the runtime.
#[async_trait]
pub trait BlockApi: Send + Sync {
    async fn status(&self) -> Status;

    async fn deploy(&self, deploy: &SignedDeployData) -> ApiErr<String>;

    async fn deploy_status(&self, deploy_id: &DeployId) -> ApiErr<DeployExecStatus>;

    async fn create_block(&self, is_async: bool) -> ApiErr<String>;

    async fn get_propose_result(&self) -> ApiErr<String>;

    async fn get_listening_name_data_response(
        &self,
        depth: i32,
        listening_name: &Par,
    ) -> ApiErr<(Vec<DataWithBlockInfo>, i32)>;

    async fn get_listening_name_continuation_response(
        &self,
        depth: i32,
        listening_names: &[Par],
    ) -> ApiErr<(Vec<ContinuationsWithBlockInfo>, i32)>;

    async fn get_blocks_by_heights(
        &self,
        start_block_number: i64,
        end_block_number: i64,
    ) -> ApiErr<Vec<LightBlockInfo>>;

    async fn visualize_dag(
        &self,
        depth: i32,
        start_block_number: i32,
        show_justification_lines: bool,
    ) -> ApiErr<Vec<String>>;

    async fn machine_verifiable_dag(&self, depth: i32) -> ApiErr<String>;

    async fn get_blocks(&self, depth: i32) -> ApiErr<Vec<LightBlockInfo>>;

    async fn find_deploy(&self, id: &DeployId) -> ApiErr<LightBlockInfo>;

    async fn get_block(&self, hash: &str) -> ApiErr<BlockInfo>;

    async fn bond_status(&self, public_key: &[u8]) -> ApiErr<bool>;

    async fn exploratory_deploy(
        &self,
        term: &str,
        block_hash: Option<&str>,
        use_pre_state_hash: bool,
    ) -> ApiErr<(Vec<Par>, LightBlockInfo)>;

    async fn get_data_at_par(
        &self,
        par: &Par,
        block_hash: &str,
        use_pre_state_hash: bool,
    ) -> ApiErr<(Vec<Par>, LightBlockInfo)>;

    async fn last_finalized_block(&self) -> ApiErr<BlockInfo>;

    async fn is_finalized(&self, hash: &str) -> ApiErr<bool>;

    async fn get_latest_message(&self) -> ApiErr<BlockMetadata>;
}

/// Build a bond info (port of `bondToBondInfo`).
pub fn bond_to_bond_info(bond: (&Validator, NonNegI64)) -> BondInfo {
    BondInfo {
        validator: base16::encode(bond.0.as_bytes()),
        stake: i64::from(bond.1),
    }
}

/// Build the full block info (port of `getFullBlockInfo`).
pub fn get_full_block_info(block: &BlockMessage) -> BlockInfo {
    construct_block_info(block)
}

/// Build the light block info (port of `getLightBlockInfo`).
pub fn get_light_block_info(block: &BlockMessage) -> LightBlockInfo {
    construct_light_block_info(block)
}

fn construct_block_info(block: &BlockMessage) -> BlockInfo {
    let light_block_info = construct_light_block_info(block);
    let deploys = block
        .state
        .deploys
        .iter()
        .map(|d| d.to_deploy_info())
        .collect();
    BlockInfo {
        block_info: light_block_info,
        deploys,
    }
}

fn construct_light_block_info(block: &BlockMessage) -> LightBlockInfo {
    LightBlockInfo {
        version: block.version,
        shard_id: block.shard_id.clone(),
        block_hash: base16::encode(block.block_hash.as_bytes()),
        block_number: i64::from(block.block_number),
        sender: base16::encode(block.sender.as_bytes()),
        seq_num: i64::from(block.seq_num),
        pre_state_hash: base16::encode(block.pre_state_hash.as_bytes()),
        post_state_hash: base16::encode(block.post_state_hash.as_bytes()),
        justifications: block
            .justifications
            .iter()
            .map(|h| base16::encode(h.as_bytes()))
            .collect(),
        bonds: block
            .bonds
            .iter()
            .map(|(v, s)| bond_to_bond_info((v, *s)))
            .collect(),
        sig_algorithm: block.sig_algorithm.clone(),
        sig: base16::encode(&block.sig),
        block_size: block.to_bytes().len().to_string(),
        deploy_count: block.state.deploys.len() as i32,
        rejected_deploys: block
            .rejected_deploys
            .iter()
            .map(|d| base16::encode(d))
            .collect(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_models::block_hash::BlockHash;
    use rchain_models::casper::protocol::casper_message::RholangState;
    use std::collections::{BTreeMap, BTreeSet};

    fn block() -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: BlockHash::new([1u8; 32]),
            block_number: 5.try_into().unwrap(),
            sender: Validator::new([2u8; 65]),
            seq_num: 3.try_into().unwrap(),
            pre_state_hash: rchain_models::block::state_hash::StateHash::new([0xab; 32]),
            post_state_hash: rchain_models::block::state_hash::StateHash::new([0xcd; 32]),
            justifications: vec![BlockHash::new([9u8; 32])],
            bonds: BTreeMap::from([(Validator::new([2u8; 65]), 100.try_into().unwrap())]),
            rejected_deploys: BTreeSet::new(),
            rejected_blocks: BTreeSet::new(),
            rejected_senders: BTreeSet::new(),
            state: RholangState::default(),
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![0xee],
        }
    }

    #[test]
    fn light_block_info_renders_hashes_as_hex() {
        let info = get_light_block_info(&block());
        assert_eq!(info.block_number, 5);
        assert_eq!(info.seq_num, 3);
        assert_eq!(info.deploy_count, 0);
        assert_eq!(info.bonds.len(), 1);
        assert_eq!(info.bonds[0].stake, 100);
        assert!(info.block_hash.starts_with("0101"));
    }

    #[test]
    fn full_block_info_carries_deploys() {
        let info = get_full_block_info(&block());
        assert_eq!(info.block_info.block_number, 5);
        assert!(info.deploys.is_empty());
    }
}
