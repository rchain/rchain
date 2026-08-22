//! The `BlockApi` implementation (port of `BlockApiImpl.scala`).
//!
//! Read paths hit the block store / DAG; write paths (`deploy`, `createBlock`, `getProposeResult`)
//! drive propose through a caller-supplied trigger + the shared `ProposerState`.

use std::collections::BTreeSet;
use std::future::Future;
use std::pin::Pin;
use std::sync::Arc;

use async_trait::async_trait;

use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::{BlockDagStorage, DeployId};
use rchain_graphz::ListSerializer;
use rchain_models::ast::Par;
use rchain_models::block_hash::BlockHash;
use rchain_models::block_metadata::BlockMetadata;
use rchain_models::casper::protocol::casper_message::{BlockMessage, SignedDeployData};
use rchain_models::casper::protocol::deploy_service::{
    BlockInfo, ContinuationsWithBlockInfo, DataWithBlockInfo, DeployExecStatus, LightBlockInfo,
    Status, VersionInfo, WaitingContinuationInfo,
};
use rchain_models::rholang::RhoType::RhoDeployId;
use rchain_models::sorter::sort_par_term;
use rchain_models::validator::Validator;
use rchain_rspace::hashing::stable_hash_provider::{hash_channel, hash_channels, hash_seq};
use rchain_rspace::trace::event::Event as REvent;
use rchain_shared::base16;

use crate::api::block_api::{get_full_block_info, get_light_block_info, ApiErr, BlockApi};
use crate::api::graph_generator::{dag_as_cluster, ValidatorBlock};
use crate::api::machine_verifiable_dag::machine_verifiable_dag;
use crate::blocks::proposer::propose_result::{ProposeResult, ProposeStatus};
use crate::blocks::proposer::proposer::ProposerResult;
use crate::event_converter::to_rspace_event;
use crate::runtime_manager::RuntimeManager;
use crate::state::ProposerState;
use crate::validator_identity::ValidatorIdentity;

/// The network snapshot needed by `status` (address + peer/node counts; the Scala
/// `F[(PeerNode, Connections, Seq[PeerNode])]` collapses to its rendered form).
#[derive(Clone, Debug)]
pub struct NetworkStatus {
    pub address: String,
    pub peers: i32,
    pub nodes: i32,
}

/// A propose trigger (port of `ProposeFunction[F] = Boolean => F[ProposerResult]`).
pub type ProposeFunction =
    Box<dyn Fn(bool) -> Pin<Box<dyn Future<Output = ProposerResult> + Send + 'static>> + Send + Sync>;

/// The concrete `BlockApi` (port of `BlockApiImpl`).
pub struct BlockApiImpl {
    dag: Arc<dyn BlockDagStorage>,
    block_store: BlockStore,
    runtime: Arc<RuntimeManager>,
    validator_opt: Option<ValidatorIdentity>,
    network_id: String,
    shard_id: String,
    min_phlo_price: i64,
    version: String,
    network_status: Box<dyn Fn() -> NetworkStatus + Send + Sync>,
    is_node_read_only: bool,
    max_depth_limit: i32,
    dev_mode: bool,
    trigger_propose: Option<ProposeFunction>,
    proposer_state: Option<Arc<tokio::sync::Mutex<ProposerState>>>,
    auto_propose: bool,
    system_public_keys: BTreeSet<Vec<u8>>,
}

impl BlockApiImpl {
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        dag: Arc<dyn BlockDagStorage>,
        block_store: BlockStore,
        runtime: Arc<RuntimeManager>,
        validator_opt: Option<ValidatorIdentity>,
        network_id: String,
        shard_id: String,
        min_phlo_price: i64,
        version: String,
        network_status: Box<dyn Fn() -> NetworkStatus + Send + Sync>,
        is_node_read_only: bool,
        max_depth_limit: i32,
        dev_mode: bool,
        trigger_propose: Option<ProposeFunction>,
        proposer_state: Option<Arc<tokio::sync::Mutex<ProposerState>>>,
        auto_propose: bool,
        system_public_keys: BTreeSet<Vec<u8>>,
    ) -> Self {
        BlockApiImpl {
            dag,
            block_store,
            runtime,
            validator_opt,
            network_id,
            shard_id,
            min_phlo_price,
            version,
            network_status,
            is_node_read_only,
            max_depth_limit,
            dev_mode,
            trigger_propose,
            proposer_state,
            auto_propose,
            system_public_keys,
        }
    }

    async fn get_block_unsafe(&self, hash: &BlockHash) -> Result<BlockMessage, String> {
        self.block_store
            .get(&[*hash])
            .await?
            .pop()
            .flatten()
            .ok_or_else(|| format!("missing block {}", hash.to_hex()))
    }

    async fn get_data_at_par_raw(
        &self,
        par: &Par,
        block_hash: &BlockHash,
        use_pre_state_hash: bool,
    ) -> Result<(Vec<Par>, LightBlockInfo), String> {
        let block = self.get_block_unsafe(block_hash).await?;
        let sorted_par = sort_par_term(par);
        let state_hash = if use_pre_state_hash {
            block.pre_state_hash
        } else {
            block.post_state_hash
        };
        let data = self.runtime.get_data(&state_hash, &sorted_par).await?;
        let lbi = get_light_block_info(&block);
        Ok((data, lbi))
    }

    async fn get_data_with_block_info(
        &self,
        sorted_name: &Par,
        block: &BlockMessage,
    ) -> Result<Option<DataWithBlockInfo>, String> {
        if self.is_listening_name_reduced(block, &[sorted_name.clone()]) {
            let state_hash = block.post_state_hash;
            let data = self.runtime.get_data(&state_hash, sorted_name).await?;
            let block_info = get_light_block_info(block);
            Ok(Some(DataWithBlockInfo {
                post_block_data: data,
                block: block_info,
            }))
        } else {
            Ok(None)
        }
    }

    async fn get_continuations_with_block_info(
        &self,
        sorted_names: &[Par],
        block: &BlockMessage,
    ) -> Result<Option<ContinuationsWithBlockInfo>, String> {
        if self.is_listening_name_reduced(block, sorted_names) {
            let state_hash = block.post_state_hash;
            let continuations = self.runtime.get_continuation(&state_hash, sorted_names).await?;
            let continuation_infos = continuations
                .into_iter()
                .map(|(patterns, cont)| WaitingContinuationInfo {
                    post_block_patterns: patterns,
                    post_block_continuation: cont,
                })
                .collect();
            let block_info = get_light_block_info(block);
            Ok(Some(ContinuationsWithBlockInfo {
                post_block_continuations: continuation_infos,
                block: block_info,
            }))
        } else {
            Ok(None)
        }
    }

    fn is_listening_name_reduced(&self, block: &BlockMessage, sorted_names: &[Par]) -> bool {
        let event_log: Vec<REvent> = block
            .state
            .deploys
            .iter()
            .flat_map(|d| d.deploy_log.iter())
            .map(to_rspace_event)
            .collect();
        event_log.iter().any(|ev| match ev {
            REvent::Produce(p) => {
                // A produce has exactly one channel; a query name with a different arity simply
                // does not match (must not panic on a user-supplied `Par`).
                sorted_names.len() == 1 && p.channels_hash == hash_channel(&sorted_names[0])
            }
            REvent::Consume(c) => {
                let mut c_hashes = c.channels_hashes.clone();
                c_hashes.sort();
                c_hashes == hash_seq(sorted_names)
            }
            REvent::Comm(comm) => {
                let mut c_hashes = comm.consume.channels_hashes.clone();
                c_hashes.sort();
                c_hashes == hash_seq(sorted_names)
                    || comm
                        .produces
                        .iter()
                        .any(|p| p.channels_hash == hash_channels(sorted_names))
            }
        })
    }
}

#[async_trait]
impl BlockApi for BlockApiImpl {
    async fn status(&self) -> Status {
        let net = (self.network_status)();
        Status {
            version: VersionInfo {
                api: 1.to_string(),
                node: self.version.clone(),
            },
            address: net.address,
            network_id: self.network_id.clone(),
            shard_id: self.shard_id.clone(),
            peers: net.peers,
            nodes: net.nodes,
            min_phlo_price: self.min_phlo_price,
            latest_block_number: 0,
        }
    }

    async fn deploy(&self, deploy: &SignedDeployData) -> ApiErr<String> {
        if self.is_node_read_only {
            return Err("Deploy was rejected because node is running in read-only mode.".to_string());
        }
        if !deploy.verify_signature() {
            return Err("Deploy signature is invalid.".to_string());
        }
        if deploy.data.phlo_limit < 0 {
            return Err("Deploy phlo limit must be non-negative.".to_string());
        }
        if deploy.data.shard_id != self.shard_id {
            return Err(format!(
                "Deploy shardId '{}' is not as expected network shard '{}'.",
                deploy.data.shard_id, self.shard_id
            ));
        }
        if self.system_public_keys.contains(&deploy.deployer) {
            return Err("Deploy refused because it's signed with forbidden private key.".to_string());
        }
        if deploy.data.phlo_price < self.min_phlo_price {
            return Err(format!(
                "Phlo price {} is less than minimum price {}.",
                deploy.data.phlo_price, self.min_phlo_price
            ));
        }

        let r = match crate::multi_parent_casper::deploy(self.dag.as_ref(), deploy).await {
            Ok(id) => Ok(format!("Success!\nDeployId is: {}", base16::encode(&id))),
            Err(e) => Err(e.0),
        };

        // Auto-propose if enabled and a trigger is available.
        if self.auto_propose {
            if let Some(trigger) = &self.trigger_propose {
                let _ = trigger(true).await;
            }
        }
        r
    }

    async fn deploy_status(&self, deploy_id: &DeployId) -> ApiErr<DeployExecStatus> {
        if let Some(block_hash) = self.dag.lookup_by_deploy_id(deploy_id).await? {
            let block = self.get_block_unsafe(&block_hash).await?;
            let deploy_opt = block
                .state
                .deploys
                .iter()
                .find(|d| d.deploy.sig.as_slice() == deploy_id.as_slice());
            let deploy = deploy_opt.ok_or_else(|| {
                format!(
                    "Deploy not found in the block, blockHash: {}, deploy sig: {}",
                    block_hash.to_hex(),
                    base16::encode(deploy_id)
                )
            })?;
            if !deploy.is_failed {
                let deploy_id_ch = RhoDeployId::apply(deploy_id.clone());
                let (par, light_block) =
                    self.get_data_at_par_raw(&deploy_id_ch, &block_hash, false).await?;
                Ok(DeployExecStatus::ProcessedWithSuccess {
                    deploy_result: par,
                    block: light_block,
                })
            } else {
                // The execution tracker (for the precise error message) is deferred.
                let light_block = get_light_block_info(&block);
                Ok(DeployExecStatus::ProcessedWithError {
                    deploy_error:
                        "<deploy error message not available in cache or deploy executed on another node>"
                            .to_string(),
                    block: light_block,
                })
            }
        } else if self.dag.contains_deploy_in_pool(deploy_id).await? {
            Ok(DeployExecStatus::NotProcessed {
                status: "Pooled".to_string(),
            })
        } else {
            Ok(DeployExecStatus::NotProcessed {
                status: "Unknown".to_string(),
            })
        }
    }

    async fn create_block(&self, is_async: bool) -> ApiErr<String> {
        let trigger = self
            .trigger_propose
            .as_ref()
            .ok_or_else(|| "Propose error: read-only node.".to_string())?;
        let r = trigger(is_async).await;
        Ok(match r {
            ProposerResult::Empty => {
                return Err("Failure: another propose is in progress".to_string());
            }
            ProposerResult::Failure { status, seq_number } => {
                return Err(format!("Failure: {status} (seqNum {seq_number})"));
            }
            ProposerResult::Started { seq_number } => {
                format!("Propose started (seqNum {seq_number})")
            }
            ProposerResult::Success { block, .. } => {
                format!("Success! Block {} created and added.", block.block_hash.to_hex())
            }
        })
    }

    async fn get_propose_result(&self) -> ApiErr<String> {
        let state = self
            .proposer_state
            .as_ref()
            .ok_or_else(|| "Error: read-only node.".to_string())?;
        let mut s = state.lock().await;
        match s.curr_propose_result.take() {
            None => {
                let result = s.latest_propose_result.clone().unwrap_or((
                    ProposeResult {
                        propose_status: ProposeStatus::NotEnoughNewBlocks,
                    },
                    None,
                ));
                Ok(propose_result_message(result))
            }
            Some(rx) => {
                drop(s);
                let result = rx.await.map_err(|e| e.to_string())?;
                Ok(propose_result_message(result))
            }
        }
    }

    async fn get_listening_name_data_response(
        &self,
        depth: i32,
        listening_name: &Par,
    ) -> ApiErr<(Vec<DataWithBlockInfo>, i32)> {
        if depth > self.max_depth_limit {
            return Err(format!(
                "Your request on getListeningName depth {depth} exceed the max limit {}",
                self.max_depth_limit
            ));
        }
        let dag = self.dag.get_representation().await;
        let depth_with_limit = depth.min(self.max_depth_limit) as usize;
        let sorted = sort_par_term(listening_name);
        let mut out: Vec<DataWithBlockInfo> = Vec::new();
        let mut heights = 0usize;
        for (_h, hashes) in dag.height_map.iter().rev() {
            if heights >= depth_with_limit {
                break;
            }
            heights += 1;
            for h in hashes {
                let block = self.get_block_unsafe(h).await?;
                if let Some(d) = self.get_data_with_block_info(&sorted, &block).await? {
                    out.push(d);
                }
            }
        }
        let len = out.len() as i32;
        Ok((out, len))
    }

    async fn get_listening_name_continuation_response(
        &self,
        depth: i32,
        listening_names: &[Par],
    ) -> ApiErr<(Vec<ContinuationsWithBlockInfo>, i32)> {
        if depth > self.max_depth_limit {
            return Err(format!(
                "Your request on getListeningNameContinuation depth {depth} exceed the max limit {}",
                self.max_depth_limit
            ));
        }
        let dag = self.dag.get_representation().await;
        let depth_with_limit = depth.min(self.max_depth_limit) as usize;
        let sorted: Vec<Par> = listening_names.iter().map(sort_par_term).collect();
        let mut out: Vec<ContinuationsWithBlockInfo> = Vec::new();
        let mut heights = 0usize;
        for (_h, hashes) in dag.height_map.iter().rev() {
            if heights >= depth_with_limit {
                break;
            }
            heights += 1;
            for h in hashes {
                let block = self.get_block_unsafe(h).await?;
                if let Some(c) = self.get_continuations_with_block_info(&sorted, &block).await? {
                    out.push(c);
                }
            }
        }
        let len = out.len() as i32;
        Ok((out, len))
    }

    async fn get_blocks_by_heights(
        &self,
        start_block_number: i64,
        end_block_number: i64,
    ) -> ApiErr<Vec<LightBlockInfo>> {
        if end_block_number - start_block_number > self.max_depth_limit as i64 {
            return Err(format!(
                "Your request startBlockNumber {start_block_number} and endBlockNumber {end_block_number} exceed the max limit {}",
                self.max_depth_limit
            ));
        }
        let dag = self.dag.get_representation().await;
        let topo = dag
            .topo_sort_unsafe(start_block_number, Some(end_block_number))
            .map_err(|e| e.to_string())?;
        let mut out = Vec::new();
        for hashes in &topo {
            for h in hashes {
                let block = self.get_block_unsafe(h).await?;
                out.push(get_light_block_info(&block));
            }
        }
        Ok(out)
    }

    async fn visualize_dag(
        &self,
        depth: i32,
        start_block_number: i32,
        _show_justification_lines: bool,
    ) -> ApiErr<Vec<String>> {
        let dag = self.dag.get_representation().await;
        let start_block_num = if start_block_number == 0 {
            dag.latest_block_number()
        } else {
            start_block_number as i64
        };
        let depth_limited = if depth <= 0 || depth > self.max_depth_limit {
            self.max_depth_limit
        } else {
            depth
        };
        let lowest_height = start_block_num - depth_limited as i64;
        let to_hash_str = |bytes: &[u8]| base16::encode(bytes).chars().take(5).collect::<String>();

        let blocks: Vec<ValidatorBlock> = dag
            .dag_message_state
            .msg_map
            .values()
            .filter(|m| i64::from(m.height) >= lowest_height)
            .map(|m| ValidatorBlock {
                id: to_hash_str(m.id.as_bytes()),
                sender: to_hash_str(m.sender.as_bytes()),
                height: i64::from(m.height),
                justifications: m.parents.iter().map(|h| to_hash_str(h.as_bytes())).collect(),
                fringe: m.fringe.iter().map(|h| to_hash_str(h.as_bytes())).collect(),
            })
            .collect();

        let mut ser = ListSerializer::default();
        dag_as_cluster(&blocks, &mut ser);
        Ok(ser.buf)
    }

    async fn machine_verifiable_dag(&self, depth: i32) -> ApiErr<String> {
        if depth > self.max_depth_limit {
            return Err(format!(
                "Your request depth {depth} exceed the max limit {}",
                self.max_depth_limit
            ));
        }
        let dag = self.dag.get_representation().await;
        let start = dag.latest_block_number() - depth as i64;
        let topo = dag
            .topo_sort_unsafe(start, None)
            .map_err(|e| e.to_string())?;
        let edges = machine_verifiable_dag(&topo, |h| async move {
            let block = self.get_block_unsafe(&h).await?;
            Ok::<Vec<BlockHash>, String>(block.justifications)
        })
        .await?;
        Ok(edges.iter().map(|e| e.show()).collect::<Vec<_>>().join("\n"))
    }

    async fn get_blocks(&self, depth: i32) -> ApiErr<Vec<LightBlockInfo>> {
        if depth > self.max_depth_limit {
            return Err(format!(
                "Your request depth {depth} exceed the max limit {}",
                self.max_depth_limit
            ));
        }
        let dag = self.dag.get_representation().await;
        let start = dag.latest_block_number() - depth as i64;
        let topo = dag
            .topo_sort_unsafe(start, None)
            .map_err(|e| e.to_string())?;
        let mut acc: Vec<LightBlockInfo> = Vec::new();
        for hashes in &topo {
            let mut infos = Vec::new();
            for h in hashes {
                let block = self.get_block_unsafe(h).await?;
                infos.push(get_light_block_info(&block));
            }
            acc.extend(infos);
        }
        acc.reverse();
        Ok(acc)
    }

    async fn find_deploy(&self, id: &DeployId) -> ApiErr<LightBlockInfo> {
        match self.dag.lookup_by_deploy_id(id).await? {
            Some(h) => {
                let block = self.get_block_unsafe(&h).await?;
                Ok(get_light_block_info(&block))
            }
            None => Err(format!(
                "Couldn't find block containing deploy with id: {}",
                base16::encode(id)
            )),
        }
    }

    async fn get_block(&self, hash: &str) -> ApiErr<BlockInfo> {
        if hash.len() < 6 {
            return Err(format!("Input hash value must be at least 6 characters: {hash}"));
        }
        let hash_bytes = base16::decode(hash)
            .ok_or_else(|| format!("Input hash value is not valid hex string: {hash}"))?;
        let block_opt = if hash.len() == 64 {
            self.block_store
                .get(&[BlockHash::try_from(hash_bytes.as_slice())
                    .map_err(|e| e.to_string())?])
                .await?
                .pop()
                .flatten()
        } else {
            let dag = self.dag.get_representation().await;
            match dag.find(hash) {
                Some(h) => self
                    .block_store
                    .get(&[h])
                    .await?
                    .pop()
                    .flatten(),
                None => None,
            }
        };
        let block = block_opt.ok_or_else(|| format!("Error: Failure to find block with hash: {hash}"))?;
        let dag = self.dag.get_representation().await;
        if dag.contains(&block.block_hash) {
            Ok(get_full_block_info(&block))
        } else {
            Err(format!("Error: Block with hash {hash} received but not added yet"))
        }
    }

    async fn bond_status(&self, public_key: &[u8]) -> ApiErr<bool> {
        let dag = self.dag.get_representation().await;
        let hash = dag.last_finalized_block_unsafe()?;
        let block = self.get_block_unsafe(&hash).await?;
        let state_hash = block.post_state_hash;
        let bonds = self.runtime.compute_bonds(&state_hash).await?;
        Ok(bonds.contains_key(
            &Validator::try_from(public_key).map_err(|e| e.to_string())?,
        ))
    }

    async fn exploratory_deploy(
        &self,
        term: &str,
        block_hash: Option<&str>,
        use_pre_state_hash: bool,
    ) -> ApiErr<(Vec<Par>, LightBlockInfo)> {
        if !(self.is_node_read_only || self.dev_mode) {
            return Err(
                "Exploratory deploy can only be executed on read-only RNode.".to_string(),
            );
        }
        let dag = self.dag.get_representation().await;
        let target: Option<BlockMessage> = match block_hash {
            None => {
                let hash = dag.last_finalized_block_unsafe()?;
                self.block_store.get(&[hash]).await?.pop().flatten()
            }
            Some(h) => {
                let bytes = base16::decode(h)
                    .ok_or_else(|| format!("Input hash value is not valid hex string: {h}"))?;
                self.block_store
                    .get(&[BlockHash::try_from(bytes.as_slice())
                        .map_err(|e| e.to_string())?])
                    .await?
                    .pop()
                    .flatten()
            }
        };
        match target {
            None => Err(format!("Can not find block {block_hash:?}")),
            Some(b) => {
                let state_hash = if use_pre_state_hash {
                    b.pre_state_hash
                } else {
                    b.post_state_hash
                };
                let res = self.runtime.play_exploratory_deploy(term, &state_hash).await?;
                let lbi = get_light_block_info(&b);
                Ok((res, lbi))
            }
        }
    }

    async fn get_data_at_par(
        &self,
        par: &Par,
        block_hash: &str,
        use_pre_state_hash: bool,
    ) -> ApiErr<(Vec<Par>, LightBlockInfo)> {
        let bytes = base16::decode(block_hash)
            .ok_or_else(|| format!("Invalid block hash base 16 encoding, {block_hash}"))?;
        let hash = BlockHash::try_from(bytes.as_slice()).map_err(|e| e.to_string())?;
        self.get_data_at_par_raw(par, &hash, use_pre_state_hash).await
    }

    async fn last_finalized_block(&self) -> ApiErr<BlockInfo> {
        let dag = self.dag.get_representation().await;
        let hash = dag.last_finalized_block_unsafe()?;
        let block = self.get_block_unsafe(&hash).await?;
        Ok(get_full_block_info(&block))
    }

    async fn is_finalized(&self, hash: &str) -> ApiErr<bool> {
        let dag = self.dag.get_representation().await;
        let given =
            BlockHash::try_from(base16::try_decode(hash)?.as_slice()).map_err(|e| e.to_string())?;
        Ok(dag.is_finalized(&given))
    }

    async fn get_latest_message(&self) -> ApiErr<BlockMetadata> {
        let validator = self
            .validator_opt
            .as_ref()
            .ok_or_else(|| "Validator not available (read-only node).".to_string())?;
        let validator_bytes = validator.public_key.bytes();
        let dag = self.dag.get_representation().await;
        let latest_id = dag
            .dag_message_state
            .latest_msgs
            .get(&Validator::from_slice(validator_bytes))
            .map(|m| m.id);
        match latest_id {
            Some(id) => self
                .dag
                .lookup(&id)
                .await?
                .ok_or_else(|| "No block message for latest message.".to_string()),
            None => Err("No block message for validator.".to_string()),
        }
    }
}

fn propose_result_message(result: (ProposeResult, Option<BlockMessage>)) -> String {
    match result.1 {
        Some(block) => format!(
            "Success! Block {} created and added.",
            block.block_hash.to_hex()
        ),
        None => result.0.propose_status.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_models::casper::protocol::casper_message::RholangState;

    fn block() -> BlockMessage {
        BlockMessage {
            version: 1,
            shard_id: "root".to_string(),
            block_hash: BlockHash::new([1u8; 32]),
            block_number: 0.try_into().unwrap(),
            sender: Validator::new([0u8; 65]),
            seq_num: 0.try_into().unwrap(),
            pre_state_hash: rchain_models::block::state_hash::StateHash::new([0u8; 32]),
            post_state_hash: rchain_models::block::state_hash::StateHash::new([0u8; 32]),
            justifications: vec![],
            bonds: std::collections::BTreeMap::new(),
            rejected_deploys: std::collections::BTreeSet::new(),
            rejected_blocks: std::collections::BTreeSet::new(),
            rejected_senders: std::collections::BTreeSet::new(),
            state: RholangState::default(),
            sig_algorithm: "secp256k1".to_string(),
            sig: vec![],
        }
    }

    #[test]
    fn propose_result_message_renders_success_and_failure() {
        let ok = propose_result_message((
            ProposeResult {
                propose_status: ProposeStatus::ProposeSuccess,
            },
            Some(block()),
        ));
        assert!(ok.starts_with("Success! Block "));

        let err = propose_result_message((
            ProposeResult {
                propose_status: ProposeStatus::NoNewDeploys,
            },
            None,
        ));
        assert_eq!(err, "Proposal failed: NoNewDeploys");
    }
}
