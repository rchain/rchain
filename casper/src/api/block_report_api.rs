//! Block replay reporting API (port of `BlockReportApi.scala`).
//!
//! Replays a block through `ReportingCasper` and caches the resulting `BlockEventInfo` in a
//! `ReportStore`, guarding concurrent replays of the same block with a per-hash lock. Only
//! read-only nodes (no validator identity) may produce reports.

use std::collections::BTreeMap;
use std::sync::Arc;

use rchain_block_storage::block_store::BlockStore;
use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::BlockMessage;
use rchain_models::casper::protocol::report::{
    BlockEventInfo, DeployInfoWithEventData, SingleReport, SystemDeployInfoWithEventData,
};
use rchain_rspace::reporting_transformer::ReportingTransformer;
use rchain_shared::typed_store::KeyValueTypedStore;

use crate::api::block_api::{get_light_block_info, ApiErr};
use crate::reporting::{
    DeployReportResult, ReportingCasper, ReportingProtoTransformer, SystemDeployReportResult,
};
use crate::validator_identity::ValidatorIdentity;

/// A report store: `BlockHash → BlockEventInfo` (port of `ReportStore`).
pub type ReportStore = Arc<dyn KeyValueTypedStore<BlockHash, BlockEventInfo>>;

/// The block report API (port of `BlockReportApi`).
pub struct BlockReportApi {
    block_store: BlockStore,
    reporting_casper: Arc<dyn ReportingCasper>,
    report_store: ReportStore,
    validator_identity_opt: Option<ValidatorIdentity>,
    report_transformer: ReportingProtoTransformer,
    block_lock_map: tokio::sync::Mutex<BTreeMap<BlockHash, Arc<tokio::sync::Mutex<()>>>>,
}

impl BlockReportApi {
    pub fn new(
        block_store: BlockStore,
        reporting_casper: Arc<dyn ReportingCasper>,
        report_store: ReportStore,
        validator_identity_opt: Option<ValidatorIdentity>,
    ) -> Self {
        BlockReportApi {
            block_store,
            reporting_casper,
            report_store,
            validator_identity_opt,
            report_transformer: ReportingProtoTransformer,
            block_lock_map: tokio::sync::Mutex::new(BTreeMap::new()),
        }
    }

    /// Replay + report a block (port of `blockReport`). Rejects non-read-only nodes.
    pub async fn block_report(
        &self,
        hash: &BlockHash,
        force_replay: bool,
    ) -> ApiErr<BlockEventInfo> {
        if self.validator_identity_opt.is_some() {
            return Err("Block report can only be executed on read-only RNode.".to_string());
        }
        let maybe_block = self
            .block_store
            .get(&[*hash])
            .await
            .map_err(|e| e)?
            .pop()
            .flatten();
        match maybe_block {
            Some(block) => self.block_report_within_lock(force_replay, &block).await,
            None => Err(format!("Block {} not found", hash.to_hex())),
        }
    }

    async fn block_report_within_lock(
        &self,
        force_replay: bool,
        block: &BlockMessage,
    ) -> Result<BlockEventInfo, String> {
        let lock = {
            let mut map = self.block_lock_map.lock().await;
            map.entry(block.block_hash)
                .or_insert_with(|| Arc::new(tokio::sync::Mutex::new(())))
                .clone()
        };
        let _guard = lock.lock().await;

        let cached = self
            .report_store
            .get(&[block.block_hash])
            .await?
            .pop()
            .flatten();
        match cached {
            Some(ev) if !force_replay => Ok(ev),
            _ => {
                let ev = self.replay_block(block).await?;
                self.report_store
                    .put(&[(block.block_hash, ev.clone())])
                    .await?;
                Ok(ev)
            }
        }
    }

    async fn replay_block(&self, block: &BlockMessage) -> Result<BlockEventInfo, String> {
        let report_result = self.reporting_casper.trace(block.clone()).await?;
        let light_block = get_light_block_info(block);
        let deploys = create_deploy_report(&report_result.deploy_report_result, &self.report_transformer);
        let system_deploys =
            create_system_deploy_report(&report_result.system_deploy_report_result, &self.report_transformer);
        Ok(BlockEventInfo {
            block_info: light_block,
            deploys,
            system_deploys,
            post_state_hash: report_result.post_state_hash,
        })
    }
}

fn create_deploy_report(
    results: &[DeployReportResult],
    transformer: &ReportingProtoTransformer,
) -> Vec<DeployInfoWithEventData> {
    results
        .iter()
        .map(|p| DeployInfoWithEventData {
            deploy_info: p.processed_deploy.to_deploy_info(),
            report: p
                .events
                .iter()
                .map(|a| SingleReport {
                    events: a.iter().map(|e| transformer.transform_event(e)).collect(),
                })
                .collect(),
        })
        .collect()
}

fn create_system_deploy_report(
    results: &[SystemDeployReportResult],
    transformer: &ReportingProtoTransformer,
) -> Vec<SystemDeployInfoWithEventData> {
    results
        .iter()
        .map(|sd| SystemDeployInfoWithEventData {
            system_deploy: sd.processed_system_deploy.clone(),
            report: sd
                .events
                .iter()
                .map(|a| SingleReport {
                    events: a.iter().map(|e| transformer.transform_event(e)).collect(),
                })
                .collect(),
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
    use rchain_models::casper::protocol::casper_message::{
        DeployData, PCost, ProcessedDeploy, SignedDeployData,
    };
    use rchain_models::casper::protocol::report::ReportProto;
    use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
    use rchain_models::sorted::SortedProc;
    use rchain_rspace::reporting_rspace::{ReportingEvent, ReportingProduce};

    fn produce_event() -> crate::reporting::RhoReportingEvent {
        ReportingEvent::Produce(ReportingProduce {
            channel: SortedProc::default(),
            data: ListParWithRandom {
                pars: vec![],
                random_state: Blake2b512Random::default_random(),
            },
        })
    }

    fn processed_deploy() -> ProcessedDeploy {
        ProcessedDeploy {
            deploy: SignedDeployData {
                data: DeployData {
                    term: "Nil".to_string(),
                    timestamp: 0,
                    phlo_price: 1,
                    phlo_limit: 1,
                    valid_after_block_number: 0,
                    shard_id: "root".to_string(),
                },
                deployer: vec![1],
                sig: vec![2],
                sig_algorithm: "secp256k1".to_string(),
            },
            cost: PCost { cost: 0 },
            deploy_log: vec![],
            is_failed: false,
            system_deploy_error: None,
        }
    }

    #[test]
    fn create_deploy_report_builds_info_and_events() {
        let transformer = ReportingProtoTransformer;
        let result = DeployReportResult {
            processed_deploy: processed_deploy(),
            events: vec![vec![produce_event()]],
        };
        let reports = create_deploy_report(&[result], &transformer);

        assert_eq!(reports.len(), 1);
        assert_eq!(reports[0].deploy_info.term, "Nil");
        assert_eq!(reports[0].report.len(), 1);
        assert_eq!(reports[0].report[0].events.len(), 1);
        assert!(matches!(reports[0].report[0].events[0], ReportProto::Produce(_)));
    }

    #[test]
    fn transformer_maps_consume_peeks() {
        let transformer = ReportingProtoTransformer;
        let event = ReportingEvent::Consume(rchain_rspace::reporting_rspace::ReportingConsume {
            channels: vec![],
            patterns: vec![BindPattern {
                patterns: vec![],
                remainder: None,
                free_count: 0,
            }],
            continuation: TaggedContinuation::Empty,
            peeks: vec![0, 1],
        });
        match transformer.transform_event(&event) {
            ReportProto::Consume(c) => assert_eq!(c.peeks.len(), 2),
            other => panic!("expected Consume, got {other:?}"),
        }
    }
}
