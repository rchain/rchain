//! Block processing (port of `blocks/BlockProcessor.scala`).

use std::sync::Arc;

use rchain_block_storage::block_store::BlockStore;
use rchain_block_storage::dag::dag_storage::BlockDagStorage;
use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::BlockMessage;
use rchain_shared::log::{Log, LogSource};
use tokio::sync::mpsc;

use crate::block_status::BlockStatus;
use crate::merging::BlockIndex;
use crate::multi_parent_casper::ValidateError;
use crate::protocol::comm_util::CommUtil;
use crate::runtime_manager::RuntimeManager;

/// Validate a block and insert it into the DAG (port of `validateAndAddToDag`).
pub async fn validate_and_add_to_dag<F, Fut>(
    dag: &dyn BlockDagStorage,
    block_store: &BlockStore,
    runtime: &RuntimeManager,
    block: BlockMessage,
    shard_id: &str,
    min_phlo_price: i64,
    block_index: &F,
) -> Result<Result<(), BlockStatus>, String>
where
    F: Fn(BlockHash) -> Fut,
    Fut: std::future::Future<Output = Result<BlockIndex, String>>,
{
    let result = crate::multi_parent_casper::validate(
        dag,
        block_store,
        runtime,
        &block,
        shard_id,
        min_phlo_price,
        block_index,
    )
    .await;
    let (block_meta, status) = match result {
        Ok(meta) => (meta, Ok(())),
        Err(ValidateError::ValidationFailed(meta, status)) => (meta, Err(status)),
        Err(ValidateError::Internal(e)) => return Err(e),
    };
    dag.insert(block_meta, block).await?;
    Ok(status)
}

/// Process incoming blocks: validate, add to the DAG, notify the validated queue, and broadcast the
/// block hash (port of `BlockProcessor.apply`).
pub async fn apply<F, Fut>(
    mut input_blocks: mpsc::UnboundedReceiver<BlockMessage>,
    validated_tx: mpsc::UnboundedSender<BlockMessage>,
    shard_id: String,
    min_phlo_price: i64,
    dag: Arc<dyn BlockDagStorage>,
    block_store: BlockStore,
    runtime: Arc<RuntimeManager>,
    comm_util: Arc<CommUtil>,
    block_index: F,
    log: Arc<dyn Log>,
) where
    F: Fn(BlockHash) -> Fut,
    Fut: std::future::Future<Output = Result<BlockIndex, String>>,
{
    let source = LogSource::new("casper.blocks.BlockProcessor");
    while let Some(block) = input_blocks.recv().await {
        let result = validate_and_add_to_dag(
            dag.as_ref(),
            &block_store,
            runtime.as_ref(),
            block.clone(),
            &shard_id,
            min_phlo_price,
            &block_index,
        )
        .await;
        match result {
            Ok(Err(status)) => log.warn(
                source,
                &format!(
                    "Block {} failed validation: {status:?}",
                    block.block_hash.to_hex()
                ),
            ),
            Err(err) => log.error(
                source,
                &format!("Block {} processing error: {err}", block.block_hash.to_hex()),
            ),
            Ok(Ok(())) => {}
        }
        let _ = validated_tx.send(block.clone());
        comm_util
            .send_block_hash(&block.block_hash, block.sender.as_bytes())
            .await;
    }
}
