//! Genesis block creation (port of `casper/genesis/Genesis.scala`).

pub mod contracts;
pub mod standard_deploys;

use std::collections::{BTreeMap, BTreeSet};

use rchain_crypto::public_key::PublicKey;
use rchain_models::block::state_hash::StateHash;
use rchain_models::block_version::CURRENT;
use rchain_models::casper::protocol::casper_message::{
    BlockMessage, ProcessedDeploy, RholangState, SignedDeployData,
};
use rchain_models::validator::Validator as ModelsValidator;
use rchain_rholang::system_processes::BlockData;
use rchain_shared::refined::NonNegI64;

use crate::block_random_seed::BlockRandomSeed;
use crate::genesis::contracts::{ProofOfStake, Registry, Vault};
use crate::proto_util::unsigned_block_proto;
use rchain_shared::refined::{BlockHeight, SeqNum};
use crate::runtime_manager::RuntimeManager;
use crate::validator_identity::ValidatorIdentity;

/// Genesis parameters (port of `Genesis`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Genesis {
    pub sender: PublicKey,
    pub shard_id: String,
    pub block_number: i64,
    pub proof_of_stake: ProofOfStake,
    pub registry: Registry,
    pub vaults: Vec<Vault>,
}

/// Build the bonds map (validator pubkey → stake) from the PoS validators (port of `buildBondsMap`).
fn build_bonds_map(proof_of_stake: &ProofOfStake) -> BTreeMap<ModelsValidator, NonNegI64> {
    proof_of_stake
        .validators
        .iter()
        .map(|v| (ModelsValidator::from_slice(v.pk.bytes()), v.stake))
        .collect()
}

/// Build the unsigned genesis block from processed deploys (port of
/// `createBlockWithProcessedDeploys`).
fn create_block_with_processed_deploys(
    genesis: &Genesis,
    pre_state_hash: StateHash,
    post_state_hash: StateHash,
    processed_deploys: Vec<ProcessedDeploy>,
) -> Result<BlockMessage, String> {
    if let Some(failed) = processed_deploys.iter().find(|d| d.is_failed) {
        return Err(format!(
            "Genesis block contains a failed deploy (deployer {:?})",
            failed.deploy.deployer
        ));
    }
    let state = RholangState {
        deploys: processed_deploys,
        system_deploys: Vec::new(),
    };
    Ok(unsigned_block_proto(
        CURRENT,
        genesis.shard_id.clone(),
        BlockHeight::try_from(genesis.block_number).map_err(|e| e.to_string())?,
        ModelsValidator::from_slice(genesis.sender.bytes()),
        SeqNum::zero(),
        pre_state_hash,
        post_state_hash,
        Vec::new(),
        build_bonds_map(&genesis.proof_of_stake),
        BTreeSet::new(),
        state,
    ))
}

/// The ordered list of blessed (standard) genesis deploys (port of `defaultBlessedTerms`).
///
/// Rust-first: the registry/PoS/vault system contracts are now **native** (`rholang::native_state`
/// + `system_deploy::NativeSystemDeployOp`); the blessed `.rho`/`.rhox` sources are a checklist only.
/// Genesis therefore installs no rholang system contracts.
pub fn default_blessed_terms(
    _proof_of_stake: &ProofOfStake,
    _registry: &Registry,
    _vaults: &[Vault],
    _shard_id: &str,
) -> Result<Vec<SignedDeployData>, String> {
    Ok(Vec::new())
}

/// Create the signed genesis block (port of `Genesis.createGenesisBlock`).
pub async fn create_genesis_block(
    validator: &ValidatorIdentity,
    genesis: &Genesis,
    runtime: &RuntimeManager,
) -> Result<BlockMessage, String> {
    let blessed_terms = default_blessed_terms(
        &genesis.proof_of_stake,
        &genesis.registry,
        &genesis.vaults,
        &genesis.shard_id,
    )?;
    let block_data = BlockData {
        block_number: BlockHeight::try_from(genesis.block_number).map_err(|e| e.to_string())?,
        sender: genesis.sender.clone(),
        seq_num: SeqNum::zero(),
    };
    let rand = BlockRandomSeed::random_generator_from_shard_id(&genesis.shard_id);
    let bonds = build_bonds_map(&genesis.proof_of_stake);
    let (start_hash, state_hash, processed_results) = runtime
        .compute_genesis(&blessed_terms, &rand, block_data, &bonds)
        .await?;
    // Surface deploy evaluation errors (the Scala `require` only checks the `isFailed` flag; the
    // underlying errors are otherwise lost, making genesis failures opaque).
    for (i, r) in processed_results.iter().enumerate() {
        if !r.eval_result.errors.is_empty() {
            return Err(format!(
                "Genesis deploy #{i} failed: {:?}",
                r.eval_result.errors
            ));
        }
    }
    let processed_deploys: Vec<ProcessedDeploy> =
        processed_results.into_iter().map(|r| r.deploy).collect();

    let unsigned_block = create_block_with_processed_deploys(
        genesis,
        start_hash.into(),
        state_hash.into(),
        processed_deploys,
    )?;
    let signed_block = validator.sign_block(&unsigned_block).map_err(|e| e.to_string())?;

    // Signing must not change the block hash.
    if unsigned_block.block_hash != signed_block.block_hash {
        return Err("Signed block has different block hash than unsigned".to_string());
    }
    Ok(signed_block)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::genesis::contracts::Validator;

    fn pos() -> ProofOfStake {
        ProofOfStake {
            minimum_bond: 1,
            maximum_bond: 100,
            validators: vec![
                Validator {
                    pk: PublicKey::new(vec![1; 65]),
                    stake: 10.try_into().unwrap(),
                },
                Validator {
                    pk: PublicKey::new(vec![2; 65]),
                    stake: 20.try_into().unwrap(),
                },
            ],
            epoch_length: 0,
            quarantine_length: 0,
            number_of_active_validators: 0,
            pos_multi_sig_public_keys: vec![],
            pos_multi_sig_quorum: 0,
            pos_vault_pub_key: String::new(),
        }
    }

    #[test]
    fn build_bonds_map_extracts_stakes() {
        let bonds = build_bonds_map(&pos());
        assert_eq!(bonds.len(), 2);
        assert_eq!(i64::from(bonds[&ModelsValidator::from_slice(&[1; 65])]), 10);
        assert_eq!(i64::from(bonds[&ModelsValidator::from_slice(&[2; 65])]), 20);
    }
}
