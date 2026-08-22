//! Standard (blessed) genesis deploys (port of `genesis/contracts/StandardDeploys.scala`).
//!
//! The `.rho`/`.rhox` contract sources are embedded via `include_str!`; `load_source`/`load_template`
//! reproduce `CompiledRholangSource.loadSource` / `CompiledRholangTemplate.loadTemplate` (the
//! `//Loaded from resource file <<path>>` comment + `$$name$$` macro substitution).

use rchain_crypto::private_key::PrivateKey;
use rchain_crypto::public_key::PublicKey;
use rchain_crypto::signatures::secp256k1::Secp256k1;
use rchain_crypto::signatures::signatures_alg::SignaturesAlg;
use rchain_crypto::signatures::signed::Signed;
use rchain_models::casper::protocol::casper_message::{DeployData, SignedDeployData};
use rchain_shared::base16;

use crate::genesis::contracts::{rev_generator_code, ProofOfStake, Registry, Vault};

// -------------------------------------------------------------------------------------------------
// Embedded contract sources
// -------------------------------------------------------------------------------------------------

const REGISTRY_RHO: &str = include_str!("resources/Registry.rho");
const LIST_OPS_RHO: &str = include_str!("resources/ListOps.rho");
const EITHER_RHO: &str = include_str!("resources/Either.rho");
const NON_NEGATIVE_NUMBER_RHO: &str = include_str!("resources/NonNegativeNumber.rho");
const MAKE_MINT_RHO: &str = include_str!("resources/MakeMint.rho");
const AUTH_KEY_RHO: &str = include_str!("resources/AuthKey.rho");
const REV_VAULT_RHO: &str = include_str!("resources/RevVault.rho");
const MULTI_SIG_REV_VAULT_RHO: &str = include_str!("resources/MultiSigRevVault.rho");
const POS_RHOX: &str = include_str!("resources/Pos.rhox");

// -------------------------------------------------------------------------------------------------
// Fixed blessed-contract keys + timestamps
// -------------------------------------------------------------------------------------------------

const REGISTRY_PK: &str = "5a0bde2f5857124b1379c78535b07a278e3b9cefbcacc02e62ab3294c02765a1";
const LIST_OPS_PK: &str = "867c21c6a3245865444d80e49cac08a1c11e23b35965b566bbe9f49bb9897511";
const EITHER_PK: &str = "5248f8913f8572d8227a3c7787b54bd8263389f7209adc1422e36bb2beb160dc";
const NON_NEGATIVE_NUMBER_PK: &str = "e33c9f1e925819d04733db4ec8539a84507c9e9abd32822059349449fe03997d";
const MAKE_MINT_PK: &str = "de19d53f28d4cdee74bad062342d8486a90a652055f3de4b2efa5eb2fccc9d53";
const AUTH_KEY_PK: &str = "f450b26bac63e5dd9343cd46f5fae1986d367a893cd21eedd98a4cb3ac699abc";
const REV_VAULT_PK: &str = "27e5718bf55dd673cc09f13c2bcf12ed7949b178aef5dcb6cd492ad422d05e9d";
const MULTI_SIG_REV_VAULT_PK: &str = "2a2eaa76d6fea9f502629e32b0f8eea19b9de8e2188ec0d589fcafa98fb1f031";
const POS_GENERATOR_PK: &str = "a9585a0687761139ab3587a4938fb5ab9fcba675c79fefba889859674046d4a5";
const REV_GENERATOR_PK: &str = "a06959868e39bb3a8502846686a23119716ecd001700baf9e2ecfa0dbf1a3247";

const REGISTRY_TIMESTAMP: i64 = 1559156071321;
const LIST_OPS_TIMESTAMP: i64 = 1559156082324;
const EITHER_TIMESTAMP: i64 = 1559156217509;
const NON_NEGATIVE_NUMBER_TIMESTAMP: i64 = 1559156251792;
const MAKE_MINT_TIMESTAMP: i64 = 1559156452968;
const AUTH_KEY_TIMESTAMP: i64 = 1559156356769;
const REV_VAULT_TIMESTAMP: i64 = 1559156183943;
const MULTI_SIG_REV_VAULT_TIMESTAMP: i64 = 1571408470880;
const POS_GENERATOR_TIMESTAMP: i64 = 1559156420651;
// `revGenerator` has no fixed timestamp — it is batched with `1565818101792 + idx`.

/// `accounting.MAX_VALUE` (`Integer.MAX_VALUE`).
const MAX_VALUE: i64 = i32::MAX as i64;

// -------------------------------------------------------------------------------------------------
// Source/template loading
// -------------------------------------------------------------------------------------------------

/// Append the resource-comment (port of `CompiledRholangSource.loadSource`).
fn load_source(classpath: &str, content: &str) -> String {
    format!("{content}\n//Loaded from resource file <<{classpath}>>\n")
}

/// Substitute `$$name$$` macros then append the resource-comment (port of
/// `CompiledRholangTemplate.loadTemplate`).
fn load_template(classpath: &str, content: &str, macros: &[(&str, &str)]) -> String {
    let mut final_content = content.to_string();
    for (name, value) in macros {
        final_content = final_content.replace(&format!("$${name}$$"), value);
    }
    format!("{final_content}\n //Loaded from resource file <<{classpath}>>\n")
}

fn to_public(private_key_hex: &str) -> Result<PublicKey, String> {
    let private_key = PrivateKey::new(base16::unsafe_decode(private_key_hex));
    Secp256k1.to_public(&private_key).map_err(|e| e.to_string())
}

// -------------------------------------------------------------------------------------------------
// Standard deploys
// -------------------------------------------------------------------------------------------------

/// The standard (blessed) genesis deploys (port of `StandardDeploys`).
pub struct StandardDeploys;

impl StandardDeploys {
    /// Build + sign a standard deploy (port of `StandardDeploys.toDeploy`).
    fn to_deploy(
        term: String,
        private_key_hex: &str,
        timestamp: i64,
        shard_id: &str,
    ) -> Result<SignedDeployData, String> {
        let sk = PrivateKey::new(base16::unsafe_decode(private_key_hex));
        let data = DeployData {
            term,
            timestamp,
            phlo_price: 0,
            phlo_limit: MAX_VALUE,
            valid_after_block_number: 0,
            shard_id: shard_id.to_string(),
        };
        let signed = Signed::new(data, &Secp256k1, &sk).map_err(|e| e.to_string())?;
        Ok(SignedDeployData {
            data: signed.data,
            deployer: signed.pk.bytes().to_vec(),
            sig: signed.sig,
            sig_algorithm: signed.sig_algorithm.name().to_string(),
        })
    }

    /// The public keys of the standard contracts, in deploy order (port of `systemPublicKeys`).
    pub fn system_public_keys() -> Result<Vec<PublicKey>, String> {
        Ok(vec![
            to_public(REGISTRY_PK)?,
            to_public(LIST_OPS_PK)?,
            to_public(EITHER_PK)?,
            to_public(NON_NEGATIVE_NUMBER_PK)?,
            to_public(MAKE_MINT_PK)?,
            to_public(AUTH_KEY_PK)?,
            to_public(REV_VAULT_PK)?,
            to_public(MULTI_SIG_REV_VAULT_PK)?,
            to_public(POS_GENERATOR_PK)?,
            to_public(REV_GENERATOR_PK)?,
        ])
    }

    pub fn registry_generator(registry: &Registry, shard_id: &str) -> Result<SignedDeployData, String> {
        let term = load_template(
            "Registry.rho",
            REGISTRY_RHO,
            &[("systemContractPubKey", registry.system_contract_pub_key.as_str())],
        );
        Self::to_deploy(term, REGISTRY_PK, REGISTRY_TIMESTAMP, shard_id)
    }

    pub fn list_ops(shard_id: &str) -> Result<SignedDeployData, String> {
        Self::to_deploy(
            load_source("ListOps.rho", LIST_OPS_RHO),
            LIST_OPS_PK,
            LIST_OPS_TIMESTAMP,
            shard_id,
        )
    }

    pub fn either(shard_id: &str) -> Result<SignedDeployData, String> {
        Self::to_deploy(
            load_source("Either.rho", EITHER_RHO),
            EITHER_PK,
            EITHER_TIMESTAMP,
            shard_id,
        )
    }

    pub fn non_negative_number(shard_id: &str) -> Result<SignedDeployData, String> {
        Self::to_deploy(
            load_source("NonNegativeNumber.rho", NON_NEGATIVE_NUMBER_RHO),
            NON_NEGATIVE_NUMBER_PK,
            NON_NEGATIVE_NUMBER_TIMESTAMP,
            shard_id,
        )
    }

    pub fn make_mint(shard_id: &str) -> Result<SignedDeployData, String> {
        Self::to_deploy(
            load_source("MakeMint.rho", MAKE_MINT_RHO),
            MAKE_MINT_PK,
            MAKE_MINT_TIMESTAMP,
            shard_id,
        )
    }

    pub fn auth_key(shard_id: &str) -> Result<SignedDeployData, String> {
        Self::to_deploy(
            load_source("AuthKey.rho", AUTH_KEY_RHO),
            AUTH_KEY_PK,
            AUTH_KEY_TIMESTAMP,
            shard_id,
        )
    }

    pub fn rev_vault(shard_id: &str) -> Result<SignedDeployData, String> {
        Self::to_deploy(
            load_source("RevVault.rho", REV_VAULT_RHO),
            REV_VAULT_PK,
            REV_VAULT_TIMESTAMP,
            shard_id,
        )
    }

    pub fn multi_sig_rev_vault(shard_id: &str) -> Result<SignedDeployData, String> {
        Self::to_deploy(
            load_source("MultiSigRevVault.rho", MULTI_SIG_REV_VAULT_RHO),
            MULTI_SIG_REV_VAULT_PK,
            MULTI_SIG_REV_VAULT_TIMESTAMP,
            shard_id,
        )
    }

    pub fn pos_generator(pos: &ProofOfStake, shard_id: &str) -> Result<SignedDeployData, String> {
        let minimum_bond = pos.minimum_bond.to_string();
        let maximum_bond = pos.maximum_bond.to_string();
        let initial_bonds = ProofOfStake::initial_bonds(&pos.validators);
        let epoch_length = pos.epoch_length.to_string();
        let quarantine_length = pos.quarantine_length.to_string();
        let number_of_active_validators = pos.number_of_active_validators.to_string();
        let pos_multi_sig_public_keys = ProofOfStake::public_keys(&pos.pos_multi_sig_public_keys);
        let pos_multi_sig_quorum = pos.pos_multi_sig_quorum.to_string();
        let pos_vault_pub_key = pos.pos_vault_pub_key.clone();

        let macros: &[(&str, &str)] = &[
            ("minimumBond", minimum_bond.as_str()),
            ("maximumBond", maximum_bond.as_str()),
            ("initialBonds", initial_bonds.as_str()),
            ("epochLength", epoch_length.as_str()),
            ("quarantineLength", quarantine_length.as_str()),
            ("numberOfActiveValidators", number_of_active_validators.as_str()),
            ("posMultiSigPublicKeys", pos_multi_sig_public_keys.as_str()),
            ("posMultiSigQuorum", pos_multi_sig_quorum.as_str()),
            ("posVaultPubKey", pos_vault_pub_key.as_str()),
        ];
        let term = load_template("Pos.rhox", POS_RHOX, macros);
        Self::to_deploy(term, POS_GENERATOR_PK, POS_GENERATOR_TIMESTAMP, shard_id)
    }

    pub fn rev_generator(
        vaults: &[Vault],
        timestamp: i64,
        is_last_batch: bool,
        shard_id: &str,
    ) -> Result<SignedDeployData, String> {
        let term = rev_generator_code(vaults, is_last_batch);
        Self::to_deploy(term, REV_GENERATOR_PK, timestamp, shard_id)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_source_appends_comment() {
        let code = load_source("Foo.rho", "new x in { Nil }");
        assert!(code.starts_with("new x in { Nil }"));
        assert!(code.contains("//Loaded from resource file <<Foo.rho>>"));
    }

    #[test]
    fn load_template_substitutes_and_appends_comment() {
        let code = load_template("T.rhox", "contract( $$name$$ )", &[("name", "42")]);
        assert!(code.contains("contract( 42 )"));
        assert!(code.contains(" //Loaded from resource file <<T.rhox>>"));
    }

    #[test]
    fn system_public_keys_has_ten_entries() {
        assert_eq!(StandardDeploys::system_public_keys().unwrap().len(), 10);
    }
}
