//! Rholang tools (port of `rholang/Tools.scala`).

use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_crypto::public_key::PublicKey;
use rchain_models::casper::protocol::casper_message::{DeployData, SignedDeployData};

/// The deterministic unforgeable-name RNG for a deployer + timestamp (port of `unforgeableNameRng`).
///
/// The seed is the wire encoding of a deploy proto carrying only `deployer` + `timestamp` (all
/// other fields default), matching `DeployDataProto().withDeployer(...).withTimestamp(...)`.
pub fn unforgeable_name_rng(deployer: &PublicKey, timestamp: i64) -> Blake2b512Random {
    let deploy = SignedDeployData {
        data: DeployData {
            term: String::new(),
            timestamp,
            phlo_price: 0,
            phlo_limit: 0,
            valid_after_block_number: 0,
            shard_id: String::new(),
        },
        deployer: deployer.bytes().to_vec(),
        sig: Vec::new(),
        sig_algorithm: String::new(),
    };
    Blake2b512Random::from_init(&deploy.to_bytes())
}

/// An RNG seeded by a signature (port of `rng`).
pub fn rng(signature: &[u8]) -> Blake2b512Random {
    Blake2b512Random::from_init(signature)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rng_is_deterministic() {
        let a = rng(&[1, 2, 3]);
        let b = rng(&[1, 2, 3]);
        assert_eq!(a.to_bytes(), b.to_bytes());
    }

    #[test]
    fn unforgeable_name_rng_depends_on_inputs() {
        let pk = PublicKey::new(vec![1; 65]);
        let a = unforgeable_name_rng(&pk, 100);
        let b = unforgeable_name_rng(&pk, 200);
        assert_ne!(a.to_bytes(), b.to_bytes());
    }
}
