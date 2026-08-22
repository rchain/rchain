//! Validator signing identity (port of `ValidatorIdentity.scala`).

use rchain_crypto::errors::CryptoError;
use rchain_crypto::private_key::PrivateKey;
use rchain_crypto::public_key::PublicKey;
use rchain_crypto::signatures::secp256k1::Secp256k1;
use rchain_crypto::signatures::signatures_alg::{from_algorithm, SignaturesAlg};
use rchain_models::casper::protocol::casper_message::BlockMessage;
use rchain_shared::base16;

use crate::proto_util::hash_block;

/// A validator's signing identity (port of `ValidatorIdentity`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ValidatorIdentity {
    pub public_key: PublicKey,
    pub private_key: PrivateKey,
    pub sig_algorithm: String,
}

impl ValidatorIdentity {
    /// Sign `data` with the identity's private key (port of `signature`).
    pub fn signature(&self, data: &[u8]) -> Result<Vec<u8>, CryptoError> {
        let alg = from_algorithm(&self.sig_algorithm).ok_or(CryptoError::InvalidKey)?;
        alg.sign(data, self.private_key.bytes())
    }

    /// Compute the block's content-addressed hash and sign it (port of `signBlock`).
    pub fn sign_block(&self, block: &BlockMessage) -> Result<BlockMessage, CryptoError> {
        let block_hash = hash_block(block);
        let sig = self.signature(block_hash.as_bytes())?;
        Ok(BlockMessage {
            sig,
            block_hash,
            ..block.clone()
        })
    }

    /// Build an identity from a private key (port of `ValidatorIdentity.apply`).
    pub fn from_private_key(private_key: PrivateKey) -> Result<ValidatorIdentity, CryptoError> {
        let public_key = Secp256k1.to_public(&private_key)?;
        Ok(ValidatorIdentity {
            public_key,
            private_key,
            sig_algorithm: Secp256k1.name().to_string(),
        })
    }

    /// Build an identity from a hex-encoded private key (port of `fromHex`).
    pub fn from_hex(hex: &str) -> Option<ValidatorIdentity> {
        base16::decode(hex)
            .and_then(|bytes| ValidatorIdentity::from_private_key(PrivateKey::new(bytes)).ok())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn from_hex_derives_public_key() {
        // A known secp256k1 private key (from the crypto differential vectors).
        let priv_hex =
            "67e56582298859ddae725f972992a07c6c4fb9f62a8fff58ce3ca926a1063530";
        let identity = ValidatorIdentity::from_hex(priv_hex).unwrap();
        assert_eq!(identity.sig_algorithm, "secp256k1");
        assert_eq!(identity.private_key.bytes().len(), 32);
        assert_eq!(identity.public_key.bytes().len(), 65);
    }

    #[test]
    fn from_hex_rejects_garbage() {
        assert!(ValidatorIdentity::from_hex("not-hex").is_none());
    }

    #[test]
    fn sign_block_sets_hash_and_signature() {
        let identity = ValidatorIdentity::from_hex(
            "67e56582298859ddae725f972992a07c6c4fb9f62a8fff58ce3ca926a1063530",
        )
        .unwrap();
        let block = crate::proto_util::unsigned_block_proto(
            1,
            "root".to_string(),
            rchain_shared::refined::BlockHeight::try_from(0).unwrap(),
            rchain_models::validator::Validator::from_slice(&identity.public_key.bytes()),
            rchain_shared::refined::SeqNum::try_from(0).unwrap(),
            rchain_models::block::state_hash::StateHash::new([0u8; 32]),
            rchain_models::block::state_hash::StateHash::new([0u8; 32]),
            vec![],
            std::collections::BTreeMap::new(),
            std::collections::BTreeSet::new(),
            rchain_models::casper::protocol::casper_message::RholangState::default(),
        );
        let signed = identity.sign_block(&block).unwrap();
        assert_eq!(signed.block_hash, hash_block(&block));
        assert!(!signed.sig.is_empty());
    }
}
