//! Deploy construction helpers (port of `util/ConstructDeploy.scala`).

use rchain_crypto::private_key::PrivateKey;
use rchain_crypto::public_key::PublicKey;
use rchain_crypto::signatures::secp256k1::Secp256k1;
use rchain_crypto::signatures::signatures_alg::SignaturesAlg;
use rchain_crypto::signatures::signed::Signed;
use rchain_models::casper::protocol::casper_message::DeployData;
use rchain_shared::base16;

/// The default private key (port of `defaultSec`).
pub fn default_sec() -> PrivateKey {
    PrivateKey::new(base16::unsafe_decode(
        "a68a6e6cca30f81bd24a719f3145d20e8424bd7b396309b0708a16c7d8000b76",
    ))
}

/// The default public key (port of `defaultPub`).
pub fn default_pub() -> Result<PublicKey, String> {
    let sec = default_sec();
    Secp256k1
        .to_public(&sec)
        .map_err(|e| format!("derive default public key: {e}"))
}

/// The default (private, public) key pair (port of `defaultKeyPair`).
pub fn default_key_pair() -> Result<(PrivateKey, PublicKey), String> {
    let sec = default_sec();
    let pub_key = Secp256k1
        .to_public(&sec)
        .map_err(|e| format!("derive default public key: {e}"))?;
    Ok((sec, pub_key))
}

/// A second default private key (port of `defaultSec2`).
pub fn default_sec2() -> PrivateKey {
    PrivateKey::new(base16::unsafe_decode(
        "5a0bde2f5857124b1379c78535b07a278e3b9cefbcacc02e62ab3294c02765a1",
    ))
}

/// A second default public key (port of `defaultPub2`).
pub fn default_pub2() -> Result<PublicKey, String> {
    let sec = default_sec2();
    Secp256k1
        .to_public(&sec)
        .map_err(|e| format!("derive default public key: {e}"))
}

/// Build a signed deploy from source + parameters (port of `sourceDeploy`).
pub fn source_deploy(
    source: &str,
    timestamp: i64,
    phlo_limit: i64,
    phlo_price: i64,
    sec: &PrivateKey,
    vabn: i64,
    shard_id: &str,
) -> Result<Signed<DeployData>, String> {
    let data = DeployData {
        term: source.to_string(),
        timestamp,
        phlo_price,
        phlo_limit,
        valid_after_block_number: vabn,
        shard_id: shard_id.to_string(),
    };
    Signed::new(data, &Secp256k1, sec).map_err(|e| format!("sign deploy: {e}"))
}

/// Build a signed deploy with the current timestamp (port of `sourceDeployNow`).
pub fn source_deploy_now(
    source: &str,
    sec: &PrivateKey,
    phlo_limit: i64,
    vabn: i64,
    shard_id: &str,
) -> Result<Signed<DeployData>, String> {
    let now = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_millis() as i64)
        .unwrap_or(0);
    source_deploy(source, now, phlo_limit, 1, sec, vabn, shard_id)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::serialize::Serialize;

    #[test]
    fn default_pub_matches_default_sec() {
        let (sec, pub_key) = default_key_pair().unwrap();
        assert_eq!(Secp256k1.to_public(&sec).unwrap(), pub_key);
    }

    #[test]
    fn source_deploy_signs_with_default_key() {
        let sec = default_sec();
        let deploy = source_deploy("Nil", 0, 90000, 1, &sec, 0, "root").unwrap();
        assert_eq!(deploy.data.term, "Nil");
        assert_eq!(deploy.pk, default_pub().unwrap());
        // The signature verifies over the serialized deploy data.
        let serialized = <DeployData as Serialize<DeployData>>::encode(&deploy.data);
        let hash = rchain_crypto::signatures::signed::signature_hash("secp256k1", &serialized);
        assert!(Secp256k1.verify(&hash, &deploy.sig, deploy.pk.bytes()));
    }
}
