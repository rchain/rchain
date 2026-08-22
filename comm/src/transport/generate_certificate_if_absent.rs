//! Self-signed X.509 certificate generation (when absent).
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/GenerateCertificateIfAbsent.scala` and
//! `crypto/.../util/CertificateHelper.generate`. The node certificate is a self-signed P-256 cert
//! whose CommonName is the node's base16 address (keccak-20 of the EC public point).

use std::fs;
use std::path::Path;

use rcgen::{CertificateParams, DnType, KeyPair, PKCS_ECDSA_P256_SHA256};

use crate::transport::tls_conf::TlsConf;

/// The elliptic curve used for node identity (port of `CertificateHelper.EllipticCurveName`).
pub const ELLIPTIC_CURVE_NAME: &str = "secp256r1";

/// Generate a self-signed P-256 certificate and PKCS#8 key. Returns `(cert_pem, key_pem)`.
pub fn generate_certificate() -> Result<(String, String), String> {
    let key_pair = KeyPair::generate_for(&PKCS_ECDSA_P256_SHA256).map_err(|e| e.to_string())?;
    let raw = key_pair.public_key_raw();
    // `raw` is the uncompressed point `04 || x || y`; the node address hashes `x || y`.
    let address = rchain_crypto::util::certificate_helper::public_address(&raw[1..]);
    let cn = rchain_shared::base16::encode(&address);

    let mut params = CertificateParams::new(Vec::new()).map_err(|e| e.to_string())?;
    params.distinguished_name.push(DnType::CommonName, &cn);
    let cert = params.self_signed(&key_pair).map_err(|e| e.to_string())?;
    Ok((cert.pem(), key_pair.serialize_pem()))
}

/// Generate a certificate at `tls.certificate_path` (and key at `tls.key_path`) if one is absent
/// and not custom-provided (port of `GenerateCertificateIfAbsent.run`).
pub fn run(tls: &TlsConf) -> Result<(), String> {
    if tls.custom_certificate_location || Path::new(&tls.certificate_path).exists() {
        return Ok(());
    }
    let (cert_pem, key_pem) = generate_certificate()?;
    fs::write(&tls.certificate_path, cert_pem).map_err(|e| e.to_string())?;
    // The node private key is secret material: write it owner-only (0600), not with default perms.
    rchain_crypto::util::key_util::write_private_key(Path::new(&tls.key_path), key_pem.as_bytes())?;
    Ok(())
}

/// Compute the node identity (the 20-byte keccak-20 address) from the generated key (port of
/// `NodeEnvironment.name`; the Scala reads the certificate, whose public key equals the key's).
pub fn node_address(tls: &TlsConf) -> Result<Vec<u8>, String> {
    let key_pem = fs::read_to_string(&tls.key_path).map_err(|e| e.to_string())?;
    let key_pair = KeyPair::from_pem(&key_pem).map_err(|e| e.to_string())?;
    let raw = key_pair.public_key_raw();
    Ok(rchain_crypto::util::certificate_helper::public_address(&raw[1..]))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn generates_pem_pair() {
        let (cert, key) = generate_certificate().unwrap();
        assert!(cert.starts_with("-----BEGIN CERTIFICATE-----"));
        assert!(cert.ends_with("-----END CERTIFICATE-----\n"));
        assert!(key.starts_with("-----BEGIN PRIVATE KEY-----"));
    }

    #[test]
    fn node_address_is_20_bytes() {
        let (_, key_pem) = generate_certificate().unwrap();
        let key_pair = KeyPair::from_pem(&key_pem).unwrap();
        let raw = key_pair.public_key_raw();
        let addr = rchain_crypto::util::certificate_helper::public_address(&raw[1..]);
        assert_eq!(addr.len(), 20);
    }
}
