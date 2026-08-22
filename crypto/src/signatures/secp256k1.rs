//! secp256k1 ECDSA signatures.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/signatures/Secp256k1.scala`. The Scala wraps
//! the native `libsecp256k1` (`NativeSecp256k1`) and produces/consumes DER-encoded signatures; the
//! Rust port wraps the pure-Rust `k256` crate (RFC-6979 deterministic nonces, low-S DER).

use std::fs;
use std::path::Path;

use super::signatures_alg::SignaturesAlg;
use crate::errors::CryptoError;
use crate::private_key::PrivateKey;
use crate::public_key::PublicKey;
use k256::ecdsa::signature::hazmat::{PrehashSigner, PrehashVerifier};
use k256::ecdsa::{Signature, SigningKey, VerifyingKey};
use k256::elliptic_curve::sec1::ToEncodedPoint;
use k256::pkcs8::DecodePrivateKey;
use k256::SecretKey;
use pkcs8::{EncryptedPrivateKeyInfo, SecretDocument};
use rand::rngs::OsRng;

/// The secp256k1 algorithm.
pub struct Secp256k1;

impl Secp256k1 {
    /// Sign a 32-byte message hash, returning a DER-encoded signature.
    pub fn sign_bytes(data: &[u8], sec: &[u8]) -> Result<Vec<u8>, CryptoError> {
        let sk = SecretKey::from_slice(sec).map_err(|_| CryptoError::InvalidKey)?;
        let signing = SigningKey::from(&sk);
        let sig: Signature = signing.sign_prehash(data).map_err(|_| CryptoError::InvalidKey)?;
        Ok(sig.to_der().as_bytes().to_vec())
    }

    /// Verify a DER-encoded signature over a 32-byte message hash.
    pub fn verify_bytes(data: &[u8], signature: &[u8], pub_key: &[u8]) -> bool {
        let Ok(sig) = Signature::from_der(signature) else {
            return false;
        };
        let Ok(pk) = k256::PublicKey::from_sec1_bytes(pub_key) else {
            return false;
        };
        VerifyingKey::from(&pk).verify_prehash(data, &sig).is_ok()
    }

    /// Check that a 32-byte secret key is a valid scalar.
    pub fn sec_key_verify(seckey: &[u8]) -> bool {
        SecretKey::from_slice(seckey).is_ok()
    }

    /// Compute the uncompressed (65-byte) public key from a 32-byte secret key.
    pub fn to_public_bytes(seckey: &[u8]) -> Result<Vec<u8>, CryptoError> {
        let sk = SecretKey::from_slice(seckey).map_err(|_| CryptoError::InvalidKey)?;
        Ok(sk.public_key().to_encoded_point(false).as_bytes().to_vec())
    }

    /// Parse an encrypted PEM private key into a secp256k1 `PrivateKey`.
    ///
    /// Port of `Secp256k1.parsePemFile`. Reads a PKCS#8 `ENCRYPTED PRIVATE KEY` (the format written
    /// by [`crate::util::key_util::write_keys`]), decrypts it with `password`, and returns the raw
    /// 32-byte secret key. The Scala oracle read a BouncyCastle `PEMEncryptedKeyPair`, but that
    /// never round-tripped with `KeyUtil.writeKeys` (which emits PKCS#8 `EncryptedPrivateKeyInfo`);
    /// the port reads the PKCS#8 form so it round-trips with `write_keys`.
    pub fn parse_pem_file(path: &Path, password: &str) -> Result<PrivateKey, String> {
        let pem = fs::read_to_string(path).map_err(|e| format!("Could not read PEM file: {e}"))?;
        let (label, secret_doc) =
            SecretDocument::from_pem(&pem).map_err(|_| "PEM file is not encrypted".to_string())?;
        if label != "ENCRYPTED PRIVATE KEY" {
            return Err("PEM file is not encrypted".to_string());
        }
        let epki = EncryptedPrivateKeyInfo::try_from(secret_doc.as_bytes())
            .map_err(|_| "PEM file is not encrypted".to_string())?;
        let decrypted = epki
            .decrypt(password.as_bytes())
            .map_err(|_| "Could not decrypt PEM file".to_string())?;
        let secret = SecretKey::from_pkcs8_der(decrypted.as_bytes())
            .map_err(|_| "Could not parse private key from PEM file".to_string())?;
        Ok(PrivateKey::new(secret.to_bytes().to_vec()))
    }
}

impl SignaturesAlg for Secp256k1 {
    fn verify(&self, data: &[u8], signature: &[u8], pub_key: &[u8]) -> bool {
        Secp256k1::verify_bytes(data, signature, pub_key)
    }

    fn sign(&self, data: &[u8], sec: &[u8]) -> Result<Vec<u8>, CryptoError> {
        Secp256k1::sign_bytes(data, sec)
    }

    fn to_public(&self, sec: &PrivateKey) -> Result<PublicKey, CryptoError> {
        Ok(PublicKey::new(Secp256k1::to_public_bytes(sec.bytes())?))
    }

    fn new_key_pair(&self) -> (PrivateKey, PublicKey) {
        let signing = SigningKey::random(&mut OsRng);
        let sec = signing.to_bytes().to_vec();
        let pub_key = signing.verifying_key().to_encoded_point(false).as_bytes().to_vec();
        (PrivateKey::new(sec), PublicKey::new(pub_key))
    }

    fn name(&self) -> &'static str {
        "secp256k1"
    }

    fn sig_length(&self) -> usize {
        32
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::hash::sha256;
    use rchain_shared::base16;

    #[test]
    fn verifies_signature_with_keypair() {
        let (PrivateKey(sec), public_key) = Secp256k1.new_key_pair();
        let data = sha256::hash(b"testing");
        let sig = Secp256k1::sign_bytes(&data, &sec).expect("sign with valid secret key");
        assert!(Secp256k1::verify_bytes(&data, &sig, public_key.bytes()));
    }

    #[test]
    fn verifies_known_signature() {
        let data = sha256::hash(b"testing");
        let sig = base16::unsafe_decode(
            "3044022079BE667EF9DCBBAC55A06295CE870B07029BFCDB2DCE28D959F2815B16F817980220294F14E883B3F525B5367756C2A11EF6CF84B730B36C17CB0C56F0AAB2C98589",
        );
        let pub_key = base16::unsafe_decode(
            "040A629506E1B65CD9D2E0BA9C75DF9C4FED0DB16DC9625ED14397F0AFC836FAE595DC53F8B0EFE61E703075BD9B143BAC75EC0E19F82A2208CAEB32BE53414C40",
        );
        assert!(Secp256k1::verify_bytes(&data, &sig, &pub_key));
    }

    #[test]
    fn creates_known_ecdsa_signature() {
        let data = sha256::hash(b"testing");
        let sec = base16::unsafe_decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530");
        let sig = Secp256k1::sign_bytes(&data, &sec).expect("sign with valid secret key");
        assert_eq!(
            base16::encode(&sig).to_uppercase(),
            "30440220182A108E1448DC8F1FB467D06A0F3BB8EA0533584CB954EF8DA112F1D60E39A202201C66F36DA211C087F3AF88B50EDF4F9BDAA6CF5FD6817E74DCA34DB12390C6E9"
        );
    }

    #[test]
    fn sec_key_verify() {
        let sec = base16::unsafe_decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530");
        assert!(Secp256k1::sec_key_verify(&sec));
    }

    #[test]
    fn computes_public_key_from_secret_key() {
        let sec = base16::unsafe_decode("67E56582298859DDAE725F972992A07C6C4FB9F62A8FFF58CE3CA926A1063530");
        let pub_key = Secp256k1::to_public_bytes(&sec).expect("compute public key");
        assert_eq!(
            base16::encode(&pub_key).to_uppercase(),
            "04C591A8FF19AC9C4E4E5793673B83123437E975285E7B442F4EE2654DFFCA5E2D2103ED494718C697AC9AEBCFD19612E224DB46661011863ED2FC54E71861E2A6"
        );
    }

    #[test]
    fn parse_pem_file_round_trips_write_keys() {
        let (sk, pk) = Secp256k1.new_key_pair();
        let dir = std::env::temp_dir().join(format!("rchain_secp_pem_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        let private_path = dir.join("private.pem");
        let public_path = dir.join("public.pem");
        let hex_path = dir.join("public.hex");

        crate::util::key_util::write_keys(
            &sk,
            &pk,
            &Secp256k1,
            "password",
            &private_path,
            &public_path,
            &hex_path,
        )
        .unwrap();

        let parsed = Secp256k1::parse_pem_file(&private_path, "password").unwrap();
        assert_eq!(parsed.bytes(), sk.bytes());

        assert!(Secp256k1::parse_pem_file(&private_path, "wrong").is_err());
        assert!(Secp256k1::parse_pem_file(&public_path, "password").is_err());

        fs::remove_dir_all(&dir).unwrap();
    }
}
