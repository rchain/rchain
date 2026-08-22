//! Key writing (port of `crypto/util/KeyUtil.scala`).
//!
//! Writes secp256k1 validator keys: the private key as an `ENCRYPTED PRIVATE KEY`
//! (PKCS#8 `EncryptedPrivateKeyInfo`, PBES2/PBKDF2-SHA256 + AES-256-CBC, matching the original's
//! `JcePEMEncryptorBuilder("AES-256-CBC")`), the public key as `PUBLIC KEY`
//! (SubjectPublicKeyInfo), and the public key as hex.

use std::fs;
use std::io::Write;
use std::path::Path;

use k256::pkcs8::{EncodePrivateKey, EncodePublicKey, LineEnding};
use k256::SecretKey;
use pkcs5::pbes2;
use pkcs8::{EncryptedPrivateKeyInfo, SecretDocument};
use rand::RngCore;

use rchain_shared::base16;

use crate::private_key::PrivateKey;
use crate::public_key::PublicKey;
use crate::signatures::signatures_alg::SignaturesAlg;

/// PBKDF2 iteration count for the encrypted private key.
///
/// Raised from BouncyCastle's `1024` default to the OWASP-recommended floor for PBKDF2-HMAC-SHA256
/// (documented deviation — a higher count slows offline brute-force at rest). Note the interop
/// caveat: keys written with this count cannot be decrypted by tooling pinned to the 1024 default
/// and vice-versa.
const PBKDF2_ITERATIONS: u32 = 310_000;
const SALT_LEN: usize = 16;
const AES_BLOCK_SIZE: usize = 16;

/// Write secret material (e.g. a private key) with owner-only permissions (`0o600`). Falls back to a
/// plain `fs::write` on non-Unix platforms (the workspace targets Linux).
pub fn write_private_key(path: &Path, bytes: impl AsRef<[u8]>) -> Result<(), String> {
    write_with_mode(path, bytes.as_ref(), 0o600)
}

#[cfg(unix)]
fn write_with_mode(path: &Path, bytes: &[u8], mode: u32) -> Result<(), String> {
    use std::os::unix::fs::OpenOptionsExt;
    let mut opts = fs::OpenOptions::new();
    opts.write(true).create(true).truncate(true).mode(mode);
    opts.open(path)
        .map_err(|e| e.to_string())?
        .write_all(bytes)
        .map_err(|e| e.to_string())
}

#[cfg(not(unix))]
fn write_with_mode(path: &Path, bytes: &[u8], _mode: u32) -> Result<(), String> {
    fs::write(path, bytes).map_err(|e| e.to_string())
}

/// Write validator keys to PEM/hex files (port of `KeyUtil.writeKeys`).
pub fn write_keys(
    sk: &PrivateKey,
    pk: &PublicKey,
    sig_algorithm: &dyn SignaturesAlg,
    password: &str,
    private_key_pem_path: &Path,
    public_key_pem_path: &Path,
    public_key_hex_path: &Path,
) -> Result<(), String> {
    if sig_algorithm.name() != "secp256k1" {
        return Err("Invalid algorithm".to_string());
    }

    let secret = SecretKey::from_slice(sk.bytes()).map_err(|e| e.to_string())?;
    let public = k256::PublicKey::from_sec1_bytes(pk.bytes()).map_err(|e| e.to_string())?;

    // Private key → PKCS#8 DER → encrypted PEM (PBKDF2-SHA256 + AES-256-CBC).
    let private_der = secret.to_pkcs8_der().map_err(|e| e.to_string())?;
    let mut salt = [0u8; SALT_LEN];
    let mut iv = [0u8; AES_BLOCK_SIZE];
    rand::rngs::OsRng.fill_bytes(&mut salt);
    rand::rngs::OsRng.fill_bytes(&mut iv);
    let params = pbes2::Parameters::pbkdf2_sha256_aes256cbc(PBKDF2_ITERATIONS, &salt, &iv)
        .map_err(|e| e.to_string())?;
    let encrypted = params
        .encrypt(password.as_bytes(), private_der.as_bytes())
        .map_err(|e| e.to_string())?;
    let epki = EncryptedPrivateKeyInfo {
        encryption_algorithm: params.into(),
        encrypted_data: &encrypted,
    };
    let secret_doc = SecretDocument::try_from(&epki).map_err(|e| e.to_string())?;
    let private_pem = secret_doc
        .to_pem("ENCRYPTED PRIVATE KEY", LineEnding::LF)
        .map_err(|e| e.to_string())?;

    // Public key → SPKI DER → PEM.
    let public_der = public.to_public_key_der().map_err(|e| e.to_string())?;
    let public_pem = public_der
        .to_pem("PUBLIC KEY", LineEnding::LF)
        .map_err(|e| e.to_string())?;

    write_private_key(private_key_pem_path, private_pem.as_bytes())?;
    fs::write(public_key_pem_path, public_pem.as_bytes()).map_err(|e| e.to_string())?;
    fs::write(public_key_hex_path, format!("{}\n", base16::encode(pk.bytes())))
        .map_err(|e| e.to_string())?;

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::signatures::secp256k1::Secp256k1;
    use crate::signatures::signatures_alg::SignaturesAlg;
    use k256::pkcs8::{DecodePrivateKey, DecodePublicKey};

    fn temp_dir() -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!("rchain_key_util_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn write_keys_round_trips() {
        let dir = temp_dir();
        let (sk, pk) = Secp256k1.new_key_pair();
        let private_path = dir.join("private.pem");
        let public_path = dir.join("public.pem");
        let hex_path = dir.join("public.hex");

        write_keys(
            &sk,
            &pk,
            &Secp256k1,
            "password",
            &private_path,
            &public_path,
            &hex_path,
        )
        .unwrap();

        // Public key hex.
        let hex = fs::read_to_string(&hex_path).unwrap();
        assert_eq!(hex, format!("{}\n", base16::encode(pk.bytes())));

        // Public key PEM round-trips to the same key.
        let public_pem = fs::read_to_string(&public_path).unwrap();
        assert!(public_pem.starts_with("-----BEGIN PUBLIC KEY-----"));
        let public2 = k256::PublicKey::from_public_key_pem(&public_pem).unwrap();
        assert_eq!(
            public2,
            k256::PublicKey::from_sec1_bytes(pk.bytes()).unwrap()
        );

        // Private key PEM decrypts back to the same secret key.
        let private_pem = fs::read_to_string(&private_path).unwrap();
        assert!(private_pem.starts_with("-----BEGIN ENCRYPTED PRIVATE KEY-----"));
        let (label, secret_doc) = SecretDocument::from_pem(&private_pem).unwrap();
        assert_eq!(label, "ENCRYPTED PRIVATE KEY");
        let epki = EncryptedPrivateKeyInfo::try_from(secret_doc.as_bytes()).unwrap();
        let decrypted = epki.decrypt(b"password").unwrap();
        let secret2 = SecretKey::from_pkcs8_der(decrypted.as_bytes()).unwrap();
        assert_eq!(
            secret2.to_bytes(),
            SecretKey::from_slice(sk.bytes()).unwrap().to_bytes()
        );

        fs::remove_dir_all(&dir).unwrap();
    }
}
