//! DER signature helpers, public-address derivation, and X.509 certificate helpers.
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/util/CertificateHelper.scala`. The DER
//! signature codecs and the raw `publicAddress` were ported first; this module now also ports the
//! X.509/P-256 helpers (`from` / `fromFile` / `readKeyPair` / `generateKeyPair` / `generate` /
//! `publicAddress(PublicKey)`) and the `CertificatePrinter` PEM printers.

use std::fs;
use std::path::Path;
use std::str::FromStr;
use std::time::Duration;

use p256::ecdsa::SigningKey;
use p256::elliptic_curve::sec1::ToEncodedPoint;
use p256::pkcs8::{DecodePrivateKey, EncodePrivateKey};
use rand::rngs::OsRng;
use rand::RngCore;
use x509_cert::builder::{Builder, CertificateBuilder, Profile};
use x509_cert::der::{pem::LineEnding, Decode, DecodePem, EncodePem};
use x509_cert::name::Name;
use x509_cert::serial_number::SerialNumber;
use x509_cert::spki::SubjectPublicKeyInfoOwned;
use x509_cert::time::Validity;
use x509_cert::Certificate;

use rchain_shared::base16;

use crate::hash::keccak256;

/// Keccak256-hash the input and drop the leading 12 bytes, yielding a 20-byte address.
pub fn public_address(input: &[u8]) -> Vec<u8> {
    keccak256::hash(input)[12..].to_vec()
}

/// Encode a raw 64-byte RS signature as a DER `SEQUENCE { INTEGER r, INTEGER s }`.
pub fn encode_signature_rs_to_der(signature_rs: &[u8]) -> Result<Vec<u8>, String> {
    if signature_rs.is_empty() {
        return Err("Input array must not be empty".to_string());
    }
    let taken = &signature_rs[..signature_rs.len().min(64)];
    let (r, s) = taken.split_at(taken.len().min(32));

    let r_enc = der_integer(r);
    let s_enc = der_integer(s);
    let content_len = 2 + r_enc.len() + 2 + s_enc.len();

    let mut out = vec![0x30];
    encode_len(content_len, &mut out);
    out.push(0x02);
    out.push(r_enc.len() as u8);
    out.extend_from_slice(&r_enc);
    out.push(0x02);
    out.push(s_enc.len() as u8);
    out.extend_from_slice(&s_enc);
    Ok(out)
}

/// Decode a DER signature back into a 64-byte RS signature (each integer left-padded to 32 bytes).
pub fn decode_signature_der_to_rs(signature_der: &[u8]) -> Result<Vec<u8>, String> {
    if signature_der.is_empty() {
        return Err("Input array must not be empty".to_string());
    }
    if signature_der[0] != 0x30 {
        return Err("Input array is not valid DER message format".to_string());
    }
    let (seq_len, mut pos) = read_length(signature_der, 1)?;
    let end = pos + seq_len;

    if signature_der.get(pos) != Some(&0x02) {
        return Err("Input array is not valid DER message format".to_string());
    }
    let (r_len, r_pos) = read_length(signature_der, pos + 1)?;
    let r = &signature_der[r_pos..r_pos + r_len];
    pos = r_pos + r_len;

    if signature_der.get(pos) != Some(&0x02) {
        return Err("Input array is not valid DER message format".to_string());
    }
    let (s_len, s_pos) = read_length(signature_der, pos + 1)?;
    let s = &signature_der[s_pos..s_pos + s_len];

    if end > signature_der.len() {
        return Err("Input array is not valid DER message format".to_string());
    }

    let mut out = Vec::with_capacity(64);
    out.extend_from_slice(&left_pad_32(r)?);
    out.extend_from_slice(&left_pad_32(s)?);
    Ok(out)
}

/// Encode an unsigned big-endian integer as a minimal DER INTEGER.
fn der_integer(unsigned: &[u8]) -> Vec<u8> {
    let mut bytes = unsigned;
    while bytes.len() > 1 && bytes[0] == 0 {
        bytes = &bytes[1..];
    }
    let mut out = Vec::with_capacity(bytes.len() + 1);
    if bytes[0] & 0x80 != 0 {
        out.push(0x00);
    }
    out.extend_from_slice(bytes);
    out
}

fn encode_len(len: usize, out: &mut Vec<u8>) {
    if len < 0x80 {
        out.push(len as u8);
    } else {
        let bytes = len.to_be_bytes();
        let start = bytes.iter().position(|&b| b != 0).unwrap_or(bytes.len() - 1);
        out.push(0x80 | (bytes.len() - start) as u8);
        out.extend_from_slice(&bytes[start..]);
    }
}

fn read_length(buf: &[u8], pos: usize) -> Result<(usize, usize), String> {
    let first = *buf.get(pos).ok_or("truncated DER")?;
    if first & 0x80 == 0 {
        Ok((first as usize, pos + 1))
    } else {
        let n = (first & 0x7f) as usize;
        if n == 0 || n > 4 {
            return Err("invalid DER length".to_string());
        }
        let mut len = 0usize;
        for i in 0..n {
            len = (len << 8) | *buf.get(pos + 1 + i).ok_or("truncated DER")? as usize;
        }
        Ok((len, pos + 1 + n))
    }
}

fn left_pad_32(bytes: &[u8]) -> Result<[u8; 32], String> {
    let mut start = 0;
    while start < bytes.len() && bytes[start] == 0 {
        start += 1;
    }
    let significant = &bytes[start..];
    if significant.len() > 32 {
        return Err("integer too large".to_string());
    }
    let mut out = [0u8; 32];
    out[32 - significant.len()..].copy_from_slice(significant);
    Ok(out)
}

/// The EC curve used by the node's X.509 certificates (`secp256r1` = NIST P-256).
pub const ELLIPTIC_CURVE_NAME: &str = "secp256r1";

/// A P-256 (`secp256r1`) EC key pair for X.509 certificate use.
#[derive(Clone, Debug)]
pub struct EcKeyPair {
    pub private_key: p256::SecretKey,
    pub public_key: p256::PublicKey,
}

impl EcKeyPair {
    pub fn new(private_key: p256::SecretKey, public_key: p256::PublicKey) -> Self {
        Self {
            private_key,
            public_key,
        }
    }
}

/// Generate a fresh P-256 key pair.
///
/// The Scala `generateKeyPair(useNonBlockingRandom)` selected between a blocking and non-blocking
/// JVM `SecureRandom`; Rust's `OsRng` is a single, non-blocking OS CSPRNG, so the flag is dropped.
pub fn generate_key_pair() -> EcKeyPair {
    let private_key = p256::SecretKey::random(&mut OsRng);
    let public_key = private_key.public_key();
    EcKeyPair::new(private_key, public_key)
}

/// Read a PKCS#8 (`PRIVATE KEY`) P-256 private key and derive its public key.
pub fn read_key_pair(key_file: &Path) -> Result<EcKeyPair, String> {
    let pem = fs::read_to_string(key_file).map_err(|e| e.to_string())?;
    let private_key = p256::SecretKey::from_pkcs8_pem(&pem).map_err(|e| e.to_string())?;
    let public_key = private_key.public_key();
    Ok(EcKeyPair::new(private_key, public_key))
}

/// Parse an X.509 certificate (DER or PEM) from a file path string.
pub fn from(cert_file_path: &str) -> Result<Certificate, String> {
    from_file(Path::new(cert_file_path))
}

/// Parse an X.509 certificate (DER or PEM) from a file.
pub fn from_file(cert_file: &Path) -> Result<Certificate, String> {
    let bytes = fs::read(cert_file).map_err(|e| e.to_string())?;
    if bytes.starts_with(b"-----BEGIN") {
        let pem = std::str::from_utf8(&bytes).map_err(|e| e.to_string())?;
        Certificate::from_pem(pem).map_err(|e| e.to_string())
    } else {
        Certificate::from_der(&bytes).map_err(|e| e.to_string())
    }
}

/// Build a self-signed X.509 certificate for `key_pair` (SHA256withECDSA, `CN=<address>`).
///
/// The Scala built a minimal V3 cert through `sun.security.x509` and signed it twice (a
/// `X509CertImpl` fix-up quirk). The port produces the equivalent self-signed cert directly via the
/// `x509-cert` builder's `Profile::Root` (V3 + BasicConstraints CA + KeyUsage).
pub fn generate(key_pair: &EcKeyPair) -> Result<Certificate, String> {
    let signing_key = SigningKey::from(&key_pair.private_key);
    let subject_spki =
        SubjectPublicKeyInfoOwned::from_key(key_pair.public_key).map_err(|e| e.to_string())?;

    let mut serial_bytes = [0u8; 8];
    OsRng.fill_bytes(&mut serial_bytes);
    let serial_number = SerialNumber::from(u64::from_be_bytes(serial_bytes));

    let validity =
        Validity::from_now(Duration::from_secs(365 * 86400)).map_err(|e| e.to_string())?;

    let address = base16::encode(&public_address_from_public_key(&key_pair.public_key));
    let subject = Name::from_str(&format!("CN={address}")).map_err(|e| e.to_string())?;

    let builder = CertificateBuilder::new(
        Profile::Root,
        serial_number,
        validity,
        subject,
        subject_spki,
        &signing_key,
    )
    .map_err(|e| e.to_string())?;

    builder
        .build::<p256::ecdsa::DerSignature>()
        .map_err(|e| e.to_string())
}

/// The `publicAddress(publicKey)` overload: keccak256(x ‖ y) with the leading 12 bytes dropped.
///
/// The Scala guarded this with a runtime `isExpectedEllipticCurve` check; the static
/// `p256::PublicKey` type makes that check unnecessary.
pub fn public_address_from_public_key(public_key: &p256::PublicKey) -> Vec<u8> {
    let point = public_key.to_encoded_point(false);
    let bytes = point.as_bytes();
    public_address(&bytes[1..])
}

/// PEM-encode a certificate (`-----BEGIN CERTIFICATE-----`).
pub fn print_certificate(certificate: &Certificate) -> Result<String, String> {
    certificate.to_pem(LineEnding::LF).map_err(|e| e.to_string())
}

/// PEM-encode a private key (`-----BEGIN PRIVATE KEY-----`, PKCS#8).
pub fn print_private_key(private_key: &p256::SecretKey) -> Result<String, String> {
    let der = private_key.to_pkcs8_der().map_err(|e| e.to_string())?;
    let pem = der.to_pem("PRIVATE KEY", LineEnding::LF).map_err(|e| e.to_string())?;
    Ok(pem.to_string())
}

#[cfg(test)]
mod tests {
    use super::*;
    use x509_cert::der::Encode;

    #[test]
    fn der_round_trips_rs_signature() {
        // A representative 64-byte RS signature.
        let rs: Vec<u8> = (0..64).collect();
        let der = encode_signature_rs_to_der(&rs).expect("encode RS signature to DER");
        assert_eq!(
            decode_signature_der_to_rs(&der).expect("decode DER signature to RS"),
            rs
        );
    }

    #[test]
    fn encoder_rejects_empty_input() {
        assert!(encode_signature_rs_to_der(&[]).is_err());
    }

    #[test]
    fn decoder_rejects_empty_input() {
        assert!(decode_signature_der_to_rs(&[]).is_err());
    }

    #[test]
    fn decoder_rejects_invalid_der() {
        assert!(decode_signature_der_to_rs(&[0xff, 0x00, 0x00, 0x00]).is_err());
    }

    #[test]
    fn public_address_is_last_20_bytes_of_keccak() {
        let input = b"hello";
        let addr = public_address(input);
        assert_eq!(addr.len(), 20);
        assert_eq!(addr, crate::hash::keccak256::hash(input)[12..].to_vec());
    }

    fn temp_dir(tag: &str) -> std::path::PathBuf {
        // A unique per-test suffix avoids the shared-dir race when libtest runs these tests in
        // parallel (a previous pid-only name made `remove_dir_all` delete another test's dir).
        let dir = std::env::temp_dir().join(format!(
            "rchain_cert_helper_{}_{}",
            std::process::id(),
            tag
        ));
        let _ = fs::remove_dir_all(&dir);
        fs::create_dir_all(&dir).unwrap();
        dir
    }

    #[test]
    fn public_address_from_public_key_matches_xy_keccak() {
        let key_pair = generate_key_pair();
        let addr = public_address_from_public_key(&key_pair.public_key);
        assert_eq!(addr.len(), 20);

        let point = key_pair.public_key.to_encoded_point(false);
        let xy = &point.as_bytes()[1..];
        assert_eq!(addr, public_address(xy));
    }

    #[test]
    fn generates_and_round_trips_certificate() {
        let key_pair = generate_key_pair();
        let cert = generate(&key_pair).expect("generate self-signed certificate");

        // The certificate carries the key pair's public key.
        let spki = SubjectPublicKeyInfoOwned::from_key(key_pair.public_key).unwrap();
        assert_eq!(cert.tbs_certificate.subject_public_key_info, spki);

        // PEM printing round-trips through parsing.
        let pem = print_certificate(&cert).expect("print certificate");
        assert!(pem.starts_with("-----BEGIN CERTIFICATE-----"));
        assert_eq!(Certificate::from_pem(&pem).expect("parse PEM"), cert);

        // Both file branches of `from_file` round-trip (PEM and DER).
        let dir = temp_dir("cert");
        let pem_path = dir.join("cert.pem");
        let der_path = dir.join("cert.der");
        fs::write(&pem_path, &pem).unwrap();
        fs::write(&der_path, cert.to_der().expect("encode DER")).unwrap();
        assert_eq!(from_file(&pem_path).expect("parse PEM file"), cert);
        assert_eq!(from_file(&der_path).expect("parse DER file"), cert);
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn read_key_pair_round_trips_private_key() {
        let key_pair = generate_key_pair();
        let dir = temp_dir("key_pair");
        let key_file = dir.join("private.pem");

        let pem = print_private_key(&key_pair.private_key).expect("print private key");
        assert!(pem.starts_with("-----BEGIN PRIVATE KEY-----"));
        fs::write(&key_file, pem).unwrap();

        let read = read_key_pair(&key_file).expect("read key pair");
        assert_eq!(read.private_key, key_pair.private_key);
        assert_eq!(read.public_key, key_pair.public_key);

        fs::remove_dir_all(&dir).unwrap();
    }
}
