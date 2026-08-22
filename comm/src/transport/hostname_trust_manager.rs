//! TLS node-identity trust manager.
//!
//! Mirrors `comm/src/main/scala/coop/rchain/comm/transport/HostnameTrustManagerFactory.scala`. The
//! trust model is "a peer's certificate public key must hash (keccak-20) to the peer's node id":
//! the client verifies the server cert against the expected authority (the peer's base16 node id),
//! and the server requires a client cert whose public key is a valid P-256 node key.

use std::sync::Arc;

use rustls::client::danger::{HandshakeSignatureValid, ServerCertVerified, ServerCertVerifier};
use rustls::crypto::{verify_tls12_signature, verify_tls13_signature, CryptoProvider};
use rustls::pki_types::{CertificateDer, PrivateKeyDer, ServerName, UnixTime};
use rustls::server::danger::{ClientCertVerified, ClientCertVerifier};
use rustls::{DigitallySignedStruct, Error, SignatureScheme};

/// Compute the 20-byte node address from a certificate's P-256 public key (the uncompressed point
/// `04 || x || y` hashed with keccak-256, dropping the first 12 bytes).
pub fn public_address_of_cert(cert: &CertificateDer<'_>) -> Option<Vec<u8>> {
    let (_, parsed) = x509_parser::parse_x509_certificate(cert.as_ref()).ok()?;
    let spki = parsed.public_key();
    let point: &[u8] = spki.subject_public_key.data.as_ref();
    if point.len() != 65 || point[0] != 0x04 {
        return None;
    }
    Some(rchain_crypto::util::certificate_helper::public_address(&point[1..]))
}

fn ring_provider() -> Arc<CryptoProvider> {
    Arc::new(rustls::crypto::ring::default_provider())
}

/// Check that the certificate is currently valid (`not_before <= now <= not_after`). The custom
/// verifier replaces rustls' default path, so the validity window (which the default verifier
/// checks) must be enforced explicitly.
fn cert_valid_at(cert: &CertificateDer<'_>, now: UnixTime) -> bool {
    let Ok((_, parsed)) = x509_parser::parse_x509_certificate(cert.as_ref()) else {
        return false;
    };
    let validity = parsed.validity();
    let now_secs = now.as_secs() as i64;
    now_secs >= validity.not_before.timestamp() && now_secs <= validity.not_after.timestamp()
}

/// Client-side verifier: the server cert's public address must equal the DNS server name (peer id).
#[derive(Debug)]
pub struct NodeIdServerVerifier {
    provider: Arc<CryptoProvider>,
}

impl NodeIdServerVerifier {
    pub fn new() -> Arc<Self> {
        Arc::new(NodeIdServerVerifier {
            provider: ring_provider(),
        })
    }
}

impl ServerCertVerifier for NodeIdServerVerifier {
    fn verify_server_cert(
        &self,
        end_entity: &CertificateDer<'_>,
        _intermediates: &[CertificateDer<'_>],
        server_name: &ServerName<'_>,
        _ocsp_response: &[u8],
        now: UnixTime,
    ) -> Result<ServerCertVerified, Error> {
        if !cert_valid_at(end_entity, now) {
            return Err(Error::General("certificate is not currently valid".into()));
        }
        let expected = match server_name {
            ServerName::DnsName(dns) => dns.as_ref().as_bytes(),
            _ => return Err(Error::General("expected a DNS server name (peer id)".into())),
        };
        let address = public_address_of_cert(end_entity)
            .ok_or_else(|| Error::General("certificate's public key has the wrong algorithm".into()))?;
        let address_hex = rchain_shared::base16::encode(&address);
        if address_hex.as_bytes() == expected {
            Ok(ServerCertVerified::assertion())
        } else {
            Err(Error::General(
                "certificate's public address doesn't match the hostname".into(),
            ))
        }
    }

    fn verify_tls12_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, Error> {
        verify_tls12_signature(
            message,
            cert,
            dss,
            &self.provider.signature_verification_algorithms,
        )
    }

    fn verify_tls13_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, Error> {
        verify_tls13_signature(
            message,
            cert,
            dss,
            &self.provider.signature_verification_algorithms,
        )
    }

    fn supported_verify_schemes(&self) -> Vec<SignatureScheme> {
        self.provider
            .signature_verification_algorithms
            .supported_schemes()
    }
}

/// Server-side verifier: require (mandatory) a client cert with a valid P-256 node key.
#[derive(Debug)]
pub struct NodeIdClientVerifier {
    provider: Arc<CryptoProvider>,
}

impl NodeIdClientVerifier {
    pub fn new() -> Arc<Self> {
        Arc::new(NodeIdClientVerifier {
            provider: ring_provider(),
        })
    }
}

impl ClientCertVerifier for NodeIdClientVerifier {
    fn verify_client_cert(
        &self,
        end_entity: &CertificateDer<'_>,
        _intermediates: &[CertificateDer<'_>],
        now: UnixTime,
    ) -> Result<ClientCertVerified, Error> {
        if !cert_valid_at(end_entity, now) {
            return Err(Error::General("certificate is not currently valid".into()));
        }
        public_address_of_cert(end_entity)
            .map(|_| ClientCertVerified::assertion())
            .ok_or_else(|| Error::General("certificate's public key has the wrong algorithm".into()))
    }

    fn client_auth_mandatory(&self) -> bool {
        true
    }

    fn root_hint_subjects(&self) -> &[rustls::DistinguishedName] {
        &[]
    }

    fn verify_tls12_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, Error> {
        verify_tls12_signature(
            message,
            cert,
            dss,
            &self.provider.signature_verification_algorithms,
        )
    }

    fn verify_tls13_signature(
        &self,
        message: &[u8],
        cert: &CertificateDer<'_>,
        dss: &DigitallySignedStruct,
    ) -> Result<HandshakeSignatureValid, Error> {
        verify_tls13_signature(
            message,
            cert,
            dss,
            &self.provider.signature_verification_algorithms,
        )
    }

    fn supported_verify_schemes(&self) -> Vec<SignatureScheme> {
        self.provider
            .signature_verification_algorithms
            .supported_schemes()
    }
}

fn load_certs(pem: &str) -> Result<Vec<CertificateDer<'static>>, String> {
    let mut reader = std::io::BufReader::new(pem.as_bytes());
    rustls_pemfile::certs(&mut reader)
        .collect::<Result<Vec<_>, _>>()
        .map_err(|e| e.to_string())
}

fn load_key(pem: &str) -> Result<PrivateKeyDer<'static>, String> {
    let mut reader = std::io::BufReader::new(pem.as_bytes());
    rustls_pemfile::private_key(&mut reader)
        .map_err(|e| e.to_string())?
        .ok_or_else(|| "no private key found".to_string())
}

/// Build a mutual-TLS server config from the node's cert/key (server requires a client cert whose
/// public key is a valid P-256 node key).
pub fn server_config(cert_pem: &str, key_pem: &str) -> Result<Arc<rustls::ServerConfig>, String> {
    let certs = load_certs(cert_pem)?;
    let key = load_key(key_pem)?;
    let config = rustls::ServerConfig::builder()
        .with_client_cert_verifier(NodeIdClientVerifier::new())
        .with_single_cert(certs, key)
        .map_err(|e| e.to_string())?;
    Ok(Arc::new(config))
}

/// Build a client config that presents the node's cert and verifies the server cert against the
/// expected peer node id (via the DNS server name).
pub fn client_config(cert_pem: &str, key_pem: &str) -> Result<Arc<rustls::ClientConfig>, String> {
    let certs = load_certs(cert_pem)?;
    let key = load_key(key_pem)?;
    let config = rustls::ClientConfig::builder()
        .dangerous()
        .with_custom_certificate_verifier(NodeIdServerVerifier::new())
        .with_client_auth_cert(certs, key)
        .map_err(|e| e.to_string())?;
    Ok(Arc::new(config))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn public_address_matches_cert_common_name() {
        let (cert_pem, _) =
            crate::transport::generate_certificate_if_absent::generate_certificate().unwrap();
        let (_, pem) = x509_parser::pem::parse_x509_pem(cert_pem.as_bytes()).unwrap();
        let der = CertificateDer::from(pem.contents);
        let address = public_address_of_cert(&der).unwrap();
        assert_eq!(address.len(), 20);

        let (_, parsed) = x509_parser::parse_x509_certificate(der.as_ref()).unwrap();
        let cn = parsed
            .subject()
            .iter_common_name()
            .next()
            .unwrap()
            .as_str()
            .unwrap();
        assert_eq!(cn, rchain_shared::base16::encode(&address));
    }

    #[test]
    fn server_verifier_rejects_wrong_hostname() {
        let (cert_pem, _) =
            crate::transport::generate_certificate_if_absent::generate_certificate().unwrap();
        let (_, pem) = x509_parser::pem::parse_x509_pem(cert_pem.as_bytes()).unwrap();
        let der = CertificateDer::from(pem.contents);

        let verifier = NodeIdServerVerifier::new();
        // A fixed 40-hex peer id that cannot equal the randomly-generated cert's address.
        let name = ServerName::try_from("deadbeefdeadbeefdeadbeefdeadbeefdeadbeef").unwrap();
        let result = verifier.verify_server_cert(&der, &[], &name, &[], UnixTime::now());
        assert!(result.is_err(), "a server cert whose address != the peer id must be rejected");
    }

    #[test]
    fn client_verifier_rejects_stale_cert() {
        let (cert_pem, _) =
            crate::transport::generate_certificate_if_absent::generate_certificate().unwrap();
        let (_, pem) = x509_parser::pem::parse_x509_pem(cert_pem.as_bytes()).unwrap();
        let der = CertificateDer::from(pem.contents);

        let verifier = NodeIdClientVerifier::new();
        // `now` at the Unix epoch is before the cert's `not_before`.
        let now = UnixTime::since_unix_epoch(std::time::Duration::ZERO);
        let result = verifier.verify_client_cert(&der, &[], now);
        assert!(result.is_err(), "a not-yet-valid cert must be rejected");
    }
}
