//! Curve25519 encryption (XSalsa20-Poly1305 box).
//!
//! Mirrors `crypto/src/main/scala/coop/rchain/crypto/encryption/Curve25519.scala`. The Scala wraps
//! kalium/lib sodium's `crypto_box_curve25519xsalsa20poly1305`; the Rust port wraps `crypto_box`
//! (`SalsaBox`), which is the same construction.

use crypto_box::aead::{Aead, Nonce};
use crypto_box::{PublicKey as BoxPublicKey, SalsaBox, SecretKey as BoxSecretKey};
use rand::RngCore;

use crate::errors::CryptoError;

/// Generate a fresh (public, secret) key pair.
pub fn new_key_pair() -> (Vec<u8>, Vec<u8>) {
    let sk = BoxSecretKey::generate(&mut rand::rngs::OsRng);
    let pk = sk.public_key();
    (pk.as_bytes().to_vec(), sk.to_bytes().to_vec())
}

/// Generate a 24-byte nonce.
pub fn new_nonce() -> Vec<u8> {
    let mut nonce = [0u8; 24];
    rand::rngs::OsRng.fill_bytes(&mut nonce);
    nonce.to_vec()
}

/// Compute the 32-byte public key from a 32-byte secret key.
pub fn to_public(sec: &[u8]) -> Vec<u8> {
    let mut arr = [0u8; 32];
    arr.copy_from_slice(sec);
    BoxSecretKey::from(arr).public_key().as_bytes().to_vec()
}

/// Encrypt `message` with the box keyed by `(pub, sec)` and `nonce`.
pub fn encrypt(
    pub_key: &[u8],
    sec: &[u8],
    nonce: &[u8],
    message: &[u8],
) -> Result<Vec<u8>, CryptoError> {
    let sender_sk = secret_key_from(sec);
    let recipient_pk = public_key_from(pub_key)?;
    let b = SalsaBox::new(&recipient_pk, &sender_sk);
    let nonce = Nonce::<SalsaBox>::from_slice(nonce);
    b.encrypt(nonce, message)
        .map_err(|_| CryptoError::EncryptionFailed)
}

/// Decrypt `cipher` with the box keyed by `(pub, sec)` and `nonce`.
pub fn decrypt(
    pub_key: &[u8],
    sec: &[u8],
    nonce: &[u8],
    cipher: &[u8],
) -> Result<Vec<u8>, CryptoError> {
    let sender_sk = secret_key_from(sec);
    let recipient_pk = public_key_from(pub_key)?;
    let b = SalsaBox::new(&recipient_pk, &sender_sk);
    let nonce = Nonce::<SalsaBox>::from_slice(nonce);
    b.decrypt(nonce, cipher)
        .map_err(|_| CryptoError::EncryptionFailed)
}

fn secret_key_from(bytes: &[u8]) -> BoxSecretKey {
    let mut arr = [0u8; 32];
    arr.copy_from_slice(bytes);
    BoxSecretKey::from(arr)
}

fn public_key_from(bytes: &[u8]) -> Result<BoxPublicKey, CryptoError> {
    let arr: [u8; 32] = bytes.try_into().map_err(|_| CryptoError::InvalidLength {
        expected: 32,
        actual: bytes.len(),
    })?;
    Ok(BoxPublicKey::from(arr))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_shared::base16;

    #[test]
    fn encodes_a_cipher() {
        let bob_sec =
            base16::unsafe_decode("5dab087e624a8a4b79e17f8b83800ee66f3bb1292618b6fd1c2f8b27ff88e0eb");
        let alice_pub =
            base16::unsafe_decode("8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a");
        let nonce = base16::unsafe_decode("69696ee955b62b73cd62bda875fc73d68219e0036b7a0b37");
        let message = base16::unsafe_decode(
            "be075fc53c81f2d5cf141316ebeb0c7b5228c52a4c62cbd44b66849b64244ffce5ecbaaf33bd751a1ac728d45e6c61296cdc3c01233561f41db66cce314adb310e3be8250c46f06dceea3a7fa1348057e2f6556ad6b1318a024a838f21af1fde048977eb48f59ffd4924ca1c60902e52f0a089bc76897040e082f937763848645e0705",
        );
        let cipher = encrypt(&alice_pub, &bob_sec, &nonce, &message).expect("box encrypt");
        assert_eq!(
            base16::encode(&cipher),
            "f3ffc7703f9400e52a7dfb4b3d3305d98e993b9f48681273c29650ba32fc76ce48332ea7164d96a4476fb8c531a1186ac0dfc17c98dce87b4da7f011ec48c97271d2c20f9b928fe2270d6fb863d51738b48eeee314a7cc8ab932164548e526ae90224368517acfeabd6bb3732bc0e9da99832b61ca01b6de56244a9e88d5f9b37973f622a43d14a6599b1f654cb45a74e355a5"
        );
    }

    #[test]
    fn decrypts() {
        let bob_pub =
            base16::unsafe_decode("de9edb7d7b7dc1b4d35b61c2ece435373f8343c85b78674dadfc7e146f882b4f");
        let alice_sec =
            base16::unsafe_decode("77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a");
        let nonce = base16::unsafe_decode("69696ee955b62b73cd62bda875fc73d68219e0036b7a0b37");
        let message = base16::unsafe_decode(
            "be075fc53c81f2d5cf141316ebeb0c7b5228c52a4c62cbd44b66849b64244ffce5ecbaaf33bd751a1ac728d45e6c61296cdc3c01233561f41db66cce314adb310e3be8250c46f06dceea3a7fa1348057e2f6556ad6b1318a024a838f21af1fde048977eb48f59ffd4924ca1c60902e52f0a089bc76897040e082f937763848645e0705",
        );
        let cipher = base16::unsafe_decode(
            "f3ffc7703f9400e52a7dfb4b3d3305d98e993b9f48681273c29650ba32fc76ce48332ea7164d96a4476fb8c531a1186ac0dfc17c98dce87b4da7f011ec48c97271d2c20f9b928fe2270d6fb863d51738b48eeee314a7cc8ab932164548e526ae90224368517acfeabd6bb3732bc0e9da99832b61ca01b6de56244a9e88d5f9b37973f622a43d14a6599b1f654cb45a74e355a5",
        );
        assert_eq!(decrypt(&bob_pub, &alice_sec, &nonce, &cipher).expect("box decrypt"), message);
    }

    #[test]
    fn gets_public() {
        let alice_sec =
            base16::unsafe_decode("77076d0a7318a57d3c16c17251b26645df4c2f87ebc0992ab177fba51db92c2a");
        let alice_pub =
            base16::unsafe_decode("8520f0098930a754748b7ddcb43ef75a0dbf3a0d26381af4eba4a98eaa9b4e6a");
        assert_eq!(to_public(&alice_sec), alice_pub);
    }
}
