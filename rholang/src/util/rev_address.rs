//! REV address derivation (port of `interpreter/util/RevAddress.scala` + `AddressTools.scala`).

use rchain_crypto::hash::{blake2b256, keccak256};
use rchain_crypto::public_key::PublicKey;
use rchain_models::ast::GPrivate;
use rchain_models::validator::LENGTH as VALIDATOR_LENGTH;
use rchain_shared::base16;

use super::base58;

const COIN_ID: &str = "000000";
const VERSION: &str = "00";
const CHECKSUM_LENGTH: usize = 4;
const KEY_HASH_LENGTH: usize = 32;
const ETH_ADDRESS_LENGTH: usize = 40;

/// A derived address: `prefix ++ keyHash ++ checksum` (port of `Address`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Address {
    prefix: Vec<u8>,
    key_hash: Vec<u8>,
    checksum: Vec<u8>,
}

impl Address {
    pub fn to_base58(&self) -> String {
        let mut address = self.prefix.clone();
        address.extend_from_slice(&self.key_hash);
        address.extend_from_slice(&self.checksum);
        base58::encode(&address)
    }
}

/// Derives `Address`es from public keys / unforgeables and parses/validates them (port of
/// `AddressTools`).
pub struct AddressTools {
    prefix: Vec<u8>,
    key_length: usize,
    checksum_length: usize,
}

impl AddressTools {
    pub fn new(prefix: Vec<u8>, key_length: usize, checksum_length: usize) -> Self {
        AddressTools {
            prefix,
            key_length,
            checksum_length,
        }
    }

    fn compute_checksum(&self, to_check: &[u8]) -> Vec<u8> {
        blake2b256::hash(to_check)
            .into_iter()
            .take(self.checksum_length)
            .collect()
    }

    pub fn from_public_key(&self, pk: &PublicKey) -> Option<Address> {
        if self.key_length == pk.bytes().len() {
            let eth_hex = base16::encode(&keccak256::hash(&pk.bytes()[1..]));
            let eth_address = &eth_hex[eth_hex.len() - ETH_ADDRESS_LENGTH..];
            self.from_eth_address(eth_address)
        } else {
            None
        }
    }

    pub fn from_eth_address(&self, eth_address: &str) -> Option<Address> {
        let stripped = eth_address.strip_prefix("0x").unwrap_or(eth_address);
        if stripped.len() == ETH_ADDRESS_LENGTH {
            let key_hash = keccak256::hash(&base16::unsafe_decode(stripped));
            let mut payload = self.prefix.clone();
            payload.extend_from_slice(&key_hash);
            let checksum = self.compute_checksum(&payload);
            Some(Address {
                prefix: self.prefix.clone(),
                key_hash,
                checksum,
            })
        } else {
            None
        }
    }

    pub fn from_unforgeable(&self, gprivate: &GPrivate) -> Address {
        let key_hash = keccak256::hash(&gprivate.id);
        let mut payload = self.prefix.clone();
        payload.extend_from_slice(&key_hash);
        let checksum = self.compute_checksum(&payload);
        Address {
            prefix: self.prefix.clone(),
            key_hash,
            checksum,
        }
    }

    pub fn parse(&self, address: &str) -> Result<Address, String> {
        let decoded = base58::decode(address).ok_or("Invalid Base58 encoding")?;
        let checksum_start = self.prefix.len() + KEY_HASH_LENGTH;
        let address_length = self.prefix.len() + KEY_HASH_LENGTH + self.checksum_length;

        if decoded.len() != address_length {
            return Err("Invalid address length".to_string());
        }
        let (payload, checksum) = decoded.split_at(checksum_start);
        if self.compute_checksum(payload) != checksum {
            return Err("Invalid checksum".to_string());
        }
        let (actual_prefix, key_hash) = payload.split_at(self.prefix.len());
        if actual_prefix != self.prefix {
            return Err("Invalid prefix".to_string());
        }
        Ok(Address {
            prefix: self.prefix.clone(),
            key_hash: key_hash.to_vec(),
            checksum: checksum.to_vec(),
        })
    }

    pub fn is_valid(&self, address: &str) -> bool {
        self.parse(address).is_ok()
    }
}

/// A REV address (port of `RevAddress`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RevAddress(Address);

impl RevAddress {
    pub fn to_base58(&self) -> String {
        self.0.to_base58()
    }

    fn tools() -> AddressTools {
        let prefix = base16::unsafe_decode(&format!("{COIN_ID}{VERSION}"));
        AddressTools::new(prefix, VALIDATOR_LENGTH, CHECKSUM_LENGTH)
    }

    pub fn from_deployer_id(deployer_id: &[u8]) -> Option<RevAddress> {
        Self::from_public_key(&PublicKey::new(deployer_id.to_vec()))
    }

    pub fn from_public_key(pk: &PublicKey) -> Option<RevAddress> {
        Self::tools().from_public_key(pk).map(RevAddress)
    }

    pub fn from_unforgeable(gprivate: &GPrivate) -> RevAddress {
        RevAddress(Self::tools().from_unforgeable(gprivate))
    }

    pub fn parse(address: &str) -> Result<RevAddress, String> {
        Self::tools().parse(address).map(RevAddress)
    }

    pub fn is_valid(address: &str) -> bool {
        Self::parse(address).is_ok()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_round_trips_public_key() {
        // 65-byte key (0x04 uncompressed prefix + 64 bytes).
        let mut key = vec![0u8; 65];
        key[0] = 0x04;
        for i in 0..64 {
            key[i + 1] = i as u8;
        }
        let pk = PublicKey::new(key);
        let addr = RevAddress::from_public_key(&pk).unwrap();
        let encoded = addr.to_base58();
        let parsed = RevAddress::parse(&encoded).unwrap();
        assert_eq!(parsed.to_base58(), encoded);
    }

    #[test]
    fn is_valid_rejects_garbage() {
        assert!(!RevAddress::is_valid("not-a-rev-address"));
    }
}
