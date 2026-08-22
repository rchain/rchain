//! Genesis contract parameter types + rholang source builders (port of
//! `casper/genesis/contracts/`).
//!
//! The `.rho`/`.rhox` template *loading* (`CompiledRholangSource`/`CompiledRholangTemplate`) is
//! deferred; only the parameter types and the pure source-string builders are ported here.

use rchain_crypto::public_key::PublicKey;
use rchain_rholang::util::rev_address::RevAddress;
use rchain_shared::base16;
use rchain_shared::refined::NonNegI64;

/// A genesis validator (port of `contracts.Validator`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Validator {
    pub pk: PublicKey,
    pub stake: NonNegI64,
}

/// A genesis REV vault (port of `contracts.Vault`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Vault {
    pub rev_address: RevAddress,
    pub initial_balance: NonNegI64,
}

/// Proof-of-stake genesis parameters (port of `contracts.ProofOfStake`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProofOfStake {
    pub minimum_bond: i64,
    pub maximum_bond: i64,
    pub validators: Vec<Validator>,
    pub epoch_length: i32,
    pub quarantine_length: i32,
    pub number_of_active_validators: i32,
    pub pos_multi_sig_public_keys: Vec<String>,
    pub pos_multi_sig_quorum: i32,
    pub pos_vault_pub_key: String,
}

impl ProofOfStake {
    /// The rholang `initialBonds` map literal (port of `ProofOfStake.initialBonds`).
    pub fn initial_bonds(validators: &[Validator]) -> String {
        let mut sorted: Vec<&Validator> = validators.iter().collect();
        sorted.sort_by(|a, b| a.pk.cmp(&b.pk));
        let entries: Vec<String> = sorted
            .iter()
            .map(|v| {
                let pk = base16::encode(v.pk.bytes());
                format!(" \"{}\".hexToBytes() : {}", pk, i64::from(v.stake))
            })
            .collect();
        format!("{{{}}}", entries.join(", "))
    }

    /// The rholang `posMultiSigPublicKeys` list literal (port of `ProofOfStake.publicKeys`).
    pub fn public_keys(keys: &[String]) -> String {
        let indent_brackets = 12;
        let indent_keys = indent_brackets + 2;
        let items: Vec<String> = keys
            .iter()
            .map(|pk| format!("{}\"{}\".hexToBytes()", " ".repeat(indent_keys), pk))
            .collect();
        format!("[\n{}\n{}]", items.join(",\n"), " ".repeat(indent_brackets))
    }
}

/// Registry genesis parameters (port of `contracts.Registry`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Registry {
    pub system_contract_pub_key: String,
}

/// The rholang source of a REV-vault generation deploy (port of `RevGenerator.apply`).
pub fn rev_generator_code(vaults: &[Vault], is_last_batch: bool) -> String {
    let vault_balance_list = vaults
        .iter()
        .map(|v| format!("(\"{}\", {})", v.rev_address.to_base58(), i64::from(v.initial_balance)))
        .collect::<Vec<_>>()
        .join(", ");
    let init_continue = if is_last_batch { "" } else { "| initContinue!()" };

    let template = r#" new rl(`rho:registry:lookup`), revVaultCh in {
   rl!(`rho:rchain:revVault`, *revVaultCh) |
   for (@(_, RevVault) <- revVaultCh) {
     new revVaultInitCh in {
       @RevVault!("init", *revVaultInitCh) |
       for (TreeHashMap, @vaultMap, initVault, initContinue <- revVaultInitCh) {
         match [$VAULT_BALANCE_LIST] {
           vaults => {
             new iter in {
               contract iter(@[(addr, initialBalance) ... tail]) = {
                  iter!(tail) |
                  new vault, setDoneCh in {
                    initVault!(*vault, addr, initialBalance) |
                    TreeHashMap!("set", vaultMap, addr, *vault, *setDoneCh) |
                    for (_ <- setDoneCh) { Nil }
                  }
               } |
               iter!(vaults) $INIT_CONTINUE
             }
           }
         }
       }
     }
   }
 }"#;

    template
        .replace("$VAULT_BALANCE_LIST", &vault_balance_list)
        .replace("$INIT_CONTINUE", init_continue)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn validator(byte: u8, stake: i64) -> Validator {
        Validator {
            pk: PublicKey::new(vec![byte; 65]),
            stake: NonNegI64::try_from(stake).unwrap(),
        }
    }

    #[test]
    fn initial_bonds_sorts_by_public_key() {
        let validators = vec![validator(2, 200), validator(1, 100)];
        let bonds = ProofOfStake::initial_bonds(&validators);
        // Validator 1 comes first (sorted by pk bytes).
        assert!(bonds.starts_with('{'));
        assert!(bonds.contains(": 100"));
        assert!(bonds.contains(": 200"));
        assert!(bonds.contains("0101"));
        assert!(bonds.contains("0202"));
    }

    #[test]
    fn initial_bonds_uses_signed_public_key_order() {
        // 0x80 sorts BEFORE 0x01 under signed-byte order (i8: -128 < 1) but AFTER under unsigned
        // order (u8: 128 > 1). This discriminates the canonical (signed) ordering from the raw
        // `bytes().cmp()` ordering it replaces.
        let validators = vec![validator(0x01, 100), validator(0x80, 200)];
        let bonds = ProofOfStake::initial_bonds(&validators);
        let pos_200 = bonds.find(": 200").expect("stake 200 present");
        let pos_100 = bonds.find(": 100").expect("stake 100 present");
        assert!(pos_200 < pos_100, "0x80 must sort before 0x01 (signed order): {bonds}");
    }

    #[test]
    fn public_keys_builds_list_literal() {
        let keys = vec!["aabb".to_string(), "ccdd".to_string()];
        let literal = ProofOfStake::public_keys(&keys);
        assert!(literal.starts_with('['));
        assert!(literal.contains("\"aabb\".hexToBytes()"));
        assert!(literal.contains("\"ccdd\".hexToBytes()"));
    }

    #[test]
    fn rev_generator_includes_batches_and_continue() {
        let rev_address = RevAddress::from_public_key(&PublicKey::new(vec![1; 65])).unwrap();
        let vaults = vec![Vault {
            rev_address: rev_address.clone(),
            initial_balance: NonNegI64::try_from(42).unwrap(),
        }];
        let not_last = rev_generator_code(&vaults, false);
        assert!(not_last.contains("initContinue!()"));

        let last = rev_generator_code(&vaults, true);
        assert!(!last.contains("initContinue!()"));
        assert!(last.contains(&rev_address.to_base58()));
        assert!(last.contains("42"));
    }
}
