//! Genesis wallets-file parser (port of `casper/util/VaultParser.scala`).
//!
//! Reads a `<REV_address>,<balance>` wallets file used by the genesis ceremony to seed initial REV
//! accounts. The Scala's fs2 streaming read becomes a synchronous line read.

use std::fs;
use std::path::Path;

use rchain_rholang::util::rev_address::RevAddress;
use rchain_shared::refined::NonNegI64;

use crate::genesis::contracts::Vault;

/// Parse a wallets file of `<REV_address>,<balance>` lines into genesis vaults (port of
/// `VaultParser.parse(Path)`).
pub fn parse(vaults_path: &Path) -> Result<Vec<Vault>, String> {
    let content = fs::read_to_string(vaults_path)
        .map_err(|e| format!("FAILED PARSING WALLETS FILE: {}\n{}", vaults_path.display(), e))?;
    let mut vaults = Vec::new();
    for line in content.lines() {
        if line.trim().is_empty() {
            continue;
        }
        let vault = parse_line(line)
            .map_err(|e| format!("FAILED PARSING WALLETS FILE: {}\n{}", vaults_path.display(), e))?;
        vaults.push(vault);
    }
    Ok(vaults)
}

/// Parse a wallets file, returning an empty list when the file is absent (port of
/// `VaultParser.parse(String)`).
pub fn parse_if_exists(vaults_path: &Path) -> Result<Vec<Vault>, String> {
    if vaults_path.exists() {
        parse(vaults_path)
    } else {
        Ok(Vec::new())
    }
}

fn parse_line(line: &str) -> Result<Vault, String> {
    let line_format = "<REV_address>,<balance>";
    let (addr, balance) = line
        .split_once(',')
        .ok_or_else(|| format!("INVALID LINE FORMAT: `{line_format}`, actual: `{line}`"))?;

    // The Scala line regex is `^([1-9a-zA-Z]+),([0-9]+)` (unanchored): the address is one or more
    // of `[1-9a-zA-Z]`.
    if addr.is_empty()
        || !addr
            .bytes()
            .all(|b| (b'1'..=b'9').contains(&b) || b.is_ascii_alphabetic())
    {
        return Err(format!("INVALID LINE FORMAT: `{line_format}`, actual: `{line}`"));
    }

    let rev_address = RevAddress::parse(addr)
        .map_err(|e| format!("PARSE ERROR: {e}, `{line_format}`, actual: `{line}`"))?;
    let balance_value = balance
        .parse::<i64>()
        .map_err(|_| format!("INVALID WALLET BALANCE `{balance}`. Please put positive number."))?;
    let initial_balance = NonNegI64::try_from(balance_value).map_err(|_| {
        format!("INVALID WALLET BALANCE `{balance}`. Please put a non-negative number.")
    })?;
    Ok(Vault {
        rev_address,
        initial_balance,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_crypto::public_key::PublicKey;

    fn valid_address() -> String {
        let mut key = vec![0u8; 65];
        key[0] = 0x04;
        RevAddress::from_public_key(&PublicKey::new(key))
            .unwrap()
            .to_base58()
    }

    fn temp_dir(name: &str) -> std::path::PathBuf {
        let dir = std::env::temp_dir().join(format!("rchain_vault_{name}_{}", std::process::id()));
        let _ = fs::remove_dir_all(&dir);
        dir
    }

    #[test]
    fn parses_valid_line() {
        let addr = valid_address();
        let vault = parse_line(&format!("{addr},123")).unwrap();
        assert_eq!(vault.initial_balance, NonNegI64::try_from(123).unwrap());
        assert_eq!(vault.rev_address.to_base58(), addr);
    }

    #[test]
    fn parses_file_skipping_empty_lines() {
        let addr = valid_address();
        let dir = temp_dir("parse_file");
        fs::create_dir_all(&dir).unwrap();
        let file = dir.join("wallets.txt");
        fs::write(&file, format!("{addr},1\n\n{addr},2\n   \n")).unwrap();
        let vaults = parse(&file).unwrap();
        assert_eq!(vaults.len(), 2);
        assert_eq!(vaults[0].initial_balance, NonNegI64::try_from(1).unwrap());
        assert_eq!(vaults[1].initial_balance, NonNegI64::try_from(2).unwrap());
        fs::remove_dir_all(&dir).unwrap();
    }

    #[test]
    fn rejects_invalid_line_format() {
        assert!(parse_line("no-comma-here")
            .unwrap_err()
            .contains("INVALID LINE FORMAT"));
        assert!(parse_line(",123").unwrap_err().contains("INVALID LINE FORMAT"));
    }

    #[test]
    fn rejects_invalid_address() {
        // '0' is not in the `[1-9a-zA-Z]` address alphabet.
        let err = parse_line("0invalid,123").unwrap_err();
        assert!(err.contains("INVALID LINE FORMAT"), "got: {err}");
    }

    #[test]
    fn rejects_invalid_balance() {
        let addr = valid_address();
        let err = parse_line(&format!("{addr},not-a-number")).unwrap_err();
        assert!(err.contains("INVALID WALLET BALANCE"), "got: {err}");
    }

    #[test]
    fn rejects_negative_balance() {
        let addr = valid_address();
        let err = parse_line(&format!("{addr},-5")).unwrap_err();
        assert!(err.contains("non-negative"), "got: {err}");
    }

    #[test]
    fn parse_if_exists_returns_empty_for_missing_file() {
        let dir = temp_dir("missing");
        assert!(parse_if_exists(&dir.join("nope.txt")).unwrap().is_empty());
    }
}
