//! Base16 CLI value converter (port of `Base16Converter.scala`).

/// Parse a base16-encoded CLI argument (port of `Base16Converter.parse`).
///
/// `args` is a list of `(option_name, values)` pairs, flattened to individual `(name, value)`
/// entries. Returns `Ok(None)` when no value is supplied, `Ok(Some(bytes))` for exactly one valid
/// hex value, and `Err(msg)` otherwise.
pub fn parse(args: &[(String, Vec<String>)]) -> Result<Option<Vec<u8>>, String> {
    let flat: Vec<(&str, &str)> = args
        .iter()
        .flat_map(|(name, vals)| vals.iter().map(move |v| (name.as_str(), v.as_str())))
        .collect();

    match flat.as_slice() {
        [] => Ok(None),
        [(name, value)] => rchain_shared::base16::decode(value)
            .map(Some)
            .ok_or_else(|| format!("Error parsing {name}. Invalid base16 encoding.")),
        _ => Err("Expecting a single argument encoded as base16.".to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_returns_error_for_bad_characters() {
        let samples = [
            "", "0", "ff", "abc", "0123456789abcdefABCDEF", "xyz", "12z4", "0x12", " ", "GG",
        ];
        for s in samples {
            let has_invalid = s.chars().any(|c| !c.is_ascii_hexdigit()) || s.len() % 2 != 0;
            let result = parse(&[("".to_string(), vec![s.to_string()])]);
            assert_eq!(result.is_err(), has_invalid, "input: {s:?}");
        }
    }

    #[test]
    fn parse_decodes_a_single_hex_value() {
        assert_eq!(
            parse(&[("key".to_string(), vec!["ff".to_string()])]).unwrap(),
            Some(vec![0xff])
        );
        assert_eq!(parse(&[("key".to_string(), vec![])]) .unwrap(), None);
        assert!(parse(&[("key".to_string(), vec!["a".to_string(), "b".to_string()])]).is_err());
    }
}
