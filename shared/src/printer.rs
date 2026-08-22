//! Printer configuration (port of `shared/Printer.scala`).

const ENV_VAR: &str = "PRETTY_PRINTER_OUTPUT_TRIM_AFTER";

/// The pretty-printer output trim length, read from the `PRETTY_PRINTER_OUTPUT_TRIM_AFTER`
/// environment variable (port of `Printer.OUTPUT_CAPPED`).
///
/// `None` when the variable is absent, unparseable, or negative.
pub fn output_capped() -> Option<i32> {
    std::env::var(ENV_VAR).ok().and_then(|s| parse_trim(&s))
}

fn parse_trim(s: &str) -> Option<i32> {
    s.parse::<i32>().ok().filter(|&n| n >= 0)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_non_negative_values() {
        assert_eq!(parse_trim("0"), Some(0));
        assert_eq!(parse_trim("42"), Some(42));
    }

    #[test]
    fn rejects_negative_and_non_numeric() {
        assert_eq!(parse_trim("-1"), None);
        assert_eq!(parse_trim("abc"), None);
        assert_eq!(parse_trim(""), None);
    }
}
