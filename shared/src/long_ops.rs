//! Long extensions (port of `shared/LongOps.scala`).

/// Formats a size in bytes to a human-readable string (port of `LongOps.toHumanReadableSize`).
///
/// Uses 1 KB = 1024 bytes (not 1000). E.g. `512` -> `"512 B"`, `1536` -> `"1.5 KB"`.
pub fn to_human_readable_size(value: i64) -> String {
    if value < 1024 {
        format!("{} B", value)
    } else {
        // Order of magnitude in units of 1024: the highest set bit index divided by 10
        // (2^10 = 1024).
        let z = (63 - value.leading_zeros()) / 10;
        let scaled = value as f64 / (1_i64 << (z * 10)) as f64;
        let unit = b" KMGTPE"[z as usize] as char;
        format!("{:.1} {}B", scaled, unit)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn formats_bytes_below_kb() {
        assert_eq!(to_human_readable_size(0), "0 B");
        assert_eq!(to_human_readable_size(512), "512 B");
        assert_eq!(to_human_readable_size(1023), "1023 B");
    }

    #[test]
    fn formats_kilo_and_mega() {
        assert_eq!(to_human_readable_size(1024), "1.0 KB");
        assert_eq!(to_human_readable_size(1536), "1.5 KB");
        assert_eq!(to_human_readable_size(1024 * 1024), "1.0 MB");
        assert_eq!(to_human_readable_size(1536 * 1024), "1.5 MB");
    }

    #[test]
    fn formats_larger_units() {
        assert_eq!(to_human_readable_size(1024 * 1024 * 1024), "1.0 GB");
        assert_eq!(to_human_readable_size(1024_i64.pow(4)), "1.0 TB");
        assert_eq!(to_human_readable_size(1024_i64.pow(5)), "1.0 PB");
        assert_eq!(to_human_readable_size(1024_i64.pow(6)), "1.0 EB");
    }
}
