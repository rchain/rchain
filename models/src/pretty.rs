//! Pretty-printing string helpers (port of the `PrettyUtils` half of `models/Pretty.scala`).
//!
//! The Magnolia-derived `Pretty[A]` typeclass is deferred; only the pure string-literal escaping
//! and indentation helpers are ported.

/// Escape a string into a double-quoted literal (port of `PrettyUtils.literalize`).
///
/// When `unicode` is set, non-ASCII characters (`> '~'`) are escaped as `\uXXXX`.
pub fn literalize(s: &str, unicode: bool) -> String {
    let mut sb = String::with_capacity(s.len() + 2);
    sb.push('"');
    for c in s.chars() {
        escape_char(c, &mut sb, unicode);
    }
    sb.push('"');
    sb
}

fn escape_char(c: char, sb: &mut String, unicode: bool) {
    match c {
        '"' => sb.push_str("\\\""),
        '\\' => sb.push_str("\\\\"),
        '\u{0008}' => sb.push_str("\\b"),
        '\u{000C}' => sb.push_str("\\f"),
        '\n' => sb.push_str("\\n"),
        '\r' => sb.push_str("\\r"),
        '\t' => sb.push_str("\\t"),
        c => {
            if c < ' ' || (c > '~' && unicode) {
                sb.push_str(&format!("\\u{:04x}", c as u32));
            } else {
                sb.push(c);
            }
        }
    }
}

/// Format a list of strings as a parenthesised, multi-line, indented block (port of
/// `PrettyUtils.parenthesisedStrings`).
pub fn parenthesised_strings(param_strings: &[String], indent_level: usize) -> String {
    if param_strings.is_empty() {
        "()".to_string()
    } else {
        let start = format!("(\n{}", indent(indent_level + 1));
        let sep = format!(",\n{}", indent(indent_level + 1));
        let end = format!("\n{})", indent(indent_level));
        format!("{start}{}{end}", param_strings.join(&sep))
    }
}

fn indent(indent_level: usize) -> String {
    "  ".repeat(indent_level)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn literalize_quotes_plain_string() {
        assert_eq!(literalize("abc", true), "\"abc\"");
    }

    #[test]
    fn literalize_escapes_special_chars() {
        // a quote, a backslash, a newline, a tab.
        assert_eq!(literalize("\"\\\n\t", true), "\"\\\"\\\\\\n\\t\"");
    }

    #[test]
    fn literalize_escapes_non_ascii_when_unicode() {
        assert_eq!(literalize("é", true), "\"\\u00e9\"");
        assert_eq!(literalize("é", false), "\"é\"");
    }

    #[test]
    fn parenthesised_strings_empty_is_unit() {
        assert_eq!(parenthesised_strings(&[], 0), "()");
    }

    #[test]
    fn parenthesised_strings_formats_multiline() {
        let out = parenthesised_strings(&["a".to_string(), "b".to_string()], 0);
        assert_eq!(out, "(\n  a,\n  b\n)");
    }
}
