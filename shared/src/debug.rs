//! Debug helpers (port of `shared/Debug.scala`).
//!
//! The Scala `sourcecode` macros (`print`/`printUnsafe`/`string`, which capture the expression
//! source and source location) become `macro_rules!`: [`debug_string!`] and [`debug_println!`] use
//! `stringify!` for the expression source and `file!()`/`line!()` (plus `module_path!()` as the
//! enclosing-scope stand-in, since Rust has no stable `function!()` macro) for the location.

use std::sync::OnceLock;

/// The substring after the last occurrence of `pattern`, or the whole string when absent (port of
/// `Debug.suffixAfterLast`).
pub fn suffix_after_last<'a>(pattern: &str, string: &'a str) -> &'a str {
    string
        .rsplit_once(pattern)
        .map(|(_, after)| after)
        .unwrap_or(string)
}

fn startup() -> &'static std::time::Instant {
    static START: OnceLock<std::time::Instant> = OnceLock::new();
    START.get_or_init(std::time::Instant::now)
}

/// Build the debug string for a list of `(source, rendered_value)` pairs (port of `Debug.string`).
pub fn string(values: &[(&str, String)], module: &str, file: &str, line: u32) -> String {
    let name = suffix_after_last("::", module);
    let filename = suffix_after_last("/", file);
    let value_indent = "           "; // 11 spaces (Scala's value indent)
    let values_text = if values.is_empty() {
        String::new()
    } else {
        format!(
            "\n{}",
            values
                .iter()
                .map(|(src, val)| format!("{value_indent}{src} = {val}"))
                .collect::<Vec<_>>()
                .join("\n")
        )
    };
    let timestamp = startup().elapsed().as_secs_f64();
    format!("{timestamp:8.3} {name}({filename}:{line}){values_text}")
}

/// Produce a `Debug.string`-style message for the given expressions, capturing each expression's
/// source (`stringify!`) and `Debug` rendering, plus the call site (`module_path!`/`file!`/`line!`).
#[macro_export]
macro_rules! debug_string {
    ($($expr:expr),* $(,)?) => {{
        $crate::debug::string(
            &[$( (stringify!($expr), format!("{:?}", $expr)) ),*],
            module_path!(),
            file!(),
            line!(),
        )
    }};
}

/// Print a [`debug_string!`] message to stdout (port of `Debug.printUnsafe`).
#[macro_export]
macro_rules! debug_println {
    ($($expr:expr),* $(,)?) => {{
        println!("{}", $crate::debug_string!($($expr),*));
    }};
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn suffix_after_last_extracts_tail() {
        assert_eq!(suffix_after_last(".", "a.b.c"), "c");
        assert_eq!(suffix_after_last("/", "path/to/file"), "file");
    }

    #[test]
    fn suffix_after_last_returns_whole_when_absent() {
        assert_eq!(suffix_after_last(".", "no_dots"), "no_dots");
    }

    #[test]
    fn string_renders_source_and_value() {
        let out = string(
            &[("x", "42".to_string()), ("y", "\"hi\"".to_string())],
            "rchain_shared::debug::tests",
            "/src/debug.rs",
            12,
        );
        assert!(out.contains("tests(debug.rs:12)"), "{out}");
        assert!(out.contains("           x = 42"), "{out}");
        assert!(out.contains("           y = \"hi\""), "{out}");
    }

    #[test]
    fn debug_string_macro_captures_source_and_value() {
        let x = 42;
        let out = debug_string!(x, 1 + 2);
        assert!(out.contains("x = 42"), "{out}");
        assert!(out.contains("1 + 2 = 3"), "{out}");
    }
}
