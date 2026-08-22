//! String helpers (port of `shared/StringOps.scala`).

/// A string tagged with an ANSI color (port of `ColoredString`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ColoredString {
    pub str: String,
    pub color: String,
}

impl ColoredString {
    pub fn colorize(&self) -> String {
        format!("{}{}\u{001b}[0m", self.color, self.str)
    }
}

/// Color/format extensions on `str` (port of `StringColors`).
pub trait StringColors {
    fn green(&self) -> ColoredString;
    fn red(&self) -> ColoredString;
    fn blue(&self) -> ColoredString;
    fn is_number(&self) -> bool;
}

impl StringColors for str {
    fn green(&self) -> ColoredString {
        ColoredString {
            str: self.to_string(),
            color: "\u{001b}[32m".to_string(),
        }
    }

    fn red(&self) -> ColoredString {
        ColoredString {
            str: self.to_string(),
            color: "\u{001b}[31m".to_string(),
        }
    }

    fn blue(&self) -> ColoredString {
        ColoredString {
            str: self.to_string(),
            color: "\u{001b}[34m".to_string(),
        }
    }

    fn is_number(&self) -> bool {
        !self.is_empty() && self.parse::<f64>().is_ok()
    }
}

/// Wrap an expression in braces unless it is a plain number or already parenthesized (port of
/// `BracesOps.wrapWithBraces`).
pub fn wrap_with_braces(expr: &str) -> String {
    if expr.parse::<i64>().is_ok() || (expr.starts_with('(') && expr.ends_with(')')) {
        expr.to_string()
    } else {
        format!("({expr})")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn colorize_wraps_with_ansi_codes() {
        assert_eq!("hello".red().colorize(), "\u{001b}[31mhello\u{001b}[0m");
        assert_eq!("hello".blue().colorize(), "\u{001b}[34mhello\u{001b}[0m");
        assert_eq!("hello".green().colorize(), "\u{001b}[32mhello\u{001b}[0m");
    }

    #[test]
    fn is_number_detects_numbers() {
        assert!("42".is_number());
        assert!("-3.14".is_number());
        assert!(!"abc".is_number());
        assert!(!"".is_number());
    }

    #[test]
    fn wrap_with_braces_wraps_non_numbers() {
        assert_eq!(wrap_with_braces("42"), "42");
        assert_eq!(wrap_with_braces("(a)"), "(a)");
        assert_eq!(wrap_with_braces("a"), "(a)");
    }
}
