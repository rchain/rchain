//! SDK error types (port of `sdk/error/FatalError.scala`).

use std::fmt;

/// An error that should not be handled and should cause the main process to exit (port of
/// `FatalError`).
///
/// In the Scala it extends `Throwable` directly to mark it as a "lower" level error than
/// `Exception`. The Rust port models it as a `std::error::Error` with an optional source cause.
#[derive(Debug)]
pub struct FatalError {
    message: String,
    cause: Option<Box<dyn std::error::Error>>,
}

impl FatalError {
    /// Create a `FatalError` with a message (port of `FatalError.apply(message)`).
    pub fn new(message: impl Into<String>) -> Self {
        FatalError {
            message: message.into(),
            cause: None,
        }
    }

    /// Create a `FatalError` with a message and a lower-level cause (port of
    /// `FatalError.apply(message, cause)`).
    pub fn with_cause(message: impl Into<String>, cause: impl std::error::Error + 'static) -> Self {
        FatalError {
            message: message.into(),
            cause: Some(Box::new(cause)),
        }
    }
}

impl fmt::Display for FatalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for FatalError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.cause.as_deref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::error::Error;

    #[test]
    fn fatal_error_displays_its_message() {
        let e = FatalError::new("boom");
        assert_eq!(e.to_string(), "boom");
    }

    #[test]
    fn fatal_error_carries_a_source_cause() {
        let cause = std::io::Error::other("underlying");
        let e = FatalError::with_cause("boom", cause);
        assert_eq!(e.to_string(), "boom");
        let source = e.source().expect("source should be present");
        assert_eq!(source.to_string(), "underlying");
    }

    #[test]
    fn fatal_error_without_cause_has_no_source() {
        let e = FatalError::new("boom");
        assert!(e.source().is_none());
    }
}
