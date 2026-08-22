//! Error-chain utilities (port of `shared/ThrowableOps.scala`).

use std::error::Error;

/// Extension methods on errors (port of `RichThrowable`).
pub trait ThrowableOps {
    /// Whether this error or any cause's message contains `str` (port of `containsMessageWith`).
    fn contains_message_with(&self, str: &str) -> bool;

    /// Fold over the cause chain, from this error down through its causes (port of `fold`).
    fn fold<B>(&self, z: B, f: impl Fn(B, &(dyn Error + 'static)) -> B) -> B;

    /// Messages along the cause chain, prefixing each cause's message with `prefix` (port of
    /// `toMessageList`).
    fn to_message_list(&self, prefix: &str) -> Vec<String>;
}

impl<E: Error + 'static> ThrowableOps for E {
    fn contains_message_with(&self, str: &str) -> bool {
        let mut current: Option<&(dyn Error + 'static)> = Some(self);
        while let Some(t) = current {
            if t.to_string().contains(str) {
                return true;
            }
            current = t.source();
        }
        false
    }

    fn fold<B>(&self, z: B, f: impl Fn(B, &(dyn Error + 'static)) -> B) -> B {
        let mut acc = z;
        let mut current: Option<&(dyn Error + 'static)> = Some(self);
        while let Some(t) = current {
            acc = f(acc, t);
            current = t.source();
        }
        acc
    }

    fn to_message_list(&self, prefix: &str) -> Vec<String> {
        self.fold(Vec::new(), |mut ms, t| {
            let msg = t.to_string();
            if !msg.trim().is_empty() {
                let rendered = if ms.is_empty() {
                    msg
                } else {
                    format!("{prefix}{msg}")
                };
                ms.push(rendered);
            }
            ms
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fmt;

    #[derive(Debug)]
    struct TestError {
        msg: String,
        cause: Option<Box<dyn Error>>,
    }

    impl TestError {
        fn new(msg: &str) -> Self {
            TestError {
                msg: msg.to_string(),
                cause: None,
            }
        }
        fn with_cause(msg: &str, cause: impl Error + 'static) -> Self {
            TestError {
                msg: msg.to_string(),
                cause: Some(Box::new(cause)),
            }
        }
    }

    impl fmt::Display for TestError {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{}", self.msg)
        }
    }

    impl Error for TestError {
        fn source(&self) -> Option<&(dyn Error + 'static)> {
            self.cause.as_deref()
        }
    }

    #[test]
    fn contains_message_with_searches_cause_chain() {
        let e = TestError::with_cause("root", TestError::new("inner"));
        assert!(e.contains_message_with("root"));
        assert!(e.contains_message_with("inner"));
        assert!(!e.contains_message_with("absent"));
    }

    #[test]
    fn fold_walks_the_cause_chain() {
        let e = TestError::with_cause("root", TestError::new("inner"));
        let count = e.fold(0, |acc, _| acc + 1);
        assert_eq!(count, 2);
    }

    #[test]
    fn to_message_list_prefixes_causes() {
        let e = TestError::with_cause("root", TestError::new("inner"));
        assert_eq!(e.to_message_list("Caused by: "), vec!["root", "Caused by: inner"]);
    }
}
