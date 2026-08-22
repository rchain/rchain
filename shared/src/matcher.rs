//! String matchers (port of `shared/Matcher.scala`).

/// A prefix extractor (port of `Matcher.WithPrefix`).
pub struct WithPrefix<'a> {
    prefix: &'a str,
}

impl<'a> WithPrefix<'a> {
    pub fn new(prefix: &'a str) -> Self {
        WithPrefix { prefix }
    }

    /// Returns the suffix after `prefix` when `s` starts with it (port of `unapply`).
    pub fn unapply<'s>(&self, s: &'s str) -> Option<&'s str> {
        s.strip_prefix(self.prefix)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn matches_prefix_and_returns_suffix() {
        let wp = WithPrefix::new("foo-");
        assert_eq!(wp.unapply("foo-bar"), Some("bar"));
        assert_eq!(wp.unapply("bar-foo"), None);
    }
}
