//! Node version string (port of `web/VersionInfo.scala`).
//!
//! `VersionInfo.get` is a `val` computed from the sbt-buildinfo `BuildInfo` (`version`,
//! `gitHeadCommit`), which is generated at build time and has no Rust equivalent yet. The port
//! exposes the same formatting as a pure function over those two inputs. The http4s `service` is
//! deferred.

/// Format the node version string (port of `VersionInfo.get`).
pub fn get(version: &str, git_head_commit: Option<&str>) -> String {
    format!(
        "RChain Node {} ({})",
        version,
        git_head_commit.unwrap_or("commit # unknown")
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn get_formats_with_commit() {
        assert_eq!(get("1.0.0", Some("abc123")), "RChain Node 1.0.0 (abc123)");
    }

    #[test]
    fn get_uses_unknown_when_no_commit() {
        assert_eq!(get("1.0.0", None), "RChain Node 1.0.0 (commit # unknown)");
    }
}
