//! Terminal mode detection (port of `shared/TerminalMode.scala`).

/// Whether the process has an interactive console (port of `TerminalMode.readMode`, the
/// `System.console() != null` check).
pub struct TerminalMode;

impl TerminalMode {
    pub fn read_mode() -> bool {
        use std::io::IsTerminal;
        std::io::stdin().is_terminal()
    }
}
