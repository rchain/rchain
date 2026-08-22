//! Node effects (port of `coop.rchain.node.effects`).

pub mod console_io;
pub mod repl_client;

pub use console_io::{ConsoleIo, NopConsoleIo, RustylineConsole, StdioConsole};
pub use repl_client::{GrpcReplClient, ReplClient};
