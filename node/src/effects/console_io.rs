//! Console I/O interface (port of `effects/ConsoleIO.scala`).

use rustyline::completion::{Completer as CompleterTrait, Pair};
use rustyline::error::ReadlineError;
use rustyline::highlight::MatchingBracketHighlighter;
use rustyline::hint::HistoryHinter;
use rustyline::history::DefaultHistory;
use rustyline::validate::MatchingBracketValidator;
use rustyline::{Completer, Helper, Highlighter, Hinter, Validator};
use rustyline::{Context, Editor, Result as RlResult};

use rchain_shared::string_ops::ColoredString;

/// Console I/O (port of `ConsoleIO[F]`; the `F[_]` effect is simplified to synchronous calls).
pub trait ConsoleIo {
    /// Read a line, returning `None` on EOF (the Scala `null`).
    fn read_line(&mut self) -> Option<String>;
    fn read_password(&mut self, prompt: &str) -> String;
    fn println(&mut self, s: &str);
    fn println_colored(&mut self, s: &ColoredString);
    fn update_completion(&mut self, history: &[String]);
    fn close(&mut self);
}

/// A stdin/stdout console (port of `effects.consoleIO`/`JLineConsoleIO`; the jline line-editing
/// and prompt are not reproduced — only plain line reads/writes).
#[derive(Default)]
pub struct StdioConsole;

impl ConsoleIo for StdioConsole {
    fn read_line(&mut self) -> Option<String> {
        let mut line = String::new();
        match std::io::stdin().read_line(&mut line) {
            Ok(0) => None,
            Ok(_) => {
                if line.ends_with('\n') {
                    line.pop();
                    if line.ends_with('\r') {
                        line.pop();
                    }
                }
                Some(line)
            }
            Err(_) => None,
        }
    }

    fn read_password(&mut self, prompt: &str) -> String {
        use std::io::Write;
        let mut stdout = std::io::stdout();
        let _ = stdout.write_all(prompt.as_bytes());
        let _ = stdout.flush();
        let mut line = String::new();
        let _ = std::io::stdin().read_line(&mut line);
        line.trim().to_string()
    }

    fn println(&mut self, s: &str) {
        println!("{s}");
    }

    fn println_colored(&mut self, s: &ColoredString) {
        println!("{}", s.colorize());
    }

    fn update_completion(&mut self, _history: &[String]) {}

    fn close(&mut self) {}
}

/// A tab-completer over a fixed keyword list (port of jline's `StringsCompleter`).
struct KeywordCompleter {
    keywords: Vec<String>,
}

impl CompleterTrait for KeywordCompleter {
    type Candidate = Pair;

    fn complete(&self, line: &str, pos: usize, _ctx: &Context<'_>) -> RlResult<(usize, Vec<Pair>)> {
        let start = line[..pos]
            .rfind(char::is_whitespace)
            .map(|i| i + 1)
            .unwrap_or(0);
        let word = &line[start..pos];
        let matches: Vec<Pair> = self
            .keywords
            .iter()
            .filter(|k| k.starts_with(word))
            .map(|k| Pair {
                display: k.clone(),
                replacement: k.clone(),
            })
            .collect();
        Ok((start, matches))
    }
}

/// The rustyline helper stack (completer + hinter + highlighter + validator).
#[derive(Completer, Helper, Hinter, Highlighter, Validator)]
struct ReplHelper {
    #[rustyline(Completer)]
    completer: KeywordCompleter,
    #[rustyline(Hinter)]
    hinter: HistoryHinter,
    #[rustyline(Highlighter)]
    highlighter: MatchingBracketHighlighter,
    #[rustyline(Validator)]
    validator: MatchingBracketValidator,
}

impl ReplHelper {
    fn new(keywords: Vec<String>) -> Self {
        ReplHelper {
            completer: KeywordCompleter { keywords },
            hinter: HistoryHinter {},
            highlighter: MatchingBracketHighlighter::new(),
            validator: MatchingBracketValidator::new(),
        }
    }
}

/// A rustyline-backed console with a prompt, line-editing, history, and tab-completion (the Rust
/// spelling of the Scala jline `JLineConsoleIO`).
pub struct RustylineConsole {
    editor: Editor<ReplHelper, DefaultHistory>,
}

impl RustylineConsole {
    pub fn new() -> Result<Self, String> {
        let mut editor = Editor::<ReplHelper, DefaultHistory>::new().map_err(|e| e.to_string())?;
        editor.set_helper(Some(ReplHelper::new(Vec::new())));
        Ok(RustylineConsole { editor })
    }
}

impl ConsoleIo for RustylineConsole {
    fn read_line(&mut self) -> Option<String> {
        match self.editor.readline("rholang $ ") {
            Ok(line) => Some(line),
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => None,
            Err(_) => None,
        }
    }

    fn read_password(&mut self, prompt: &str) -> String {
        use std::io::Write;
        let mut stdout = std::io::stdout();
        let _ = stdout.write_all(prompt.as_bytes());
        let _ = stdout.flush();
        let mut line = String::new();
        let _ = std::io::stdin().read_line(&mut line);
        line.trim().to_string()
    }

    fn println(&mut self, s: &str) {
        println!("{s}");
    }

    fn println_colored(&mut self, s: &ColoredString) {
        println!("{}", s.colorize());
    }

    fn update_completion(&mut self, history: &[String]) {
        self.editor.set_helper(Some(ReplHelper::new(history.to_vec())));
    }

    fn close(&mut self) {}
}

/// A no-op console (port of `NOPConsoleIO`).
#[derive(Default)]
pub struct NopConsoleIo;

impl ConsoleIo for NopConsoleIo {
    fn read_line(&mut self) -> Option<String> {
        Some(String::new())
    }

    fn read_password(&mut self, _prompt: &str) -> String {
        String::new()
    }

    fn println(&mut self, _s: &str) {}

    fn println_colored(&mut self, _s: &ColoredString) {}

    fn update_completion(&mut self, _history: &[String]) {}

    fn close(&mut self) {}
}
