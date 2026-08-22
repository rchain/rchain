//! REPL interpreter (port of `runtime/ReplRuntime.scala`).

use rchain_shared::string_ops::StringColors;
use rchain_shared::terminal_mode::TerminalMode;

use crate::effects::{ConsoleIo, ReplClient};

/// The REPL banner logo (port of `ReplRuntime.logo`).
const LOGO: &str = "\n  ╦═╗┌─┐┬ ┬┌─┐┬┌┐┌  ╔╗╔┌─┐┌┬┐┌─┐  ╦═╗╔═╗╔═╗╦\n  ╠╦╝│  ├─┤├─┤││││  ║║║│ │ ││├┤   ╠╦╝║╣ ╠═╝║\n  ╩╚═└─┘┴ ┴┴ ┴┴┘└┘  ╝╚╝└─┘─┴┘└─┘  ╩╚═╚═╝╩  ╩═╝\n    ";

/// The REPL program (port of `ReplRuntime`).
pub struct ReplRuntime;

impl ReplRuntime {
    /// Run the interactive REPL loop (port of `replProgram`).
    pub fn repl_program(&self, console: &mut dyn ConsoleIo, client: &dyn ReplClient) {
        if TerminalMode::read_mode() {
            console.println_colored(&LOGO.red());
        }
        console.update_completion(&ReplRuntime::keywords());
        loop {
            let Some(line) = console.read_line() else {
                return;
            };
            match line.trim() {
                "" => console.println(""),
                ":q" => return,
                program => match client.run(program) {
                    Ok(result) => console.println_colored(&result.blue()),
                    Err(error) => {
                        console.println_colored(&format!("Error: {error}").red());
                        return;
                    }
                },
            }
        }
    }

    /// Evaluate files and print their results (port of `evalProgram`).
    pub fn eval_program(
        &self,
        console: &mut dyn ConsoleIo,
        client: &dyn ReplClient,
        file_names: &[String],
        print_unmatched_sends_only: bool,
    ) {
        console.println(&format!("Evaluating from {}", file_names.join(", ")));
        let results = client.eval(file_names, print_unmatched_sends_only);
        for (file_name, result) in file_names.iter().zip(results) {
            console.println("");
            console.println_colored(&format!("Result for {file_name}:").blue());
            match result {
                Ok(output) => console.println(&output),
                Err(error) => console.println_colored(&format!("Error: {error}").red()),
            }
        }
    }

    /// The list of REPL keywords (port of `ReplRuntime.keywords`).
    pub fn keywords() -> Vec<String> {
        vec!["stdout", "stdoutack", "stderr", "stderrack", "for", "!!"]
            .into_iter()
            .map(str::to_string)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::effects::{ConsoleIo, ReplClient};
    use rchain_shared::string_ops::ColoredString;

    struct MockConsole {
        lines: Vec<Option<String>>,
        printed: Vec<String>,
    }

    impl ConsoleIo for MockConsole {
        fn read_line(&mut self) -> Option<String> {
            if self.lines.is_empty() {
                None
            } else {
                Some(self.lines.remove(0).unwrap())
            }
        }
        fn read_password(&mut self, _p: &str) -> String {
            String::new()
        }
        fn println(&mut self, s: &str) {
            self.printed.push(s.to_string());
        }
        fn println_colored(&mut self, s: &ColoredString) {
            self.printed.push(s.colorize());
        }
        fn update_completion(&mut self, _h: &[String]) {}
        fn close(&mut self) {}
    }

    struct MockClient {
        result: Result<String, String>,
    }

    impl ReplClient for MockClient {
        fn run(&self, _line: &str) -> Result<String, String> {
            self.result.clone()
        }
        fn eval(&self, _files: &[String], _p: bool) -> Vec<Result<String, String>> {
            vec![]
        }
    }

    #[test]
    fn repl_quits_on_q() {
        let mut console = MockConsole {
            lines: vec![Some(":q".to_string())],
            printed: vec![],
        };
        let client = MockClient {
            result: Ok("result".to_string()),
        };
        ReplRuntime.repl_program(&mut console, &client);
        assert!(console.printed.is_empty());
    }

    #[test]
    fn repl_runs_program_and_quits() {
        let mut console = MockConsole {
            lines: vec![
                Some("new x in { 0 }".to_string()),
                Some(":q".to_string()),
            ],
            printed: vec![],
        };
        let client = MockClient {
            result: Ok("result".to_string()),
        };
        ReplRuntime.repl_program(&mut console, &client);
        assert_eq!(console.printed, vec!["\u{001b}[34mresult\u{001b}[0m"]);
    }

    #[test]
    fn repl_stops_on_error() {
        let mut console = MockConsole {
            lines: vec![Some("bad".to_string()), Some(":q".to_string())],
            printed: vec![],
        };
        let client = MockClient {
            result: Err("boom".to_string()),
        };
        ReplRuntime.repl_program(&mut console, &client);
        assert_eq!(console.printed, vec!["\u{001b}[31mError: boom\u{001b}[0m"]);
    }

    #[test]
    fn keywords_are_listed() {
        assert_eq!(
            ReplRuntime::keywords(),
            vec!["stdout", "stdoutack", "stderr", "stderrack", "for", "!!"]
        );
    }
}
