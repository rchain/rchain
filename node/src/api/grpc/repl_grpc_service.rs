//! The REPL gRPC service (port of `ReplGrpcService.scala`).

use std::sync::Arc;
use std::time::Duration;

use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::ast::Par;
use rchain_rholang::accounting::Cost;
use rchain_rholang::normalizer::source_to_adt;
use rchain_rholang::pretty_printer::PrettyPrinter;
use rchain_rholang::runtime::RhoRuntime;
use rchain_rholang::storage_printer::{pretty_print, pretty_print_unmatched_sends};

/// The phlo (gas) limit for a single Repl evaluation (documented deviation: Scala runs Repl with no
/// limit). The reducer aborts with `OutOfPhlogistonsError` once the balance is exhausted, so a
/// runaway term cannot drain the node.
const REPL_PHLO_LIMIT: i64 = 1_000_000_000;
/// The wall-clock deadline for a single Repl evaluation (documented deviation).
const REPL_EVAL_TIMEOUT: Duration = Duration::from_secs(60);

/// `CmdRequest` (run a single line).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CmdRequest {
    pub line: String,
}

/// `EvalRequest` (evaluate a program, optionally reporting only unmatched sends).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct EvalRequest {
    pub program: String,
    pub print_unmatched_sends_only: bool,
}

/// `ReplResponse` (the rendered output).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ReplResponse {
    pub output: String,
}

/// The REPL service (port of `ReplGrpcService`).
pub struct ReplGrpcService {
    runtime: Arc<RhoRuntime>,
}

impl ReplGrpcService {
    pub fn new(runtime: Arc<RhoRuntime>) -> Self {
        ReplGrpcService { runtime }
    }

    /// Run a single line (port of `run`).
    pub async fn run(&self, request: &CmdRequest) -> ReplResponse {
        self.exec(&request.line, false).await
    }

    /// Evaluate a program (port of `eval`).
    pub async fn eval(&self, request: &EvalRequest) -> ReplResponse {
        self.exec(&request.program, request.print_unmatched_sends_only).await
    }

    async fn exec(&self, source: &str, print_unmatched_sends_only: bool) -> ReplResponse {
        // Parse first so a syntax error surfaces as `Error: ...` before evaluation.
        match source_to_adt(source) {
            Err(e) => ReplResponse {
                output: format!("Error: {e}"),
            },
            Ok(term) => {
                // Port of `printNormalizedTerm`: echo the normalized term on the node console.
                println!("\nEvaluating:");
                println!("{}", PrettyPrinter::new().build_string(&Par::from(term.clone())));
                let rand = Blake2b512Random::default_random();
                // Bound the Repl evaluation: a phlo cap (the reducer aborts when the balance is
                // exhausted) + a wall-clock deadline.
                self.runtime.cost().set(Cost::new(REPL_PHLO_LIMIT, "repl"));
                let eval =
                    match tokio::time::timeout(REPL_EVAL_TIMEOUT, self.runtime.evaluate(source, &rand))
                        .await
                    {
                        Ok(res) => res,
                        Err(_) => {
                            return ReplResponse {
                                output: format!(
                                    "Error: evaluation timed out after {REPL_EVAL_TIMEOUT:?}"
                                ),
                            };
                        }
                    };
                let pretty_storage = if print_unmatched_sends_only {
                    pretty_print_unmatched_sends(self.runtime.as_ref()).await
                } else {
                    pretty_print(self.runtime.as_ref()).await
                };
                match eval {
                    Ok(res) => {
                        let error_str = if res.errors.is_empty() {
                            String::new()
                        } else {
                            format!(
                                "Errors received during evaluation:\n{}\n",
                                res.errors
                                    .iter()
                                    .map(|e| e.to_string())
                                    .collect::<Vec<_>>()
                                    .join("\n")
                            )
                        };
                        ReplResponse {
                            output: format!(
                                "Deployment cost: {}\n{}Storage Contents:\n{}",
                                res.cost.value, error_str, pretty_storage
                            ),
                        }
                    }
                    Err(e) => ReplResponse {
                        output: format!("Error: {e}"),
                    },
                }
            }
        }
    }
}
