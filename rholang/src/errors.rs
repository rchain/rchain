//! Interpreter error types. Mirrors `rholang/src/main/scala/coop/rchain/rholang/interpreter/errors.scala`.

use std::fmt;

use rchain_models::ast::{Par, Var};

/// A source position (port of `compiler.SourcePosition`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SourcePosition {
    pub row: i32,
    pub column: i32,
}

impl fmt::Display for SourcePosition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}:{}", self.row, self.column)
    }
}

/// The rholang interpreter error ADT (mirrors the Scala `InterpreterError` hierarchy).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum RholangError {
    BugFoundError(String),
    NormalizerError(String),
    SyntaxError(String),
    LexerError(String),
    ParserError(String),
    UnboundVariableRef {
        var_name: String,
        line: i32,
        col: i32,
    },
    UnexpectedNameContext {
        var_name: String,
        proc_var_source_position: SourcePosition,
        name_source_position: SourcePosition,
    },
    UnexpectedReuseOfNameContextFree {
        var_name: String,
        first_use: SourcePosition,
        second_use: SourcePosition,
    },
    UnexpectedProcContext {
        var_name: String,
        name_var_source_position: SourcePosition,
        process_source_position: SourcePosition,
    },
    UnexpectedReuseOfProcContextFree {
        var_name: String,
        first_use: SourcePosition,
        second_use: SourcePosition,
    },
    UnexpectedBundleContent(String),
    UnrecognizedNormalizerError(String),
    OutOfPhlogistonsError,
    TopLevelWildcardsNotAllowedError(Par),
    TopLevelFreeVariablesNotAllowedError(Par),
    TopLevelLogicalConnectivesNotAllowedError(Par),
    SubstituteError { term: Var },
    PatternReceiveError(String),
    SetupError(String),
    UnrecognizedInterpreterError(String),
    SortMatchError(String),
    ReduceError(String),
    MethodNotDefined {
        method: String,
        other_type: String,
    },
    MethodArgumentNumberMismatch {
        method: String,
        expected: i32,
        actual: i32,
    },
    OperatorNotDefined {
        op: String,
        other_type: String,
    },
    OperatorExpectedError {
        op: String,
        expected: String,
        other_type: String,
    },
    AggregateError {
        interpreter_errors: Vec<RholangError>,
        errors: Vec<String>,
    },
    ReceiveOnSameChannelsError { line: i32, col: i32 },
}

impl fmt::Display for RholangError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            RholangError::BugFoundError(m) => write!(f, "{m}"),
            RholangError::NormalizerError(m) => write!(f, "{m}"),
            RholangError::SyntaxError(m) => write!(f, "{m}"),
            RholangError::LexerError(m) => write!(f, "{m}"),
            RholangError::ParserError(m) => write!(f, "{m}"),
            RholangError::UnboundVariableRef { var_name, line, col } => {
                write!(f, "Variable reference: ={var_name} at {line}:{col} is unbound.")
            }
            RholangError::UnexpectedNameContext {
                var_name,
                proc_var_source_position,
                name_source_position,
            } => write!(
                f,
                "Proc variable: {var_name} at {proc_var_source_position} used in Name context at {name_source_position}"
            ),
            RholangError::UnexpectedReuseOfNameContextFree {
                var_name,
                first_use,
                second_use,
            } => write!(
                f,
                "Free variable {var_name} is used twice as a binder (at {first_use} and {second_use}) in name context."
            ),
            RholangError::UnexpectedProcContext {
                var_name,
                name_var_source_position,
                process_source_position,
            } => write!(
                f,
                "Name variable: {var_name} at {name_var_source_position} used in process context at {process_source_position}"
            ),
            RholangError::UnexpectedReuseOfProcContextFree {
                var_name,
                first_use,
                second_use,
            } => write!(
                f,
                "Free variable {var_name} is used twice as a binder (at {first_use} and {second_use}) in process context."
            ),
            RholangError::UnexpectedBundleContent(m) => write!(f, "{m}"),
            RholangError::UnrecognizedNormalizerError(m) => write!(f, "{m}"),
            RholangError::OutOfPhlogistonsError => write!(f, "Computation ran out of phlogistons."),
            RholangError::TopLevelWildcardsNotAllowedError(w) => {
                write!(f, "Top level wildcards are not allowed: {w:?}.")
            }
            RholangError::TopLevelFreeVariablesNotAllowedError(v) => {
                write!(f, "Top level free variables are not allowed: {v:?}.")
            }
            RholangError::TopLevelLogicalConnectivesNotAllowedError(c) => {
                write!(f, "Top level logical connectives are not allowed: {c:?}.")
            }
            RholangError::SubstituteError { term } => write!(f, "Illegal Substitution [{term:?}]"),
            RholangError::PatternReceiveError(c) => {
                write!(f, "Invalid pattern in the receive: {c}. Only logical AND is allowed.")
            }
            RholangError::SetupError(m) => write!(f, "{m}"),
            RholangError::UnrecognizedInterpreterError(_) => {
                write!(f, "Unrecognized interpreter error")
            }
            RholangError::SortMatchError(m) => write!(f, "{m}"),
            RholangError::ReduceError(m) => write!(f, "{m}"),
            RholangError::MethodNotDefined { method, other_type } => {
                write!(f, "Error: Method `{method}` is not defined on {other_type}.")
            }
            RholangError::MethodArgumentNumberMismatch {
                method,
                expected,
                actual,
            } => write!(
                f,
                "Error: Method `{method}` expects {expected} Par argument(s), but got {actual} argument(s)."
            ),
            RholangError::OperatorNotDefined { op, other_type } => {
                write!(f, "Error: Operator `{op}` is not defined on {other_type}.")
            }
            RholangError::OperatorExpectedError {
                op,
                expected: _,
                other_type,
            } => write!(f, "Error: Operator `{op}` is not defined on {other_type}."),
            RholangError::AggregateError {
                interpreter_errors,
                errors,
            } => {
                write!(f, "Error: Aggregate Error")?;
                for e in interpreter_errors {
                    write!(f, "\n{e}")?;
                }
                for e in errors {
                    write!(f, "\n{e}")?;
                }
                Ok(())
            }
            RholangError::ReceiveOnSameChannelsError { line, col } => write!(
                f,
                "Receiving on the same channels is currently not allowed (at {line}:{col}). Ref. RCHAIN-4032."
            ),
        }
    }
}

impl std::error::Error for RholangError {}

/// Convenience result alias.
pub type Result<A> = std::result::Result<A, RholangError>;
