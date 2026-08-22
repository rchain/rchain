//! Propose-service protocol types (port of `ProposeServiceCommon.proto`).
//!
//! The `ProposeResponse`/`ProposeResultResponse` oneofs collapse to `Result<String, ServiceError>`
//! at the service layer; only the query messages are modeled here.

/// `ProposeQuery` (trigger a block proposal).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct ProposeQuery {
    pub is_async: bool,
}

/// `ProposeResultQuery` (wait for/read the latest proposal result).
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub struct ProposeResultQuery;
