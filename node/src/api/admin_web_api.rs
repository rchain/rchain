//! Admin web API interface (port of `api/AdminWebApi.scala`).

use async_trait::async_trait;

use super::dto::BlockApiException;

/// The admin web API contract (port of `AdminWebApi[F]`).
#[async_trait]
pub trait AdminWebApi: Send + Sync {
    async fn propose(&self) -> Result<String, BlockApiException>;

    async fn propose_result(&self) -> Result<String, BlockApiException>;
}
