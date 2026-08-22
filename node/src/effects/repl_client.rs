//! REPL client interface (port of `effects/ReplClient.scala`).

use std::sync::Mutex;

use rchain_models::proto::repl::repl_client::ReplClient as TonicReplClient;
use rchain_models::proto::repl::{CmdRequest, EvalRequest};

/// A thin REPL client (port of `ReplClient[F]`; the `F[_]` effect is simplified to synchronous
/// calls and `Either[Throwable, String]` becomes `Result<String, String>`).
pub trait ReplClient {
    fn run(&self, line: &str) -> Result<String, String>;

    fn eval(
        &self,
        file_names: &[String],
        print_unmatched_sends_only: bool,
    ) -> Vec<Result<String, String>>;
}

/// A tonic-backed REPL client (port of `GrpcReplClient`).
///
/// The trait is synchronous, so each call blocks on the stored [`tokio::runtime::Handle`]; the REPL
/// loop is expected to run on a blocking thread (e.g. under `tokio::task::spawn_blocking`), where
/// `Handle::block_on` is legal.
pub struct GrpcReplClient {
    handle: tokio::runtime::Handle,
    inner: Mutex<TonicReplClient<tonic::transport::Channel>>,
}

impl GrpcReplClient {
    pub async fn connect(host: &str, port: i32, max_message_size: i32) -> Result<Self, String> {
        let handle = tokio::runtime::Handle::current();
        let endpoint = tonic::transport::Endpoint::from_shared(format!("http://{host}:{port}"))
            .map_err(|e| e.to_string())?;
        let channel = endpoint.connect().await.map_err(|e| e.to_string())?;
        let max_message_size = usize::try_from(max_message_size)
            .map_err(|_| format!("negative max message size: {max_message_size}"))?;
        let inner = TonicReplClient::new(channel).max_decoding_message_size(max_message_size);
        Ok(Self {
            handle,
            inner: Mutex::new(inner),
        })
    }

    fn eval_one(&self, file_name: &str, print_unmatched_sends_only: bool) -> Result<String, String> {
        let content = std::fs::read_to_string(file_name)
            .map_err(|_| format!("File not found: {file_name}"))?;
        self.handle.block_on(async {
            let mut client = self.inner.lock().unwrap_or_else(|p| p.into_inner());
            let response = client
                .eval(EvalRequest {
                    program: content,
                    print_unmatched_sends_only,
                })
                .await
                .map_err(|s| s.to_string())?;
            Ok(response.into_inner().output)
        })
    }
}

impl ReplClient for GrpcReplClient {
    fn run(&self, line: &str) -> Result<String, String> {
        self.handle.block_on(async {
            let mut client = self.inner.lock().unwrap_or_else(|p| p.into_inner());
            let response = client
                .run(CmdRequest {
                    line: line.to_string(),
                })
                .await
                .map_err(|s| s.to_string())?;
            Ok(response.into_inner().output)
        })
    }

    fn eval(
        &self,
        file_names: &[String],
        print_unmatched_sends_only: bool,
    ) -> Vec<Result<String, String>> {
        file_names
            .iter()
            .map(|f| self.eval_one(f, print_unmatched_sends_only))
            .collect()
    }
}
