//! Casper client programs (port of `coop.rchain.casper.protocol.client`).
//!
//! The listen-at-name `Name` ADT + [`build_par`] mirror `ListenAtName.scala`; the gRPC service
//! layer ([`DeployService`]/[`ProposeService`] + tonic clients) and the CLI command programs
//! ([`DeployRuntime`]) mirror `DeployService.scala`, `ProposeService.scala`, and
//! `DeployRuntime.scala`.

use std::collections::BTreeMap;
use std::future::Future;
use std::time::Duration;

use tonic::transport::Channel;

use rchain_crypto::private_key::PrivateKey;
use rchain_crypto::signatures::secp256k1::Secp256k1;
use rchain_crypto::signatures::signed::Signed;
use rchain_models::ast::Par;
use rchain_models::casper::protocol::casper_message::{DeployData, SignedDeployData};
use rchain_models::casper::protocol::deploy_service::{
    deploy_info_from_wire, light_block_info_from_wire, BlockInfo, BlockQuery, BlocksQuery,
    BondStatusQuery, ContinuationAtNameQuery, ContinuationsWithBlockInfo, DataAtNameQuery,
    DataWithBlockInfo, DeployExecStatus, FindDeployQuery, IsFinalizedQuery, MachineVerifyQuery,
    Status, VersionInfo, VisualizeDagQuery, WaitingContinuationInfo,
};
use rchain_models::proto::casper as wire;
use rchain_models::proto::casper::deploy_service_client::DeployServiceClient;
use rchain_models::proto::casper::propose_service_client::ProposeServiceClient;
use rchain_models::rholang::RhoType::RhoName;
use rchain_models::wire::{bind_pattern_from_proto, par_from_proto, par_to_proto};
use rchain_rholang::errors::RholangError;

// -------------------------------------------------------------------------------------------------
// Listen-at-name (unchanged from the original `client.rs`)
// -------------------------------------------------------------------------------------------------

/// A name to listen at (port of `ListenAtName.Name`).
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Name {
    PrivName(String),
    PubName(String),
}

/// Build a `Par` from a name (port of `ListenAtName.buildParId`).
pub fn build_par(name: &Name) -> Result<Par, RholangError> {
    match name {
        Name::PubName(content) => {
            rchain_rholang::normalizer::source_to_adt_with_env(content, &BTreeMap::new())
                .map(Par::from)
        }
        Name::PrivName(content) => Ok(RhoName::apply_bytes(content.as_bytes().to_vec())),
    }
}

// -------------------------------------------------------------------------------------------------
// Wire -> domain conversion (shared with the node's gRPC server layer: see
// `rchain_models::casper::protocol::deploy_service::{light_block_info_from_wire,
// deploy_info_from_wire}`)
// -------------------------------------------------------------------------------------------------

fn block_info_from_wire(b: &wire::BlockInfo) -> Result<BlockInfo, String> {
    let bi = b.block_info.as_ref().ok_or("missing block_info")?;
    Ok(BlockInfo {
        block_info: light_block_info_from_wire(bi),
        deploys: b.deploys.iter().map(deploy_info_from_wire).collect(),
    })
}

fn version_info_from_wire(v: &wire::VersionInfo) -> VersionInfo {
    VersionInfo {
        api: v.api.clone(),
        node: v.node.clone(),
    }
}

fn status_from_wire(s: &wire::Status) -> Result<Status, String> {
    let v = s.version.as_ref().ok_or("missing version")?;
    Ok(Status {
        version: version_info_from_wire(v),
        address: s.address.clone(),
        network_id: s.network_id.clone(),
        shard_id: s.shard_id.clone(),
        peers: s.peers,
        nodes: s.nodes,
        min_phlo_price: s.min_phlo_price,
        latest_block_number: s.latest_block_number,
    })
}

fn deploy_exec_status_from_wire(s: &wire::DeployExecStatus) -> Result<DeployExecStatus, String> {
    use wire::deploy_exec_status::Status as WireStatus;
    match s.status.as_ref().ok_or("missing status")? {
        WireStatus::ProcessedWithSuccess(p) => Ok(DeployExecStatus::ProcessedWithSuccess {
            deploy_result: p
                .deploy_result
                .iter()
                .map(par_from_proto)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| e.to_string())?,
            block: light_block_info_from_wire(p.block.as_ref().ok_or("missing block")?),
        }),
        WireStatus::ProcessedWithError(p) => Ok(DeployExecStatus::ProcessedWithError {
            deploy_error: p.deploy_error.clone(),
            block: light_block_info_from_wire(p.block.as_ref().ok_or("missing block")?),
        }),
        WireStatus::NotProcessed(p) => Ok(DeployExecStatus::NotProcessed {
            status: p.status.clone(),
        }),
    }
}

fn data_with_block_info_from_wire(d: &wire::DataWithBlockInfo) -> Result<DataWithBlockInfo, String> {
    let block = d.block.as_ref().ok_or("missing block")?;
    Ok(DataWithBlockInfo {
        post_block_data: d
            .post_block_data
            .iter()
            .map(par_from_proto)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| e.to_string())?,
        block: light_block_info_from_wire(block),
    })
}

fn waiting_continuation_info_from_wire(
    w: &wire::WaitingContinuationInfo,
) -> Result<WaitingContinuationInfo, String> {
    let cont = w.post_block_continuation.as_ref().ok_or("missing continuation")?;
    Ok(WaitingContinuationInfo {
        post_block_patterns: w
            .post_block_patterns
            .iter()
            .map(bind_pattern_from_proto)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| e.to_string())?,
        post_block_continuation: par_from_proto(cont).map_err(|e| e.to_string())?,
    })
}

fn continuations_with_block_info_from_wire(
    c: &wire::ContinuationsWithBlockInfo,
) -> Result<ContinuationsWithBlockInfo, String> {
    let block = c.block.as_ref().ok_or("missing block")?;
    Ok(ContinuationsWithBlockInfo {
        post_block_continuations: c
            .post_block_continuations
            .iter()
            .map(waiting_continuation_info_from_wire)
            .collect::<Result<Vec<_>, _>>()?,
        block: light_block_info_from_wire(block),
    })
}

fn service_error(err: &wire::ServiceError) -> Vec<String> {
    err.messages.clone()
}

fn to_json_pretty<T: serde::Serialize>(value: &T) -> String {
    serde_json::to_string_pretty(value).unwrap_or_default()
}

async fn connect_channel(host: &str, port: i32) -> Result<Channel, String> {
    let endpoint = tonic::transport::Endpoint::from_shared(format!("http://{host}:{port}"))
        .map_err(|e| e.to_string())?;
    endpoint.connect().await.map_err(|e| e.to_string())
}

// -------------------------------------------------------------------------------------------------
// Deploy service
// -------------------------------------------------------------------------------------------------

/// Casper deploy-service client (port of `DeployService[F]`).
#[async_trait::async_trait]
pub trait DeployService: Send + Sync {
    async fn deploy(&self, d: &SignedDeployData) -> Result<String, Vec<String>>;
    async fn deploy_status(&self, deploy_id: &FindDeployQuery) -> Result<DeployExecStatus, Vec<String>>;
    async fn get_block(&self, q: &BlockQuery) -> Result<String, Vec<String>>;
    async fn get_blocks(&self, q: &BlocksQuery) -> Result<String, Vec<String>>;
    async fn visualize_dag(&self, q: &VisualizeDagQuery) -> Result<String, Vec<String>>;
    async fn machine_verifiable_dag(&self, q: &MachineVerifyQuery) -> Result<String, Vec<String>>;
    async fn find_deploy(&self, request: &FindDeployQuery) -> Result<String, Vec<String>>;
    async fn listen_for_data_at_name(
        &self,
        request: &DataAtNameQuery,
    ) -> Result<Vec<DataWithBlockInfo>, Vec<String>>;
    async fn listen_for_continuation_at_name(
        &self,
        request: &ContinuationAtNameQuery,
    ) -> Result<Vec<ContinuationsWithBlockInfo>, Vec<String>>;
    async fn last_finalized_block(&self) -> Result<String, Vec<String>>;
    async fn is_finalized(&self, q: &IsFinalizedQuery) -> Result<String, Vec<String>>;
    async fn bond_status(&self, q: &BondStatusQuery) -> Result<String, Vec<String>>;
    async fn status(&self) -> Result<String, Vec<String>>;
}

/// tonic-backed deploy service client (port of `GrpcDeployService`).
pub struct GrpcDeployService {
    client: DeployServiceClient<Channel>,
}

impl GrpcDeployService {
    pub async fn connect(host: &str, port: i32, max_message_size: i32) -> Result<Self, String> {
        let channel = connect_channel(host, port).await?;
        Ok(Self {
            client: DeployServiceClient::new(channel).max_decoding_message_size(
                usize::try_from(max_message_size)
                    .map_err(|_| format!("negative max message size: {max_message_size}"))?,
            ),
        })
    }
}

#[async_trait::async_trait]
impl DeployService for GrpcDeployService {
    async fn deploy(&self, d: &SignedDeployData) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .do_deploy(d.to_proto())
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::deploy_response::Message::Result(s)) => Ok(s),
            Some(wire::deploy_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn deploy_status(&self, deploy_id: &FindDeployQuery) -> Result<DeployExecStatus, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .deploy_status(wire::FindDeployQuery {
                deploy_id: deploy_id.deploy_id.clone(),
            })
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::deploy_status_response::Message::DeployExecStatus(s)) => {
                deploy_exec_status_from_wire(&s).map_err(|e| vec![e])
            }
            Some(wire::deploy_status_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn get_block(&self, q: &BlockQuery) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .get_block(wire::BlockQuery { hash: q.hash.clone() })
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::block_response::Message::BlockInfo(bi)) => {
                let domain = block_info_from_wire(&bi).map_err(|e| vec![e])?;
                Ok(to_json_pretty(&domain))
            }
            Some(wire::block_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn get_blocks(&self, q: &BlocksQuery) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .get_blocks(wire::BlocksQuery { depth: q.depth })
            .await
            .map_err(|s| vec![s.to_string()])?;
        let mut stream = response.into_inner();
        let mut errors = Vec::new();
        let mut blocks = Vec::new();
        while let Some(item) = stream.message().await.map_err(|s| vec![s.to_string()])? {
            match item.message {
                Some(wire::block_info_response::Message::BlockInfo(bi)) => {
                    blocks.push(light_block_info_from_wire(&bi))
                }
                Some(wire::block_info_response::Message::Error(e)) => errors.extend(e.messages),
                None => {}
            }
        }
        if errors.is_empty() {
            let rendered = blocks
                .iter()
                .map(|bi| {
                    format!(
                        "\n------------- block {} ---------------\n{}\n-----------------------------------------------------\n",
                        bi.block_number,
                        to_json_pretty(bi)
                    )
                })
                .collect::<Vec<_>>();
            let show_length = format!("\ncount: {}\n", rendered.len());
            Ok(format!("{}\n{}", rendered.join("\n"), show_length))
        } else {
            Err(errors)
        }
    }

    async fn visualize_dag(&self, q: &VisualizeDagQuery) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .visualize_dag(wire::VisualizeDagQuery {
                depth: q.depth,
                show_justification_lines: q.show_justification_lines,
                start_block_number: q.start_block_number,
            })
            .await
            .map_err(|s| vec![s.to_string()])?;
        let mut stream = response.into_inner();
        let mut errors = Vec::new();
        let mut contents = Vec::new();
        while let Some(item) = stream.message().await.map_err(|s| vec![s.to_string()])? {
            match item.message {
                Some(wire::visualize_blocks_response::Message::Content(c)) => contents.push(c),
                Some(wire::visualize_blocks_response::Message::Error(e)) => errors.extend(e.messages),
                None => {}
            }
        }
        if errors.is_empty() {
            Ok(contents.concat())
        } else {
            Err(errors)
        }
    }

    async fn machine_verifiable_dag(&self, q: &MachineVerifyQuery) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .machine_verifiable_dag(wire::MachineVerifyQuery { depth: q.depth })
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::machine_verify_response::Message::Content(c)) => Ok(c),
            Some(wire::machine_verify_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn find_deploy(&self, request: &FindDeployQuery) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .find_deploy(wire::FindDeployQuery {
                deploy_id: request.deploy_id.clone(),
            })
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::find_deploy_response::Message::BlockInfo(bi)) => {
                Ok(to_json_pretty(&light_block_info_from_wire(&bi)))
            }
            Some(wire::find_deploy_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn listen_for_data_at_name(
        &self,
        request: &DataAtNameQuery,
    ) -> Result<Vec<DataWithBlockInfo>, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .listen_for_data_at_name(wire::DataAtNameQuery {
                depth: request.depth,
                name: Some(par_to_proto(&request.name)),
            })
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::listening_name_data_response::Message::Payload(p)) => p
                .block_info
                .iter()
                .map(data_with_block_info_from_wire)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| vec![e]),
            Some(wire::listening_name_data_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn listen_for_continuation_at_name(
        &self,
        request: &ContinuationAtNameQuery,
    ) -> Result<Vec<ContinuationsWithBlockInfo>, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .listen_for_continuation_at_name(wire::ContinuationAtNameQuery {
                depth: request.depth,
                names: request.names.iter().map(par_to_proto).collect(),
            })
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::continuation_at_name_response::Message::Payload(p)) => p
                .block_results
                .iter()
                .map(continuations_with_block_info_from_wire)
                .collect::<Result<Vec<_>, _>>()
                .map_err(|e| vec![e]),
            Some(wire::continuation_at_name_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn last_finalized_block(&self) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .last_finalized_block(wire::LastFinalizedBlockQuery {})
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::last_finalized_block_response::Message::BlockInfo(bi)) => {
                let domain = block_info_from_wire(&bi).map_err(|e| vec![e])?;
                Ok(to_json_pretty(&domain))
            }
            Some(wire::last_finalized_block_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn is_finalized(&self, q: &IsFinalizedQuery) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .is_finalized(wire::IsFinalizedQuery { hash: q.hash.clone() })
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::is_finalized_response::Message::IsFinalized(true)) => {
                Ok("Block is finalized".to_string())
            }
            Some(wire::is_finalized_response::Message::IsFinalized(false)) => {
                Err(vec!["Block is not finalized".to_string()])
            }
            Some(wire::is_finalized_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn bond_status(&self, q: &BondStatusQuery) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .bond_status(wire::BondStatusQuery {
                public_key: q.public_key.clone(),
            })
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::bond_status_response::Message::IsBonded(true)) => {
                Ok("Validator is bonded".to_string())
            }
            Some(wire::bond_status_response::Message::IsBonded(false)) => {
                Err(vec!["Validator is not bonded".to_string()])
            }
            Some(wire::bond_status_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }

    async fn status(&self) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .status(())
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::status_response::Message::Status(s)) => {
                let domain = status_from_wire(&s).map_err(|e| vec![e])?;
                Ok(to_json_pretty(&domain))
            }
            Some(wire::status_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Propose service
// -------------------------------------------------------------------------------------------------

/// Casper propose-service client (port of `ProposeService[F]`).
#[async_trait::async_trait]
pub trait ProposeService: Send + Sync {
    async fn propose(&self, is_async: bool) -> Result<String, Vec<String>>;
}

/// tonic-backed propose service client (port of `GrpcProposeService`).
pub struct GrpcProposeService {
    client: ProposeServiceClient<Channel>,
}

impl GrpcProposeService {
    pub async fn connect(host: &str, port: i32, max_message_size: i32) -> Result<Self, String> {
        let channel = connect_channel(host, port).await?;
        Ok(Self {
            client: ProposeServiceClient::new(channel).max_decoding_message_size(
                usize::try_from(max_message_size)
                    .map_err(|_| format!("negative max message size: {max_message_size}"))?,
            ),
        })
    }
}

#[async_trait::async_trait]
impl ProposeService for GrpcProposeService {
    async fn propose(&self, is_async: bool) -> Result<String, Vec<String>> {
        let mut client = self.client.clone();
        let response = client
            .propose(wire::ProposeQuery { is_async })
            .await
            .map_err(|s| vec![s.to_string()])?;
        match response.into_inner().message {
            Some(wire::propose_response::Message::Result(s)) => Ok(s),
            Some(wire::propose_response::Message::Error(e)) => Err(service_error(&e)),
            None => Err(vec!["empty response".to_string()]),
        }
    }
}

// -------------------------------------------------------------------------------------------------
// Deploy runtime (CLI command programs)
// -------------------------------------------------------------------------------------------------

/// CLI command programs (port of `DeployRuntime`).
pub struct DeployRuntime;

impl DeployRuntime {
    pub async fn propose(
        service: &dyn ProposeService,
        print_unmatched_sends: bool,
    ) -> Result<(), Vec<String>> {
        graceful_exit(async {
            service
                .propose(print_unmatched_sends)
                .await
                .map(|r| format!("Response: {r}"))
        })
        .await
    }

    pub async fn get_block(service: &dyn DeployService, hash: &str) -> Result<(), Vec<String>> {
        graceful_exit(async {
            service
                .get_block(&BlockQuery {
                    hash: hash.to_string(),
                })
                .await
        })
        .await
    }

    pub async fn get_blocks(service: &dyn DeployService, depth: i32) -> Result<(), Vec<String>> {
        graceful_exit(async { service.get_blocks(&BlocksQuery { depth }).await }).await
    }

    pub async fn visualize_dag(
        service: &dyn DeployService,
        depth: i32,
        show_justification_lines: bool,
    ) -> Result<(), Vec<String>> {
        graceful_exit(async {
            service
                .visualize_dag(&VisualizeDagQuery {
                    depth,
                    show_justification_lines,
                    start_block_number: 0,
                })
                .await
        })
        .await
    }

    pub async fn machine_verifiable_dag(service: &dyn DeployService) -> Result<(), Vec<String>> {
        graceful_exit(async {
            service
                .machine_verifiable_dag(&MachineVerifyQuery { depth: 0 })
                .await
        })
        .await
    }

    pub async fn find_deploy(
        service: &dyn DeployService,
        deploy_id: &[u8],
    ) -> Result<(), Vec<String>> {
        graceful_exit(async {
            service
                .find_deploy(&FindDeployQuery {
                    deploy_id: deploy_id.to_vec(),
                })
                .await
        })
        .await
    }

    /// Deploy a Rholang source file to Casper (port of `deployFileProgram`).
    #[allow(clippy::too_many_arguments)]
    pub async fn deploy_file_program(
        service: &dyn DeployService,
        phlo_limit: i64,
        phlo_price: i64,
        valid_after_block_number: i64,
        private_key: &PrivateKey,
        file: &str,
        shard_id: &str,
    ) -> Result<(), Vec<String>> {
        graceful_exit(async {
            let code = std::fs::read_to_string(file)
                .map_err(|e| vec![format!("Error with given file: \n{e}")])?;
            let timestamp = std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .map(|d| d.as_millis() as i64)
                .unwrap_or(0);
            let data = DeployData {
                term: code,
                timestamp,
                phlo_price,
                phlo_limit,
                valid_after_block_number,
                shard_id: shard_id.to_string(),
            };
            let signed = Signed::new(data, &Secp256k1, private_key).map_err(|e| vec![e.to_string()])?;
            let signed_deploy = SignedDeployData {
                data: signed.data,
                deployer: signed.pk.bytes().to_vec(),
                sig: signed.sig,
                sig_algorithm: signed.sig_algorithm.name().to_string(),
            };
            service
                .deploy(&signed_deploy)
                .await
                .map(|r| format!("Response: {r}"))
        })
        .await
    }

    pub async fn deploy_status(
        service: &dyn DeployService,
        deploy_id: &[u8],
    ) -> Result<(), Vec<String>> {
        graceful_exit(async {
            service
                .deploy_status(&FindDeployQuery {
                    deploy_id: deploy_id.to_vec(),
                })
                .await
                .map(|s| format!("Deploy status: {}", to_json_pretty(&s)))
        })
        .await
    }

    pub async fn listen_for_data_at_name(
        service: &dyn DeployService,
        name: &Name,
    ) -> Result<(), Vec<String>> {
        let par = build_par(name).map_err(|e| vec![e.to_string()])?;
        println!("Listen at name: {name:?}");
        println!("Start monitoring for changes");
        let result = poll_until_grows(|| async {
            service
                .listen_for_data_at_name(&DataAtNameQuery {
                    depth: i32::MAX,
                    name: par.clone(),
                })
                .await
        })
        .await?;
        println!("Detected changes:");
        println!("{result:?}");
        Ok(())
    }

    pub async fn listen_for_continuation_at_name(
        service: &dyn DeployService,
        names: &[Name],
    ) -> Result<(), Vec<String>> {
        let pars = names
            .iter()
            .map(build_par)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| vec![e.to_string()])?;
        println!("Listen at name: {names:?}");
        println!("Start monitoring for changes");
        let result = poll_until_grows(|| async {
            service
                .listen_for_continuation_at_name(&ContinuationAtNameQuery {
                    depth: i32::MAX,
                    names: pars.clone(),
                })
                .await
        })
        .await?;
        println!("Detected changes:");
        println!("{result:?}");
        Ok(())
    }

    pub async fn last_finalized_block(service: &dyn DeployService) -> Result<(), Vec<String>> {
        graceful_exit(async { service.last_finalized_block().await }).await
    }

    pub async fn is_finalized(service: &dyn DeployService, hash: &str) -> Result<(), Vec<String>> {
        graceful_exit(async {
            service
                .is_finalized(&IsFinalizedQuery {
                    hash: hash.to_string(),
                })
                .await
        })
        .await
    }

    pub async fn bond_status(
        service: &dyn DeployService,
        public_key: &[u8],
    ) -> Result<(), Vec<String>> {
        graceful_exit(async {
            service
                .bond_status(&BondStatusQuery {
                    public_key: public_key.to_vec(),
                })
                .await
        })
        .await
    }

    pub async fn status(service: &dyn DeployService) -> Result<(), Vec<String>> {
        graceful_exit(async { service.status().await }).await
    }
}

/// Print a success message, or propagate the error list (port of `DeployRuntime.gracefulExit`).
async fn graceful_exit(
    program: impl Future<Output = Result<String, Vec<String>>>,
) -> Result<(), Vec<String>> {
    match program.await {
        Ok(msg) => {
            println!("{msg}");
            Ok(())
        }
        Err(errors) => Err(errors),
    }
}

/// Poll `retrieve` until the result grows, returning the larger result (port of
/// `ListenAtName.applyUntil`).
async fn poll_until_grows<T, F, Fut>(mut retrieve: F) -> Result<Vec<T>, Vec<String>>
where
    F: FnMut() -> Fut,
    Fut: Future<Output = Result<Vec<T>, Vec<String>>>,
{
    let mut current = retrieve().await?;
    loop {
        tokio::time::sleep(Duration::from_secs(1)).await;
        let next = retrieve().await?;
        if next.len() > current.len() {
            return Ok(next);
        }
        current = next;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn priv_name_builds_unforgeable() {
        let par = build_par(&Name::PrivName("abc".to_string())).unwrap();
        assert!(!par.unforgeables.is_empty());
        assert!(par.exprs.is_empty());
    }

    #[test]
    fn pub_name_normalizes_source() {
        let par = build_par(&Name::PubName("Nil".to_string())).unwrap();
        assert!(par.unforgeables.is_empty());
    }
}
