//! Casper block-report protocol types (port of the `Report*` / `*EventData` messages in
//! `DeployServiceCommon.proto`).

use serde::{Deserialize, Serialize};

use crate::ast::Par;
use crate::casper::protocol::casper_message::{Peek, SystemDeployData};
use crate::casper::protocol::deploy_service::{DeployInfo, LightBlockInfo};
use crate::runtime::{BindPattern, ListParWithRandom};

/// `ReportProduceProto` — a produce event (channel + data).
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReportProduceProto {
    pub channel: Par,
    pub data: ListParWithRandom,
}

/// `ReportConsumeProto` — a consume event (channels + patterns + peeks).
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReportConsumeProto {
    pub channels: Vec<Par>,
    pub patterns: Vec<BindPattern>,
    pub peeks: Vec<Peek>,
}

/// `ReportCommProto` — a comm event (one consume + many produces).
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct ReportCommProto {
    pub consume: ReportConsumeProto,
    pub produces: Vec<ReportProduceProto>,
}

/// `ReportProto` — the `oneof report` event sum type.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub enum ReportProto {
    Produce(ReportProduceProto),
    Consume(ReportConsumeProto),
    Comm(ReportCommProto),
}

/// `SingleReport` — the events produced by one deploy/soft-checkpoint segment.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SingleReport {
    pub events: Vec<ReportProto>,
}

/// `DeployInfoWithEventData` — a user deploy plus its report.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct DeployInfoWithEventData {
    pub deploy_info: DeployInfo,
    pub report: Vec<SingleReport>,
}

/// `SystemDeployInfoWithEventData` — a system deploy plus its report.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct SystemDeployInfoWithEventData {
    pub system_deploy: SystemDeployData,
    pub report: Vec<SingleReport>,
}

/// `BlockEventInfo` — the full per-block report.
#[derive(Clone, Debug, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct BlockEventInfo {
    pub block_info: LightBlockInfo,
    pub deploys: Vec<DeployInfoWithEventData>,
    pub system_deploys: Vec<SystemDeployInfoWithEventData>,
    #[serde(with = "crate::ast::hex_serde")]
    pub post_state_hash: Vec<u8>,
}
