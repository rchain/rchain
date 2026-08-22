//! Reporting routes (port of `web/ReportingRoutes.scala`).

use serde::Serialize;

use rchain_models::casper::protocol::report::BlockEventInfo;

/// The reporting HTTP response (port of `ReportingRoutes.ReportResponse`, with the circe
/// `"type"` discriminator + kebab-case constructor/member names).
#[derive(Clone, Debug, Serialize)]
#[serde(tag = "type", rename_all = "kebab-case")]
pub enum ReportResponse {
    BlockTracesReport {
        report: BlockEventInfo,
    },
    BlockReportError {
        #[serde(rename = "error-message")]
        error_message: String,
    },
}

/// Map a `blockReport` result to a `ReportResponse` (port of `ReportingRoutes.transforResult`).
pub fn transform_result(result: Result<BlockEventInfo, String>) -> ReportResponse {
    match result {
        Ok(report) => ReportResponse::BlockTracesReport { report },
        Err(error_message) => ReportResponse::BlockReportError { error_message },
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rchain_models::casper::protocol::deploy_service::LightBlockInfo;

    fn empty_block_event_info() -> BlockEventInfo {
        BlockEventInfo {
            block_info: LightBlockInfo {
                version: 0,
                shard_id: String::new(),
                block_hash: String::new(),
                block_number: 0,
                sender: String::new(),
                seq_num: 0,
                pre_state_hash: String::new(),
                post_state_hash: String::new(),
                justifications: vec![],
                bonds: vec![],
                sig_algorithm: String::new(),
                sig: String::new(),
                block_size: String::new(),
                deploy_count: 0,
                rejected_deploys: vec![],
            },
            deploys: vec![],
            system_deploys: vec![],
            post_state_hash: vec![],
        }
    }

    #[test]
    fn transform_error_into_block_report_error() {
        match transform_result(Err("boom".to_string())) {
            ReportResponse::BlockReportError { error_message } => assert_eq!(error_message, "boom"),
            _ => panic!("expected BlockReportError"),
        }
    }

    #[test]
    fn transform_ok_into_block_traces_report() {
        let info = empty_block_event_info();
        match transform_result(Ok(info)) {
            ReportResponse::BlockTracesReport { report } => assert!(report.deploys.is_empty()),
            _ => panic!("expected BlockTracesReport"),
        }
    }

    #[test]
    fn error_serializes_with_type_and_kebab_case() {
        let value = serde_json::to_value(ReportResponse::BlockReportError {
            error_message: "boom".to_string(),
        })
        .unwrap();
        assert_eq!(value["type"], "block-report-error");
        assert_eq!(value["error-message"], "boom");
        assert!(value.get("error_message").is_none());
    }

    #[test]
    fn report_serializes_with_type_discriminator() {
        let value = serde_json::to_value(ReportResponse::BlockTracesReport {
            report: empty_block_event_info(),
        })
        .unwrap();
        assert_eq!(value["type"], "block-traces-report");
        assert!(value.get("report").is_some());
    }
}
