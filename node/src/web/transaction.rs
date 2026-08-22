//! Transaction reporting data model (port of the DTOs + interface in `web/Transaction.scala`).

use std::sync::Arc;

use async_trait::async_trait;
use rchain_casper::api::block_report_api::BlockReportApi;
use rchain_crypto::hash::blake2b256_hash::Blake2b256Hash;
use rchain_models::ast::{Expr, Par};
use rchain_models::block_hash::BlockHash;
use rchain_models::casper::protocol::casper_message::SystemDeployData;
use rchain_models::casper::protocol::report::{ReportProto, SingleReport};
use rchain_shared::base16;
use serde::{Deserialize, Serialize};

/// A REV transaction (port of `Transaction`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct Transaction {
    pub from_addr: String,
    pub to_addr: String,
    pub amount: i64,
    pub ret_unforgeable: Par,
    pub fail_reason: Option<String>,
}

/// The kind of a transaction (port of `TransactionType`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all_fields = "camelCase")]
pub enum TransactionType {
    PreCharge {
        deploy_id: String,
    },
    UserDeploy {
        deploy_id: String,
    },
    Refund {
        deploy_id: String,
    },
    CloseBlock {
        block_hash: String,
    },
    SlashingDeploy {
        block_hash: String,
    },
}

/// A transaction plus its type (port of `TransactionInfo`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TransactionInfo {
    pub transaction: Transaction,
    pub transaction_type: TransactionType,
}

/// A list of transactions (port of `TransactionResponse`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct TransactionResponse {
    pub data: Vec<TransactionInfo>,
}

/// Transaction reporting interface (port of `TransactionAPI[F]`).
#[async_trait]
pub trait TransactionApi: Send + Sync {
    async fn get_transaction(
        &self,
        block_hash: &Blake2b256Hash,
    ) -> Result<Vec<TransactionInfo>, String>;
}

/// The cache-backed transaction API (port of `TransactionAPIImpl`). Extracts REV transfers from a
/// block's replayed report: user deploys contribute PreCharge/UserDeploy/Refund transactions and
/// system deploys contribute CloseBlock/SlashingDeploy transactions, matched against the
/// `transferUnforgeable` channel.
pub struct TransactionAPIImpl {
    block_report_api: Arc<BlockReportApi>,
    transfer_unforgeable: Par,
}

impl TransactionAPIImpl {
    pub fn new(block_report_api: Arc<BlockReportApi>, transfer_unforgeable: Par) -> Self {
        TransactionAPIImpl {
            block_report_api,
            transfer_unforgeable,
        }
    }

    /// Extract the REV transfers from a single report segment (port of `findTransactions`). A
    /// transfer is a COMM on the `transferUnforgeable` channel whose first produce carries
    /// `fromAddr`, `toAddr`, `amount` and `retUnforgeable`.
    fn find_transactions(report: &SingleReport, transfer_unforgeable: &Par) -> Vec<Transaction> {
        report
            .events
            .iter()
            .filter_map(|event| {
                let ReportProto::Comm(comm) = event else {
                    return None;
                };
                if comm.consume.channels.first() != Some(transfer_unforgeable) {
                    return None;
                }
                let produce = comm.produces.first()?;
                let from_addr = produce.data.pars.first()?.as_par().exprs.first().and_then(expr_string)?;
                let to_addr = produce.data.pars.get(2)?.as_par().exprs.first().and_then(expr_string)?;
                let amount = produce.data.pars.get(3)?.as_par().exprs.first().and_then(expr_int)?;
                let ret_unforgeable = produce.data.pars.get(5)?.as_par().clone();
                Some(Transaction {
                    from_addr,
                    to_addr,
                    amount,
                    ret_unforgeable,
                    fail_reason: None,
                })
            })
            .collect()
    }
}

fn expr_string(e: &Expr) -> Option<String> {
    match e {
        Expr::GString(s) => Some(s.clone()),
        _ => None,
    }
}

fn expr_int(e: &Expr) -> Option<i64> {
    match e {
        Expr::GInt(n) => Some(*n),
        _ => None,
    }
}

fn pre_charge(id: &str) -> TransactionType {
    TransactionType::PreCharge { deploy_id: id.to_string() }
}
fn user_deploy(id: &str) -> TransactionType {
    TransactionType::UserDeploy { deploy_id: id.to_string() }
}
fn refund(id: &str) -> TransactionType {
    TransactionType::Refund { deploy_id: id.to_string() }
}

#[async_trait]
impl TransactionApi for TransactionAPIImpl {
    async fn get_transaction(
        &self,
        block_hash: &Blake2b256Hash,
    ) -> Result<Vec<TransactionInfo>, String> {
        let hash = BlockHash::new(block_hash.to_byte_array());
        let report = self.block_report_api.block_report(&hash, false).await?;
        let block_hash_hex = base16::encode(block_hash.as_bytes());

        let mut out = Vec::new();
        // User deploys: report length 1/2/3 map to PreCharge / (PreCharge, Refund) /
        // (PreCharge, UserDeploy, Refund).
        for d in &report.deploys {
            let deploy_id = &d.deploy_info.sig;
            let ctors: &[fn(&str) -> TransactionType] = match d.report.len() {
                1 => &[pre_charge],
                2 => &[pre_charge, refund],
                3 => &[pre_charge, user_deploy, refund],
                n => return Err(format!("unexpected user report length {n}")),
            };
            for (single_report, ctor) in d.report.iter().zip(ctors.iter()) {
                for t in Self::find_transactions(single_report, &self.transfer_unforgeable) {
                    out.push(TransactionInfo {
                        transaction: t,
                        transaction_type: ctor(deploy_id),
                    });
                }
            }
        }
        // System deploys: Slash / CloseBlock (no precharge/refund).
        for s in &report.system_deploys {
            let tx_type = match &s.system_deploy {
                SystemDeployData::Slash(_) => TransactionType::SlashingDeploy {
                    block_hash: block_hash_hex.clone(),
                },
                SystemDeployData::CloseBlock => TransactionType::CloseBlock {
                    block_hash: block_hash_hex.clone(),
                },
                SystemDeployData::Empty => continue,
            };
            if let Some(single_report) = s.report.first() {
                for t in Self::find_transactions(single_report, &self.transfer_unforgeable) {
                    out.push(TransactionInfo {
                        transaction: t,
                        transaction_type: tx_type.clone(),
                    });
                }
            }
        }
        Ok(out)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn transaction_type_variants() {
        assert_eq!(
            TransactionType::PreCharge {
                deploy_id: "d1".to_string()
            },
            TransactionType::PreCharge {
                deploy_id: "d1".to_string()
            }
        );
        assert_eq!(
            TransactionType::UserDeploy {
                deploy_id: "d1".to_string()
            },
            TransactionType::UserDeploy {
                deploy_id: "d1".to_string()
            }
        );
        assert_eq!(
            TransactionType::CloseBlock {
                block_hash: "b1".to_string()
            },
            TransactionType::CloseBlock {
                block_hash: "b1".to_string()
            }
        );
        assert_ne!(
            TransactionType::Refund {
                deploy_id: "d1".to_string()
            },
            TransactionType::SlashingDeploy {
                block_hash: "d1".to_string()
            }
        );
    }

    #[test]
    fn transaction_response_composes() {
        let tx = Transaction {
            from_addr: "a".to_string(),
            to_addr: "b".to_string(),
            amount: 100,
            ret_unforgeable: Par::default(),
            fail_reason: None,
        };
        let info = TransactionInfo {
            transaction: tx.clone(),
            transaction_type: TransactionType::UserDeploy {
                deploy_id: "d".to_string(),
            },
        };
        let response = TransactionResponse { data: vec![info] };
        assert_eq!(response.data.len(), 1);
        assert_eq!(response.data[0].transaction, tx);
        assert_eq!(response.data[0].transaction.amount, 100);
    }

    #[test]
    fn find_transactions_extracts_rev_transfer() {
        use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
        use rchain_models::casper::protocol::report::{
            ReportCommProto, ReportConsumeProto, ReportProduceProto,
        };
        use rchain_models::par_ops::from_expr;
        use rchain_models::runtime::ListParWithRandom;
        use rchain_models::sorted::SortedProc;

        let transfer = from_expr(Expr::GString("transfer".to_string()));
        let from = from_expr(Expr::GString("fromAddr".to_string()));
        let to = from_expr(Expr::GString("toAddr".to_string()));
        let amount = from_expr(Expr::GInt(123));
        let ret = from_expr(Expr::GString("ret".to_string()));

        let produce = ReportProduceProto {
            channel: transfer.clone(),
            data: ListParWithRandom {
                pars: vec![
                    SortedProc::new(from.clone()),
                    SortedProc::new(from.clone()),
                    SortedProc::new(to.clone()),
                    SortedProc::new(amount.clone()),
                    SortedProc::new(from.clone()),
                    SortedProc::new(ret.clone()),
                ],
                random_state: Blake2b512Random::default_random(),
            },
        };
        let comm = ReportCommProto {
            consume: ReportConsumeProto {
                channels: vec![transfer.clone()],
                patterns: vec![],
                peeks: vec![],
            },
            produces: vec![produce],
        };
        let report = SingleReport {
            events: vec![ReportProto::Comm(comm)],
        };

        let txs = TransactionAPIImpl::find_transactions(&report, &transfer);
        assert_eq!(txs.len(), 1);
        assert_eq!(txs[0].from_addr, "fromAddr");
        assert_eq!(txs[0].to_addr, "toAddr");
        assert_eq!(txs[0].amount, 123);
        assert_eq!(txs[0].ret_unforgeable, ret);
        assert_eq!(txs[0].fail_reason, None);
    }
}
