//! System deploy types + concrete deploys (port of `casper/rholang/types/SystemDeploy*.scala`
//! and `casper/rholang/sysdeploys/`).

use std::collections::{BTreeMap, BTreeSet};

use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_crypto::public_key::PublicKey;
use rchain_models::ast::Par;
use rchain_models::casper::protocol::casper_message::Event;
use rchain_models::rholang::RhoType::{RhoBoolean, RhoString, RhoTupleN};
use rchain_models::validator::Validator;

/// A user-level system-deploy error (port of `SystemDeployUserError`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SystemDeployUserError(pub String);

/// A fatal platform failure (port of `SystemDeployPlatformFailure`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SystemDeployPlatformFailure {
    UnexpectedResult(Vec<Par>),
    UnexpectedSystemErrors(String),
    GasRefundFailure(String),
    ConsumeFailed,
}

/// Accumulated deploy events + mergeable channels (port of `EvalCollector`).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct EvalCollector {
    pub event_log: Vec<Event>,
    pub mergeable_channels: BTreeSet<Par>,
}

impl EvalCollector {
    pub fn add(&self, log: &[Event], merge_chs: &BTreeSet<Par>) -> EvalCollector {
        let mut event_log = self.event_log.clone();
        event_log.extend(log.iter().cloned());
        let mut mergeable_channels = self.mergeable_channels.clone();
        mergeable_channels.extend(merge_chs.iter().cloned());
        EvalCollector {
            event_log,
            mergeable_channels,
        }
    }
}

/// The outcome of playing a system deploy (port of `SystemDeployResult`).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SystemDeployResult<A> {
    PlaySucceeded {
        state_hash: Vec<u8>,
        event_log: Vec<Event>,
        mergeable_channels: BTreeMap<rchain_crypto::hash::blake2b256_hash::Blake2b256Hash, i64>,
        result: A,
    },
    PlayFailed {
        event_log: Vec<Event>,
        error_msg: String,
    },
}

/// A system deploy: the rholang source plus its normalizer environment (port of `SystemDeploy`).
///
/// Rust-first: the pre-charge/refund/close-block/slash system deploys are now **native** operations
/// (see [`NativeSystemDeployOp`]); `source`/`normalizer_env`/`return_channel` remain only for the
/// legacy rholang path, which is no longer constructed.
pub struct SystemDeploy {
    pub source: &'static str,
    pub normalizer_env: BTreeMap<String, Par>,
    pub rand: Blake2b512Random,
    pub return_channel: Par,
    /// A native system-deploy operation; `Some` makes this deploy bypass the rholang source path.
    pub op: Option<NativeSystemDeployOp>,
}

/// A native system-deploy operation (rust-first replacement for the rholang PoS/registry
/// system-deploy sources; the Scala sources are a checklist only).
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NativeSystemDeployOp {
    PreCharge { deployer: PublicKey, amount: i64 },
    Refund { amount: i64 },
    CloseBlock,
    Slash { validator: Validator },
}

impl SystemDeploy {
    pub fn pre_charge(amount: i64, pk: &PublicKey, rand: Blake2b512Random) -> SystemDeploy {
        SystemDeploy {
            source: "",
            normalizer_env: BTreeMap::new(),
            rand,
            return_channel: Par::default(),
            op: Some(NativeSystemDeployOp::PreCharge { deployer: pk.to_owned(), amount }),
        }
    }

    pub fn refund(amount: i64, rand: Blake2b512Random) -> SystemDeploy {
        SystemDeploy {
            source: "",
            normalizer_env: BTreeMap::new(),
            rand,
            return_channel: Par::default(),
            op: Some(NativeSystemDeployOp::Refund { amount }),
        }
    }

    pub fn close_block(rand: Blake2b512Random) -> SystemDeploy {
        SystemDeploy {
            source: "",
            normalizer_env: BTreeMap::new(),
            rand,
            return_channel: Par::default(),
            op: Some(NativeSystemDeployOp::CloseBlock),
        }
    }

    pub fn slash(validator: &Validator, rand: Blake2b512Random) -> SystemDeploy {
        SystemDeploy {
            source: "",
            normalizer_env: BTreeMap::new(),
            rand,
            return_channel: Par::default(),
            op: Some(NativeSystemDeployOp::Slash { validator: *validator }),
        }
    }
}

/// Interpret the `(Bool, Either[String, Nil])` result of the charge/refund/close/slash deploys
/// (port of their shared `processResult`).
///
/// The `Either[String, Nil]` is a bare `GString` on `Left` (the error message) or `Nil` on `Right`;
/// a `(true, _)` result succeeds, `(false, Left(msg))` fails with `msg`, and anything else fails
/// with `<no cause>`.
pub fn process_bool_result(output: &Par) -> Result<(), SystemDeployUserError> {
    let parts = RhoTupleN::unapply(output)
        .ok_or_else(|| SystemDeployUserError("<no cause>".to_string()))?;
    let success = parts
        .first()
        .and_then(RhoBoolean::unapply)
        .ok_or_else(|| SystemDeployUserError("<no cause>".to_string()))?;
    if success {
        return Ok(());
    }
    let error = parts
        .get(1)
        .and_then(RhoString::unapply)
        .map(|s| SystemDeployUserError(s.to_string()))
        .unwrap_or_else(|| SystemDeployUserError("<no cause>".to_string()));
    Err(error)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn pre_charge_is_native() {
        let pk = PublicKey::new(vec![1u8; 65]);
        let rand = Blake2b512Random::new_random(128);
        let d = SystemDeploy::pre_charge(100, &pk, rand);
        assert_eq!(
            d.op,
            Some(NativeSystemDeployOp::PreCharge { deployer: pk, amount: 100 })
        );
    }

    #[test]
    fn process_bool_result_interprets_tuple() {
        use rchain_models::rholang::RhoType::{RhoBoolean, RhoNil, RhoString, RhoTupleN};

        // (true, _) succeeds regardless of the Either.
        let ok = RhoTupleN::apply(vec![RhoBoolean::apply(true), RhoNil::apply()]);
        assert_eq!(process_bool_result(&ok), Ok(()));

        // (false, Left("boom")) fails with the message.
        let fail = RhoTupleN::apply(vec![
            RhoBoolean::apply(false),
            RhoString::apply("boom".to_string()),
        ]);
        assert_eq!(
            process_bool_result(&fail),
            Err(SystemDeployUserError("boom".to_string()))
        );

        // (false, Right(Nil)) fails with no cause.
        let fail_nil = RhoTupleN::apply(vec![RhoBoolean::apply(false), RhoNil::apply()]);
        assert_eq!(
            process_bool_result(&fail_nil),
            Err(SystemDeployUserError("<no cause>".to_string()))
        );

        // A malformed result fails with no cause.
        assert_eq!(
            process_bool_result(&Par::default()),
            Err(SystemDeployUserError("<no cause>".to_string()))
        );
    }
}
