//! Pretty-print the tuplespace state (port of `interpreter/storage/StoragePrinter.scala`).
//!
//! The deploy-evaluation variants (`prettyPrintUnmatchedSends(deploy, …)`) depend on `evaluate`
//! (parse + run), which is not yet ported; only the hot-changes snapshot printers are provided.

use rchain_models::ast::{AlwaysEqual, Par, Receive, ReceiveBind, Send};
use rchain_models::par_ops::{par_concat, prepend_receive, prepend_send};
use rchain_models::types::FreeCount;
use rchain_models::runtime::{BindPattern, ListParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_rspace::internal::{Datum, WaitingContinuation};

use crate::pretty_printer::PrettyPrinter;
use crate::runtime::RhoRuntime;

pub const NO_UNMATCHED_SENDS: &str = "No unmatched sends.";
const EMPTY_SPACE: &str =
    "The space is empty. Note that top level terms that are not sends or receives are discarded.";

/// Render the full hot-changes snapshot as rholang (port of `StoragePrinter.prettyPrint`).
pub async fn pretty_print(runtime: &RhoRuntime) -> String {
    let mapped = runtime.get_hot_changes().await;
    let pars: Vec<Par> = mapped
        .iter()
        .map(|(channels, row)| {
            if row.data.is_empty() && row.wks.is_empty() {
                Par::default()
            } else if row.wks.is_empty() {
                to_sends(&row.data, channels)
            } else if row.data.is_empty() {
                to_receive(&row.wks, channels)
            } else {
                par_concat(
                    &to_sends(&row.data, channels),
                    &to_receive(&row.wks, channels),
                )
            }
        })
        .collect();

    if pars.is_empty() {
        EMPTY_SPACE.to_string()
    } else {
        let merged = pars.into_iter().reduce(|a, b| par_concat(&a, &b)).unwrap_or_else(|| Par::default());
        PrettyPrinter::new().build_string(&merged)
    }
}

/// Render only the unmatched sends (produced data) (port of
/// `StoragePrinter.prettyPrintUnmatchedSends(runtime)`).
pub async fn pretty_print_unmatched_sends(runtime: &RhoRuntime) -> String {
    let mapped = runtime.get_hot_changes().await;
    let pars: Vec<Par> = mapped.iter().map(|(channels, row)| to_sends(&row.data, channels)).collect();
    if pars.is_empty() {
        NO_UNMATCHED_SENDS.to_string()
    } else {
        let merged = pars.into_iter().reduce(|a, b| par_concat(&a, &b)).unwrap_or_else(|| Par::default());
        PrettyPrinter::new().build_string(&merged)
    }
}

fn to_sends(data: &[Datum<ListParWithRandom>], channels: &[SortedProc]) -> Par {
    let mut acc = Par::default();
    for datum in data {
        for channel in channels {
            let send = Send {
                chan: Box::new(channel.as_par().clone().quote()),
                data: datum.a.pars.iter().map(|p| p.as_par().clone().quote()).collect(),
                persistent: datum.persist,
                locally_free: AlwaysEqual(vec![]),
                connective_used: false,
            };
            acc = prepend_send(&acc, send);
        }
    }
    acc
}

fn to_receive(wks: &[WaitingContinuation<BindPattern, TaggedContinuation>], channels: &[SortedProc]) -> Par {
    let mut acc = Par::default();
    for wk in wks {
        let binds: Vec<ReceiveBind> = channels
            .iter()
            .zip(wk.patterns.iter())
            .map(|(channel, pattern)| ReceiveBind {
                patterns: pattern.patterns.iter().map(|p| p.as_par().clone().quote()).collect(),
                source: Box::new(channel.as_par().clone().quote()),
                remainder: pattern.remainder.clone().map(Box::new),
                free_count: FreeCount::from_nonneg(pattern.free_count),
            })
            .collect();
        let (body, bind_count) = match &wk.continuation {
            TaggedContinuation::ParBody(p) => (
                p.body.as_par().clone(),
                wk.patterns.iter().map(|p| p.free_count).sum(),
            ),
            _ => (Par::default(), 0),
        };
        let receive = Receive {
            binds,
            body: Box::new(body),
            persistent: wk.persist,
            peek: !wk.peeks.is_empty(),
            bind_count,
            locally_free: AlwaysEqual(vec![]),
            connective_used: false,
        };
        acc = prepend_receive(&acc, receive);
    }
    acc
}
