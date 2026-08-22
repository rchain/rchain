//! `Par` ⇄ `RhoExpr` mapping (port of the `RhoExpr` tree in `api/WebApi.scala`).
//!
//! `RhoExpr` is the JSON-ish representation of rholang data the web API exposes. `expr_from_par`
//! converts a protobuf `Par` to it and `rho_expr_to_par` converts back.

use rchain_models::ast::{Bundle, Expr, GUnforgeable, Par};
use rchain_models::rholang::RhoType::{
    RhoBoolean, RhoByteArray, RhoDeployId, RhoDeployerId, RhoList, RhoMap, RhoName, RhoNumber,
    RhoSet, RhoString, RhoTupleN, RhoUri,
};
use rchain_shared::base16;
use serde::{Deserialize, Serialize};

/// Rholang terms interesting for translation to JSON (port of `RhoExpr`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum RhoExpr {
    ExprPar(Vec<RhoExpr>),
    ExprTuple(Vec<RhoExpr>),
    ExprList(Vec<RhoExpr>),
    ExprSet(Vec<RhoExpr>),
    ExprMap(Vec<(String, RhoExpr)>),
    ExprBool(bool),
    ExprInt(i64),
    ExprString(String),
    ExprUri(String),
    ExprBytes(String),
    ExprUnforg(RhoUnforg),
}

/// An unforgeable name (port of `RhoUnforg`).
#[derive(Clone, Debug, PartialEq, Eq, Serialize, Deserialize)]
pub enum RhoUnforg {
    UnforgPrivate(String),
    UnforgDeploy(String),
    UnforgDeployer(String),
}

/// Convert a `Par` to a `RhoExpr` (port of `exprFromParProto`).
pub fn expr_from_par(par: &Par) -> Option<RhoExpr> {
    let mut exprs = Vec::new();
    for e in &par.exprs {
        if let Some(r) = expr_from_expr(e) {
            exprs.push(r);
        }
    }
    for u in &par.unforgeables {
        if let Some(r) = unforg_from_proto(u) {
            exprs.push(r);
        }
    }
    for b in &par.bundles {
        if let Some(r) = expr_from_bundle(b) {
            exprs.push(r);
        }
    }

    // Implements the semantic of Par with Unit: P | Nil ==> P.
    match exprs.len() {
        1 => exprs.into_iter().next(),
        0 => None,
        _ => Some(RhoExpr::ExprPar(exprs)),
    }
}

fn expr_from_expr(exp: &Expr) -> Option<RhoExpr> {
    match exp {
        Expr::GBool(b) => Some(RhoExpr::ExprBool(*b)),
        Expr::GInt(i) => Some(RhoExpr::ExprInt(*i)),
        Expr::GString(s) => Some(RhoExpr::ExprString(s.clone())),
        Expr::GUri(u) => Some(RhoExpr::ExprUri(u.clone())),
        Expr::GByteArray(bs) => Some(RhoExpr::ExprBytes(base16::encode(bs))),
        Expr::ETuple(t) => Some(RhoExpr::ExprTuple(
            t.ps.iter().filter_map(expr_from_par).collect(),
        )),
        Expr::EList(l) => Some(RhoExpr::ExprList(
            l.ps.iter().filter_map(expr_from_par).collect(),
        )),
        Expr::ESet(s) => Some(RhoExpr::ExprSet(
            s.ps.iter().filter_map(expr_from_par).collect(),
        )),
        Expr::EMap(m) => {
            let mut fields = Vec::new();
            for (k, v) in &m.kvs {
                let Some(key_expr) = expr_from_par(k) else {
                    continue;
                };
                let Some(key) = key_to_string(&key_expr) else {
                    continue;
                };
                let Some(value) = expr_from_par(v) else {
                    continue;
                };
                fields.push((key, value));
            }
            Some(RhoExpr::ExprMap(fields))
        }
        _ => None,
    }
}

fn unforg_from_proto(un: &GUnforgeable) -> Option<RhoExpr> {
    match un {
        GUnforgeable::GPrivate(g) => Some(RhoExpr::ExprUnforg(RhoUnforg::UnforgPrivate(
            base16::encode(&g.id),
        ))),
        GUnforgeable::GDeployId(d) => Some(RhoExpr::ExprUnforg(RhoUnforg::UnforgDeploy(
            base16::encode(&d.sig),
        ))),
        GUnforgeable::GDeployerId(d) => Some(RhoExpr::ExprUnforg(RhoUnforg::UnforgDeployer(
            base16::encode(&d.public_key),
        ))),
        _ => None,
    }
}

fn expr_from_bundle(b: &Bundle) -> Option<RhoExpr> {
    expr_from_par(&b.body)
}

/// Stringify a map key `RhoExpr` (port of the `keyExpr match` in `exprFromExprProto`).
fn key_to_string(key: &RhoExpr) -> Option<String> {
    match key {
        RhoExpr::ExprString(s) => Some(s.clone()),
        RhoExpr::ExprInt(n) => Some(n.to_string()),
        RhoExpr::ExprBool(b) => Some(b.to_string()),
        RhoExpr::ExprUri(u) => Some(u.clone()),
        RhoExpr::ExprUnforg(u) => match u {
            RhoUnforg::UnforgPrivate(hex)
            | RhoUnforg::UnforgDeploy(hex)
            | RhoUnforg::UnforgDeployer(hex) => Some(hex.clone()),
        },
        RhoExpr::ExprBytes(hex) => Some(hex.clone()),
        _ => None,
    }
}

/// Convert a `RhoExpr` back to a `Par` (port of `rhoExprToParProto`). Hex-encoded leaves are
/// validated (failing on non-hex input) rather than silently corrupted.
pub fn rho_expr_to_par(exp: &RhoExpr) -> Result<Par, String> {
    match exp {
        RhoExpr::ExprPar(data) => {
            let pars: Result<Vec<Par>, String> = data.iter().map(rho_expr_to_par).collect();
            Ok(pars?.into_iter().fold(Par::default(), |acc, p| acc.par_merge(&p)))
        }
        RhoExpr::ExprTuple(data) => {
            let pars: Result<Vec<Par>, String> = data.iter().map(rho_expr_to_par).collect();
            Ok(RhoTupleN::apply(pars?))
        }
        RhoExpr::ExprList(data) => {
            let pars: Result<Vec<Par>, String> = data.iter().map(rho_expr_to_par).collect();
            Ok(RhoList::apply(pars?))
        }
        RhoExpr::ExprSet(data) => {
            let pars: Result<Vec<Par>, String> = data.iter().map(rho_expr_to_par).collect();
            Ok(RhoSet::apply(pars?))
        }
        RhoExpr::ExprMap(data) => {
            let kvs: Result<Vec<(String, Par)>, String> = data
                .iter()
                .map(|(k, v)| rho_expr_to_par(v).map(|p| (k.clone(), p)))
                .collect();
            Ok(RhoMap::apply(
                kvs?.into_iter()
                    .map(|(k, v)| (RhoString::apply(k), v))
                    .collect(),
            ))
        }
        RhoExpr::ExprBool(b) => Ok(RhoBoolean::apply(*b)),
        RhoExpr::ExprInt(i) => Ok(RhoNumber::apply(*i)),
        RhoExpr::ExprString(s) => Ok(RhoString::apply(s.clone())),
        RhoExpr::ExprUri(u) => Ok(RhoUri::apply(u.clone())),
        RhoExpr::ExprBytes(hex) => Ok(RhoByteArray::apply(base16::try_decode(hex)?)),
        RhoExpr::ExprUnforg(u) => unforg_to_par(u),
    }
}

/// Convert a `RhoUnforg` to a `Par` (port of `unforgToParProto`), validating the hex-encoded name.
pub fn unforg_to_par(unforg: &RhoUnforg) -> Result<Par, String> {
    match unforg {
        RhoUnforg::UnforgPrivate(name) => Ok(RhoName::apply_bytes(base16::try_decode(name)?)),
        RhoUnforg::UnforgDeploy(name) => Ok(RhoDeployId::apply(base16::try_decode(name)?)),
        RhoUnforg::UnforgDeployer(name) => Ok(RhoDeployerId::apply(base16::try_decode(name)?)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn round_trip(e: &RhoExpr) {
        assert_eq!(expr_from_par(&rho_expr_to_par(e).unwrap()), Some(e.clone()));
    }

    #[test]
    fn terminal_exprs_round_trip() {
        round_trip(&RhoExpr::ExprBool(true));
        round_trip(&RhoExpr::ExprBool(false));
        round_trip(&RhoExpr::ExprInt(42));
        round_trip(&RhoExpr::ExprInt(-7));
        round_trip(&RhoExpr::ExprString("hello".to_string()));
        round_trip(&RhoExpr::ExprUri("rho:id:123".to_string()));
        round_trip(&RhoExpr::ExprBytes("deadbeef".to_string()));
        round_trip(&RhoExpr::ExprUnforg(RhoUnforg::UnforgPrivate(
            "abcd1234".to_string(),
        )));
        round_trip(&RhoExpr::ExprUnforg(RhoUnforg::UnforgDeploy(
            "deadbeef".to_string(),
        )));
        round_trip(&RhoExpr::ExprUnforg(RhoUnforg::UnforgDeployer(
            "00112233".to_string(),
        )));
    }

    #[test]
    fn collection_exprs_round_trip() {
        round_trip(&RhoExpr::ExprTuple(vec![RhoExpr::ExprInt(1), RhoExpr::ExprBool(false)]));
        round_trip(&RhoExpr::ExprList(vec![
            RhoExpr::ExprString("a".to_string()),
            RhoExpr::ExprInt(2),
        ]));
        round_trip(&RhoExpr::ExprSet(vec![RhoExpr::ExprInt(1), RhoExpr::ExprInt(2)]));
        round_trip(&RhoExpr::ExprMap(vec![(
            "key".to_string(),
            RhoExpr::ExprInt(1),
        )]));
        round_trip(&RhoExpr::ExprPar(vec![
            RhoExpr::ExprBool(true),
            RhoExpr::ExprInt(1),
            RhoExpr::ExprString("s".to_string()),
            RhoExpr::ExprUri("u".to_string()),
            RhoExpr::ExprBytes("beef".to_string()),
            RhoExpr::ExprUnforg(RhoUnforg::UnforgPrivate("abcd".to_string())),
        ]));
    }

    #[test]
    fn non_json_exprs_map_to_none() {
        // An arithmetic expression has no RhoExpr representation.
        let p = Par {
            exprs: vec![Expr::EPlus(Box::default(), Box::default())],
            ..Default::default()
        };
        assert_eq!(expr_from_par(&p), None);
    }
}
