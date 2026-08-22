//! The pure expression-evaluation surface of the reducer (Law 4).
//!
//! Mirrors `Reduce.scala` (`evalExpr` / `evalExprToExpr` / `evalSingleExpr` / `evalToBool` and the
//! arithmetic/comparison/boolean/string/method helpers), the effectful term dispatch
//! (`eval(Send/Receive/New/Match/Bundle)`, `produce`/`consume`, `new` allocation), and the
//! collection methods (`union`/`diff`/`add`/`delete`/`contains`/`slice`/`keys`).

use std::collections::{BTreeMap, BTreeSet};
use std::sync::Mutex;

use async_trait::async_trait;
use num_bigint::BigInt;
use rchain_crypto::hash::blake2b512_random::Blake2b512Random;
use rchain_models::ast::{
    AlwaysEqual, Bundle, EList, ETuple, Expr, GPrivate, GUnforgeable, Match, MatchCase, Name, New,
    Par, ParMap, ParSet, Receive, ReceiveBind, Send, Sort, SortJoin, Var,
};
use rchain_models::par_ops::{from_expr, par_concat, single_bundle, single_expr, typ};
use rchain_models::runtime::{BindPattern, ListParWithRandom, ParWithRandom, TaggedContinuation};
use rchain_models::sorted::SortedProc;
use rchain_models::sorter::{par_map, par_set};

use crate::accounting::{CostAccounting, Costs};
use crate::env::Env;
use crate::errors::RholangError;
use crate::matcher::spatial_match_result;
use crate::substitute::{substitute_par, substitute_par_and_charge};

fn union_free(a: Vec<i32>, b: Vec<i32>) -> Vec<i32> {
    let mut set: BTreeSet<i32> = a.into_iter().collect();
    set.extend(b);
    set.into_iter().collect()
}

/// Recompute a `Par`'s cached `locallyFree` from its sub-terms (port of `updateLocallyFree`).
pub fn update_locally_free(par: &Par) -> Par {
    let mut free = Vec::new();
    for s in &par.sends {
        free = union_free(free, s.locally_free.0.clone());
    }
    for r in &par.receives {
        free = union_free(free, r.locally_free.0.clone());
    }
    for n in &par.news {
        free = union_free(free, n.locally_free.0.clone());
    }
    for e in &par.exprs {
        free = union_free(free, rchain_models::par_ops::locally_free_of_expr(e, 0));
    }
    for m in &par.matches {
        free = union_free(free, m.locally_free.0.clone());
    }
    for b in &par.bundles {
        free = union_free(free, b.body.locally_free.0.clone());
    }
    Par {
        locally_free: AlwaysEqual(free),
        ..par.clone()
    }
}

fn eval_var(v: &Var, env: &Env<Par>, cost: &CostAccounting) -> Result<Par, RholangError> {
    cost.charge(Costs::var_eval_cost())?;
    match v {
        Var::BoundVar(level) => env.get(*level).ok_or_else(|| {
            RholangError::ReduceError(format!("Unbound variable: {level}"))
        }),
        Var::Wildcard | Var::FreeVar(_) => Err(RholangError::ReduceError(
            "Unbound variable: attempting to evaluate a pattern".to_string(),
        )),
        Var::Empty => Err(RholangError::ReduceError(
            "Impossible var instance EMPTY".to_string(),
        )),
    }
}

fn eval_to_bool(par: &Par, env: &Env<Par>, cost: &CostAccounting) -> Result<bool, RholangError> {
    match eval_single_expr(par, env, cost)? {
        Expr::GBool(b) => Ok(b),
        other => Err(RholangError::ReduceError(format!(
            "Error: expected Bool, got {}",
            typ(&other)
        ))),
    }
}

fn eval_to_long(par: &Par, env: &Env<Par>, cost: &CostAccounting) -> Result<i64, RholangError> {
    match eval_single_expr(par, env, cost)? {
        Expr::GInt(v) => Ok(v),
        other => Err(RholangError::ReduceError(format!(
            "Error: expected Int, got {}",
            typ(&other)
        ))),
    }
}

fn restrict_to_int(n: i64) -> Result<usize, RholangError> {
    if n > i32::MAX as i64 || n < i32::MIN as i64 {
        Err(RholangError::ReduceError(format!(
            "Error: value out of range: {n}"
        )))
    } else {
        Ok(n as usize)
    }
}

pub fn eval_single_expr<S: Sort>(
    par: &Par<S>,
    env: &Env<Par>,
    cost: &CostAccounting,
) -> Result<Expr, RholangError> {
    match single_expr(par) {
        Some(expr) => eval_expr_to_expr(expr, env, cost),
        None => Err(RholangError::ReduceError(
            "Expected a single expression".to_string(),
        )),
    }
}

fn relop(
    p1: &Par,
    p2: &Par,
    relopb: fn(bool, bool) -> bool,
    relopi: fn(i64, i64) -> bool,
    relopbi: fn(&BigInt, &BigInt) -> bool,
    relops: fn(&str, &str) -> bool,
    env: &Env<Par>,
    cost: &CostAccounting,
) -> Result<Expr, RholangError> {
    let v1 = eval_single_expr(p1, env, cost)?;
    let v2 = eval_single_expr(p2, env, cost)?;
    Ok(match (&v1, &v2) {
        (Expr::GBool(b1), Expr::GBool(b2)) => {
            cost.charge(Costs::comparison_cost())?;
            Expr::GBool(relopb(*b1, *b2))
        }
        (Expr::GInt(i1), Expr::GInt(i2)) => {
            cost.charge(Costs::comparison_cost())?;
            Expr::GBool(relopi(*i1, *i2))
        }
        (Expr::GBigInt(b1), Expr::GBigInt(b2)) => {
            cost.charge(Costs::big_int_comparison(b1, b2))?;
            Expr::GBool(relopbi(b1, b2))
        }
        (Expr::GString(s1), Expr::GString(s2)) => {
            cost.charge(Costs::comparison_cost())?;
            Expr::GBool(relops(s1, s2))
        }
        _ => {
            return Err(RholangError::ReduceError(format!(
                "Unexpected compare: {v1:?} vs. {v2:?}"
            )))
        }
    })
}

fn eval_to_string_pair(
    key: &Expr,
    value: &Expr,
) -> Result<(String, String), RholangError> {
    match (key, value) {
        (Expr::GString(k), Expr::GString(v)) => Ok((k.clone(), v.clone())),
        (Expr::GString(k), Expr::GInt(v)) => Ok((k.clone(), v.to_string())),
        (Expr::GString(k), Expr::GBigInt(v)) => Ok((k.clone(), v.to_string())),
        (Expr::GString(k), Expr::GBool(v)) => Ok((k.clone(), v.to_string())),
        (Expr::GString(k), Expr::GUri(v)) => Ok((k.clone(), v.clone())),
        (Expr::GString(_), value) => Err(RholangError::ReduceError(format!(
            "Error: interpolation doesn't support {}",
            typ(value)
        ))),
        _ => Err(RholangError::ReduceError(
            "Error: interpolation Map should only contain String keys".to_string(),
        )),
    }
}

fn interpolate(string: &str, pairs: &[(String, String)]) -> String {
    let mut result = String::new();
    let mut current = string;
    while !current.is_empty() {
        match pairs.iter().find(|(k, _)| current.starts_with(&format!("${{{k}}}"))) {
            Some((k, v)) => {
                result.push_str(v);
                current = &current[k.len() + 3..];
            }
            None => {
                let mut chars = current.chars();
                let c = match chars.next() {
                    Some(c) => c,
                    // Unreachable: `current` is non-empty by the loop guard.
                    None => break,
                };
                result.push(c);
                current = chars.as_str();
            }
        }
    }
    result
}

fn eval_expr_to_par<S: Sort>(expr: &Expr, env: &Env<Par>, cost: &CostAccounting) -> Result<Par<S>, RholangError> {
    match expr {
        Expr::EVar(v) => {
            let p = eval_var(v, env, cost)?;
            eval_expr(&p, env, cost).map(|r| r.re_sort())
        }
        Expr::EMethod(em) => {
            cost.charge(Costs::method_call_cost())?;
            let evaled_target = eval_expr(&em.target, env, cost)?;
            let evaled_args: Vec<Par> = em
                .arguments
                .iter()
                .map(|a| eval_expr(a, env, cost))
                .collect::<Result<_, _>>()?;
            eval_method(&em.method_name, &evaled_target, &evaled_args, env, cost).map(|r| r.re_sort())
        }
        _ => Ok(from_expr(eval_expr_to_expr(expr, env, cost)?).re_sort()),
    }
}

fn eval_expr_to_expr(expr: &Expr, env: &Env<Par>, cost: &CostAccounting) -> Result<Expr, RholangError> {
    match expr {
        Expr::GBool(_)
        | Expr::GInt(_)
        | Expr::GBigInt(_)
        | Expr::GString(_)
        | Expr::GUri(_)
        | Expr::GByteArray(_) => Ok(expr.clone()),
        Expr::ENot(p) => Ok(Expr::GBool(!eval_to_bool(p, env, cost)?)),
        Expr::ENeg(p) => {
            let v = eval_single_expr(p, env, cost)?;
            match v {
                Expr::GInt(hs) => Ok(Expr::GInt(-hs)),
                Expr::GBigInt(hs) => {
                    let r = -hs;
                    cost.charge(Costs::big_int_negation(&r))?;
                    Ok(Expr::GBigInt(r))
                }
                other => Err(RholangError::OperatorNotDefined {
                    op: "Negation".to_string(),
                    other_type: typ(&other).to_string(),
                }),
            }
        }
        Expr::EMult(p1, p2) => {
            let v1 = eval_single_expr(p1, env, cost)?;
            let v2 = eval_single_expr(p2, env, cost)?;
            match (&v1, &v2) {
                (Expr::GInt(l), Expr::GInt(r)) => {
                    cost.charge(Costs::multiplication_cost())?;
                    Ok(Expr::GInt(l * r))
                }
                (Expr::GBigInt(l), Expr::GBigInt(r)) => {
                    cost.charge(Costs::big_int_multiplication(l, r))?;
                    Ok(Expr::GBigInt(l * r))
                }
                (Expr::GInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "*".to_string(),
                    expected: "Int".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (Expr::GBigInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "*".to_string(),
                    expected: "BigInt".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (o, _) => Err(RholangError::OperatorNotDefined {
                    op: "*".to_string(),
                    other_type: typ(o).to_string(),
                }),
            }
        }
        Expr::EDiv(p1, p2) => {
            let v1 = eval_single_expr(p1, env, cost)?;
            let v2 = eval_single_expr(p2, env, cost)?;
            match (&v1, &v2) {
                (Expr::GInt(l), Expr::GInt(r)) => {
                    if *r == 0 {
                        return Err(RholangError::ReduceError("/ by zero".to_string()));
                    }
                    cost.charge(Costs::division_cost())?;
                    Ok(Expr::GInt(l / r))
                }
                (Expr::GBigInt(l), Expr::GBigInt(r)) => {
                    if *r == BigInt::from(0i64) {
                        return Err(RholangError::ReduceError("/ by zero".to_string()));
                    }
                    cost.charge(Costs::big_int_division(l, r))?;
                    Ok(Expr::GBigInt(l / r))
                }
                (Expr::GInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "/".to_string(),
                    expected: "Int".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (Expr::GBigInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "/".to_string(),
                    expected: "BigInt".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (o, _) => Err(RholangError::OperatorNotDefined {
                    op: "/".to_string(),
                    other_type: typ(o).to_string(),
                }),
            }
        }
        Expr::EMod(p1, p2) => {
            let v1 = eval_single_expr(p1, env, cost)?;
            let v2 = eval_single_expr(p2, env, cost)?;
            match (&v1, &v2) {
                (Expr::GInt(l), Expr::GInt(r)) => {
                    if *r == 0 {
                        return Err(RholangError::ReduceError("/ by zero".to_string()));
                    }
                    cost.charge(Costs::modulo_cost())?;
                    Ok(Expr::GInt(l % r))
                }
                (Expr::GBigInt(l), Expr::GBigInt(r)) => {
                    if *r == BigInt::from(0i64) {
                        return Err(RholangError::ReduceError("/ by zero".to_string()));
                    }
                    cost.charge(Costs::big_int_modulo(l, r))?;
                    Ok(Expr::GBigInt(l % r))
                }
                (Expr::GInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "%".to_string(),
                    expected: "Int".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (Expr::GBigInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "%".to_string(),
                    expected: "BigInt".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (o, _) => Err(RholangError::OperatorNotDefined {
                    op: "%".to_string(),
                    other_type: typ(o).to_string(),
                }),
            }
        }
        Expr::EPlus(p1, p2) => {
            let v1 = eval_single_expr(p1, env, cost)?;
            let v2 = eval_single_expr(p2, env, cost)?;
            match (&v1, &v2) {
                (Expr::GInt(l), Expr::GInt(r)) => {
                    cost.charge(Costs::sum_cost())?;
                    Ok(Expr::GInt(l + r))
                }
                (Expr::GBigInt(l), Expr::GBigInt(r)) => {
                    cost.charge(Costs::big_int_sum(l, r))?;
                    Ok(Expr::GBigInt(l + r))
                }
                (Expr::GInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "+".to_string(),
                    expected: "Int".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (Expr::GBigInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "+".to_string(),
                    expected: "BigInt".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (o, _) => Err(RholangError::OperatorNotDefined {
                    op: "+".to_string(),
                    other_type: typ(o).to_string(),
                }),
            }
        }
        Expr::EMinus(p1, p2) => {
            let v1 = eval_single_expr(p1, env, cost)?;
            let v2 = eval_single_expr(p2, env, cost)?;
            match (&v1, &v2) {
                (Expr::GInt(l), Expr::GInt(r)) => {
                    cost.charge(Costs::subtraction_cost())?;
                    Ok(Expr::GInt(l - r))
                }
                (Expr::GBigInt(l), Expr::GBigInt(r)) => {
                    cost.charge(Costs::big_int_subtraction(l, r))?;
                    Ok(Expr::GBigInt(l - r))
                }
                (Expr::GInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "-".to_string(),
                    expected: "Int".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (Expr::GBigInt(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "-".to_string(),
                    expected: "BigInt".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (o, _) => Err(RholangError::OperatorNotDefined {
                    op: "-".to_string(),
                    other_type: typ(o).to_string(),
                }),
            }
        }
        Expr::ELt(p1, p2) => relop(p1, p2, |a, b| a < b, |a, b| a < b, |a, b| a < b, |a, b| a < b, env, cost),
        Expr::ELte(p1, p2) => {
            relop(p1, p2, |a, b| a <= b, |a, b| a <= b, |a, b| a <= b, |a, b| a <= b, env, cost)
        }
        Expr::EGt(p1, p2) => relop(p1, p2, |a, b| a > b, |a, b| a > b, |a, b| a > b, |a, b| a > b, env, cost),
        Expr::EGte(p1, p2) => {
            relop(p1, p2, |a, b| a >= b, |a, b| a >= b, |a, b| a >= b, |a, b| a >= b, env, cost)
        }
        Expr::EEq(p1, p2) => {
            let v1 = eval_expr(p1, env, cost)?;
            let v2 = eval_expr(p2, env, cost)?;
            let sv1 = substitute_par_and_charge(&v1, 0, env, cost)?;
            let sv2 = substitute_par_and_charge(&v2, 0, env, cost)?;
            cost.charge(Costs::equality_check_cost(&sv1, &sv2))?;
            Ok(Expr::GBool(sv1 == sv2))
        }
        Expr::ENeq(p1, p2) => {
            let v1 = eval_expr(p1, env, cost)?;
            let v2 = eval_expr(p2, env, cost)?;
            let sv1 = substitute_par_and_charge(&v1, 0, env, cost)?;
            let sv2 = substitute_par_and_charge(&v2, 0, env, cost)?;
            cost.charge(Costs::equality_check_cost(&sv1, &sv2))?;
            Ok(Expr::GBool(sv1 != sv2))
        }
        Expr::EAnd(p1, p2) => {
            let b1 = eval_to_bool(p1, env, cost)?;
            let b2 = eval_to_bool(p2, env, cost)?;
            cost.charge(Costs::boolean_and_cost())?;
            Ok(Expr::GBool(b1 && b2))
        }
        Expr::EOr(p1, p2) => {
            let b1 = eval_to_bool(p1, env, cost)?;
            let b2 = eval_to_bool(p2, env, cost)?;
            cost.charge(Costs::boolean_or_cost())?;
            Ok(Expr::GBool(b1 || b2))
        }
        Expr::EShortAnd(p1, p2) => {
            let b1 = eval_to_bool(p1, env, cost)?;
            let b2 = if b1 { eval_to_bool(p2, env, cost)? } else { false };
            cost.charge(Costs::boolean_and_cost())?;
            Ok(Expr::GBool(b1 && b2))
        }
        Expr::EShortOr(p1, p2) => {
            let b1 = eval_to_bool(p1, env, cost)?;
            let b2 = if b1 { true } else { eval_to_bool(p2, env, cost)? };
            cost.charge(Costs::boolean_or_cost())?;
            Ok(Expr::GBool(b1 || b2))
        }
        Expr::EMatches(target, pattern) => {
            let evaled_target = eval_expr(target, env, cost)?;
            let subst_target = substitute_par_and_charge(&evaled_target, 0, env, cost)?;
            let subst_pattern = substitute_par_and_charge(pattern, 1, env, cost)?;
            let m = spatial_match_result(&subst_target, &subst_pattern)?;
            Ok(Expr::GBool(m.is_some()))
        }
        Expr::EPercentPercent(p1, p2) => {
            cost.charge(Costs::op_call_cost())?;
            let v1 = eval_single_expr(p1, env, cost)?;
            let v2 = eval_single_expr(p2, env, cost)?;
            match (&v1, &v2) {
                (Expr::GString(lhs), Expr::EMap(ParMap { kvs, .. })) => {
                    if lhs.is_empty() && kvs.is_empty() {
                        Ok(Expr::GString(lhs.clone()))
                    } else {
                        let mut pairs = Vec::new();
                        for (k, v) in kvs {
                            let key_expr = eval_single_expr(k, env, cost)?;
                            let value_expr = eval_single_expr(v, env, cost)?;
                            pairs.push(eval_to_string_pair(&key_expr, &value_expr)?);
                        }
                        cost.charge(Costs::interpolate_cost(lhs.len() as i64, kvs.len() as i64))?;
                        Ok(Expr::GString(interpolate(lhs, &pairs)))
                    }
                }
                (Expr::GString(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "%%".to_string(),
                    expected: "Map".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (o, _) => Err(RholangError::OperatorNotDefined {
                    op: "%%".to_string(),
                    other_type: typ(o).to_string(),
                }),
            }
        }
        Expr::EPlusPlus(p1, p2) => {
            cost.charge(Costs::op_call_cost())?;
            let v1 = eval_single_expr(p1, env, cost)?;
            let v2 = eval_single_expr(p2, env, cost)?;
            match (&v1, &v2) {
                (Expr::GString(l), Expr::GString(r)) => {
                    cost.charge(Costs::string_append_cost(l.len() as i64, r.len() as i64))?;
                    Ok(Expr::GString(format!("{l}{r}")))
                }
                (Expr::GByteArray(l), Expr::GByteArray(r)) => {
                    cost.charge(Costs::string_append_cost(l.len() as i64, r.len() as i64))?;
                    let mut out = l.clone();
                    out.extend(r);
                    Ok(Expr::GByteArray(out))
                }
                (Expr::EList(l), Expr::EList(r)) => {
                    cost.charge(Costs::list_append_cost((l.ps.len() + r.ps.len()) as i64))?;
                    let mut ps = l.ps.clone();
                    ps.extend(r.ps.clone());
                    Ok(Expr::EList(EList {
                        ps,
                        locally_free: AlwaysEqual(union_free(
                            l.locally_free.0.clone(),
                            r.locally_free.0.clone(),
                        )),
                        connective_used: l.connective_used || r.connective_used,
                        ..Default::default()
                    }))
                }
                (Expr::GString(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "++".to_string(),
                    expected: "String".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (Expr::EList(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "++".to_string(),
                    expected: "List".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (o, _) => Err(RholangError::OperatorNotDefined {
                    op: "++".to_string(),
                    other_type: typ(o).to_string(),
                }),
            }
        }
        Expr::EMinusMinus(p1, p2) => {
            cost.charge(Costs::op_call_cost())?;
            let v1 = eval_single_expr(p1, env, cost)?;
            let v2 = eval_single_expr(p2, env, cost)?;
            match (&v1, &v2) {
                (Expr::ESet(b), Expr::ESet(o)) => {
                    cost.charge(Costs::diff_cost(o.ps.len() as i64))?;
                    let ps: Vec<Par> = b.ps.iter().filter(|p| !o.ps.contains(p)).cloned().collect();
                    Ok(Expr::ESet(par_set(ps)))
                }
                (Expr::ESet(_), o) => Err(RholangError::OperatorExpectedError {
                    op: "--".to_string(),
                    expected: "Set".to_string(),
                    other_type: typ(o).to_string(),
                }),
                (o, _) => Err(RholangError::OperatorNotDefined {
                    op: "--".to_string(),
                    other_type: typ(o).to_string(),
                }),
            }
        }
        Expr::EVar(v) => {
            let p = eval_var(v, env, cost)?;
            eval_single_expr(&p, env, cost)
        }
        Expr::EList(el) => {
            let evaled: Vec<Par> = el
                .ps
                .iter()
                .map(|p| eval_expr(p, env, cost).map(|p| update_locally_free(&p)))
                .collect::<Result<_, _>>()?;
            Ok(Expr::EList(EList {
                ps: evaled,
                locally_free: el.locally_free.clone(),
                connective_used: el.connective_used,
                ..Default::default()
            }))
        }
        Expr::ETuple(el) => {
            let evaled: Vec<Par> = el
                .ps
                .iter()
                .map(|p| eval_expr(p, env, cost).map(|p| update_locally_free(&p)))
                .collect::<Result<_, _>>()?;
            Ok(Expr::ETuple(ETuple {
                ps: evaled,
                locally_free: el.locally_free.clone(),
                connective_used: el.connective_used,
            }))
        }
        Expr::ESet(set) => {
            let evaled: Vec<Par> = set
                .ps
                .iter()
                .map(|p| eval_expr(p, env, cost).map(|p| update_locally_free(&p)))
                .collect::<Result<_, _>>()?;
            let mut s = par_set(evaled);
            s.connective_used = set.connective_used;
            s.locally_free = set.locally_free.clone();
            s.remainder = set.remainder.clone();
            Ok(Expr::ESet(s))
        }
        Expr::EMap(map) => {
            let evaled: Vec<(Par, Par)> = map
                .kvs
                .iter()
                .map(|(k, v)| {
                    Ok((
                        update_locally_free(&eval_expr(k, env, cost)?),
                        update_locally_free(&eval_expr(v, env, cost)?),
                    ))
                })
                .collect::<Result<_, RholangError>>()?;
            let mut m = par_map(evaled);
            m.connective_used = map.connective_used;
            m.locally_free = map.locally_free.clone();
            m.remainder = map.remainder.clone();
            Ok(Expr::EMap(m))
        }
        Expr::EMethod(em) => {
            cost.charge(Costs::method_call_cost())?;
            let evaled_target = eval_expr(&em.target, env, cost)?;
            let evaled_args: Vec<Par> = em
                .arguments
                .iter()
                .map(|a| eval_expr(a, env, cost))
                .collect::<Result<_, _>>()?;
            let result_par = eval_method(&em.method_name, &evaled_target, &evaled_args, env, cost)?;
            eval_single_expr(&result_par, env, cost)
        }
    }
}

/// Evaluate the top-level expressions of a `Par` (port of `evalExpr`).
pub fn eval_expr<S: Sort + SortJoin<S>>(par: &Par<S>, env: &Env<Par>, cost: &CostAccounting) -> Result<Par<S>, RholangError> {
    let mut result = Par {
        exprs: Vec::new(),
        ..par.clone()
    };
    for e in &par.exprs {
        let evaled = eval_expr_to_par(e, env, cost)?;
        result = par_concat(&result, &evaled).re_sort();
    }
    Ok(result)
}

fn check_arity(method: &str, expected: usize, actual: usize) -> Result<(), RholangError> {
    if actual != expected {
        Err(RholangError::MethodArgumentNumberMismatch {
            method: method.to_string(),
            expected: expected as i32,
            actual: actual as i32,
        })
    } else {
        Ok(())
    }
}

fn hex_encode(bytes: &[u8]) -> String {
    bytes.iter().map(|b| format!("{b:02x}")).collect()
}

fn hex_decode(s: &str) -> Option<Vec<u8>> {
    if s.len() % 2 != 0 {
        return None;
    }
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).ok())
        .collect()
}

fn make_tuple(k: Par, v: Par) -> Par {
    from_expr(Expr::ETuple(ETuple {
        ps: vec![k, v],
        locally_free: AlwaysEqual(vec![]),
        connective_used: false,
    }))
}

fn unapply_tuple2(p: &Par) -> Option<(Par, Par)> {
    match single_expr(p) {
        Some(Expr::ETuple(ETuple { ps, .. })) if ps.len() == 2 => {
            Some((ps[0].clone(), ps[1].clone()))
        }
        _ => None,
    }
}

fn method_not_defined(method: &str, expr: &Expr) -> RholangError {
    RholangError::MethodNotDefined {
        method: method.to_string(),
        other_type: typ(expr).to_string(),
    }
}

fn eval_method(
    method: &str,
    target: &Par,
    args: &[Par],
    env: &Env<Par>,
    cost: &CostAccounting,
) -> Result<Par, RholangError> {
    match method {
        "nth" => {
            check_arity("nth", 1, args.len())?;
            cost.charge(Costs::nth_method_call_cost())?;
            let nth = restrict_to_int(eval_to_long(&args[0], env, cost)?)?;
            let v = eval_single_expr(target, env, cost)?;
            match v {
                Expr::EList(EList { ps, .. }) | Expr::ETuple(ETuple { ps, .. }) => {
                    ps.get(nth).cloned().ok_or_else(|| {
                        RholangError::ReduceError(format!("Error: index out of bound: {nth}"))
                    })
                }
                Expr::GByteArray(bs) => {
                    if nth < bs.len() {
                        Ok(from_expr(Expr::GInt(bs[nth] as i64)))
                    } else {
                        Err(RholangError::ReduceError(format!(
                            "Error: index out of bound: {nth}"
                        )))
                    }
                }
                other => Err(RholangError::ReduceError(format!(
                    "Error: nth applied to something that wasn't a list or tuple. ({})",
                    typ(&other)
                ))),
            }
        }
        "toInt" => {
            check_arity("toInt", 0, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            match base {
                Expr::GInt(v) => Ok(from_expr(Expr::GInt(v))),
                Expr::GBigInt(bi) => {
                    cost.charge(Costs::to_int_cost_bigint(&bi))?;
                    let v = bi.to_string().parse::<i64>().map_err(|_| {
                        RholangError::ReduceError(format!(
                            "Method toInt(): input BigInt value {bi} out of range"
                        ))
                    })?;
                    Ok(from_expr(Expr::GInt(v)))
                }
                Expr::GString(s) => {
                    cost.charge(Costs::to_int_cost_string(&s))?;
                    let v = s.parse::<i64>().map_err(|_| {
                        RholangError::ReduceError(format!(
                            "Method toInt(): input string \"{s}\" cannot be converted to Int"
                        ))
                    })?;
                    Ok(from_expr(Expr::GInt(v)))
                }
                other => Err(method_not_defined("toInt", &other)),
            }
        }
        "toBigInt" => {
            check_arity("toBigInt", 0, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            match base {
                Expr::GBigInt(v) => Ok(from_expr(Expr::GBigInt(v))),
                Expr::GInt(num) => {
                    cost.charge(Costs::int_to_bigint_cost())?;
                    Ok(from_expr(Expr::GBigInt(BigInt::from(num))))
                }
                Expr::GString(s) => {
                    cost.charge(Costs::to_bigint_cost(&s))?;
                    let v = s.parse::<BigInt>().map_err(|_| {
                        RholangError::ReduceError(format!(
                            "Method toBigInt(): input string \"{s}\" cannot be converted to BigInt"
                        ))
                    })?;
                    Ok(from_expr(Expr::GBigInt(v)))
                }
                other => Err(method_not_defined("toBigInt", &other)),
            }
        }
        "hexToBytes" => {
            check_arity("hexToBytes", 0, args.len())?;
            match eval_single_expr(target, env, cost)? {
                Expr::GString(s) => {
                    cost.charge(Costs::hex_to_bytes_cost(&s))?;
                    let bytes = hex_decode(&s).ok_or_else(|| {
                        RholangError::ReduceError(
                            "Error: exception was thrown when decoding input string to hexadecimal"
                                .to_string(),
                        )
                    })?;
                    Ok(from_expr(Expr::GByteArray(bytes)))
                }
                other => Err(method_not_defined("hexToBytes", &other)),
            }
        }
        "bytesToHex" => {
            check_arity("bytesToHex", 0, args.len())?;
            match eval_single_expr(target, env, cost)? {
                Expr::GByteArray(bytes) => {
                    cost.charge(Costs::bytes_to_hex_cost(&bytes))?;
                    Ok(from_expr(Expr::GString(hex_encode(&bytes))))
                }
                other => Err(method_not_defined("bytesToHex", &other)),
            }
        }
        "toUtf8Bytes" => {
            check_arity("toUtf8Bytes", 0, args.len())?;
            match eval_single_expr(target, env, cost)? {
                Expr::GString(s) => {
                    cost.charge(Costs::hex_to_bytes_cost(&s))?;
                    Ok(from_expr(Expr::GByteArray(s.into_bytes())))
                }
                other => Err(method_not_defined("toUtf8Bytes", &other)),
            }
        }
        "toByteArray" => {
            // Serialize the substituted `Par` to its protobuf wire form (port of the Scala
            // `toByteArray` = `Serialize[Par].encode`, where `Serialize[Par]` is
            // `mkProtobufInstance(Par)` — the protobuf serialization, not UTF-8).
            check_arity("toByteArray", 0, args.len())?;
            let substituted = substitute_par_and_charge(target, 0, env, cost)?;
            let bytes =
                <Par as rchain_shared::serialize::Serialize<Par>>::encode(&substituted);
            cost.charge(Costs::to_byte_array_cost(&substituted))?;
            Ok(from_expr(Expr::GByteArray(bytes)))
        }
        "union" => {
            check_arity("union", 1, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let other = eval_single_expr(&args[0], env, cost)?;
            match (&base, &other) {
                (Expr::ESet(b), Expr::ESet(o)) => {
                    cost.charge(Costs::union_cost(o.ps.len() as i64))?;
                    let mut ps = b.ps.clone();
                    ps.extend(o.ps.clone());
                    let mut s = par_set(ps);
                    s.connective_used = b.connective_used || o.connective_used;
                    s.locally_free =
                        AlwaysEqual(union_free(b.locally_free.0.clone(), o.locally_free.0.clone()));
                    s.remainder = None;
                    Ok(from_expr(Expr::ESet(s)))
                }
                (Expr::EMap(b), Expr::EMap(o)) => {
                    cost.charge(Costs::union_cost(o.kvs.len() as i64))?;
                    let mut kvs = b.kvs.clone();
                    kvs.extend(o.kvs.clone());
                    let mut m = par_map(kvs);
                    m.connective_used = b.connective_used || o.connective_used;
                    m.locally_free =
                        AlwaysEqual(union_free(b.locally_free.0.clone(), o.locally_free.0.clone()));
                    m.remainder = None;
                    Ok(from_expr(Expr::EMap(m)))
                }
                (o, _) => Err(method_not_defined("union", o)),
            }
        }
        "diff" => {
            check_arity("diff", 1, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let other = eval_single_expr(&args[0], env, cost)?;
            match (&base, &other) {
                (Expr::ESet(b), Expr::ESet(o)) => {
                    cost.charge(Costs::diff_cost(o.ps.len() as i64))?;
                    let ps: Vec<Par> = b.ps.iter().filter(|p| !o.ps.contains(p)).cloned().collect();
                    Ok(from_expr(Expr::ESet(par_set(ps))))
                }
                (Expr::EMap(b), Expr::EMap(o)) => {
                    cost.charge(Costs::diff_cost(o.kvs.len() as i64))?;
                    let kvs: Vec<(Par, Par)> = b
                        .kvs
                        .iter()
                        .filter(|(k, _)| !o.kvs.iter().any(|(ok, _)| ok == k))
                        .cloned()
                        .collect();
                    Ok(from_expr(Expr::EMap(par_map(kvs))))
                }
                (o, _) => Err(method_not_defined("diff", o)),
            }
        }
        "add" => {
            check_arity("add", 1, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let element = eval_expr(&args[0], env, cost)?;
            cost.charge(Costs::add_cost())?;
            match base {
                Expr::ESet(b) => {
                    let element_conn = element.connective_used;
                    let element_lf = element.locally_free.0.clone();
                    let mut ps = b.ps.clone();
                    ps.push(element);
                    let mut s = par_set(ps);
                    s.connective_used = b.connective_used || element_conn;
                    s.locally_free =
                        AlwaysEqual(union_free(b.locally_free.0.clone(), element_lf));
                    s.remainder = None;
                    Ok(from_expr(Expr::ESet(s)))
                }
                other => Err(method_not_defined("add", &other)),
            }
        }
        "delete" => {
            check_arity("delete", 1, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let element = eval_expr(&args[0], env, cost)?;
            match &base {
                Expr::ESet(b) => {
                    cost.charge(Costs::remove_cost().mul(b.ps.len() as i64))?;
                    let ps: Vec<Par> = b.ps.iter().filter(|p| *p != &element).cloned().collect();
                    Ok(from_expr(Expr::ESet(par_set(ps))))
                }
                Expr::EMap(b) => {
                    cost.charge(Costs::remove_cost().mul(b.kvs.len() as i64))?;
                    let kvs: Vec<(Par, Par)> = b
                        .kvs
                        .iter()
                        .filter(|(k, _)| k != &element)
                        .cloned()
                        .collect();
                    Ok(from_expr(Expr::EMap(par_map(kvs))))
                }
                other => Err(method_not_defined("delete", other)),
            }
        }
        "contains" => {
            check_arity("contains", 1, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let element = eval_expr(&args[0], env, cost)?;
            match &base {
                Expr::ESet(b) => {
                    cost.charge(Costs::lookup_cost().mul(b.ps.len() as i64))?;
                    Ok(from_expr(Expr::GBool(b.ps.contains(&element))))
                }
                Expr::EMap(b) => {
                    cost.charge(Costs::lookup_cost().mul(b.kvs.len() as i64))?;
                    Ok(from_expr(Expr::GBool(b.kvs.iter().any(|(k, _)| k == &element))))
                }
                other => Err(method_not_defined("contains", other)),
            }
        }
        "get" => {
            check_arity("get", 1, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let key = eval_expr(&args[0], env, cost)?;
            match &base {
                Expr::EMap(b) => {
                    cost.charge(Costs::lookup_cost().mul(b.kvs.len() as i64))?;
                    Ok(b
                        .kvs
                        .iter()
                        .find(|(k, _)| k == &key)
                        .map(|(_, v)| v.clone())
                        .unwrap_or_default())
                }
                other => Err(method_not_defined("get", other)),
            }
        }
        "getOrElse" => {
            check_arity("getOrElse", 2, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let key = eval_expr(&args[0], env, cost)?;
            let default = eval_expr(&args[1], env, cost)?;
            match &base {
                Expr::EMap(b) => {
                    cost.charge(Costs::lookup_cost().mul(b.kvs.len() as i64))?;
                    Ok(b
                        .kvs
                        .iter()
                        .find(|(k, _)| k == &key)
                        .map(|(_, v)| v.clone())
                        .unwrap_or(default))
                }
                other => Err(method_not_defined("getOrElse", other)),
            }
        }
        "set" => {
            check_arity("set", 2, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let key = eval_expr(&args[0], env, cost)?;
            let value = eval_expr(&args[1], env, cost)?;
            cost.charge(Costs::add_cost())?;
            match base {
                Expr::EMap(b) => {
                    let mut kvs = b.kvs.clone();
                    if let Some(slot) = kvs.iter_mut().find(|(k, _)| k == &key) {
                        slot.1 = value;
                    } else {
                        kvs.push((key, value));
                    }
                    Ok(from_expr(Expr::EMap(par_map(kvs))))
                }
                other => Err(method_not_defined("set", &other)),
            }
        }
        "keys" => {
            check_arity("keys", 0, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            cost.charge(Costs::keys_method_cost())?;
            match base {
                Expr::EMap(b) => {
                    let keys: Vec<Par> = b.kvs.iter().map(|(k, _)| k.clone()).collect();
                    Ok(from_expr(Expr::ESet(par_set(keys))))
                }
                other => Err(method_not_defined("keys", &other)),
            }
        }
        "size" => {
            check_arity("size", 0, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let size = match &base {
                Expr::EMap(b) => b.kvs.len(),
                Expr::ESet(b) => b.ps.len(),
                other => return Err(method_not_defined("size", other)),
            };
            cost.charge(Costs::size_method_cost(size as i64))?;
            Ok(from_expr(Expr::GInt(size as i64)))
        }
        "length" => {
            check_arity("length", 0, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            cost.charge(Costs::length_method_cost())?;
            let n = match &base {
                Expr::GString(s) => s.len(),
                Expr::GByteArray(b) => b.len(),
                Expr::EList(EList { ps, .. }) => ps.len(),
                other => return Err(method_not_defined("length", other)),
            };
            Ok(from_expr(Expr::GInt(n as i64)))
        }
        "slice" => {
            check_arity("slice", 2, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let from_i = eval_to_long(&args[0], env, cost)?;
            let until_i = eval_to_long(&args[1], env, cost)?;
            // Scala `slice` clamps `from` to [0, len) and returns empty when `from >= until`.
            // `restrict_to_int` wraps a negative index to a huge `usize`, which would make
            // `until - from` underflow — so clamp in `i64` first.
            let from = from_i.max(0);
            let until = until_i.max(0);
            let len = if until > from { (until - from) as usize } else { 0 };
            cost.charge(Costs::slice_cost(len as i64))?;
            let from = from as usize;
            match base {
                Expr::GString(s) => Ok(from_expr(Expr::GString(
                    s.chars().skip(from).take(len).collect(),
                ))),
                Expr::GByteArray(b) => Ok(from_expr(Expr::GByteArray(
                    b.into_iter().skip(from).take(len).collect(),
                ))),
                Expr::EList(EList { ps, locally_free, connective_used, remainder }) => {
                    Ok(from_expr(Expr::EList(EList {
                        ps: ps.into_iter().skip(from).take(len).collect(),
                        locally_free,
                        connective_used,
                        remainder,
                    })))
                }
                other => Err(method_not_defined("slice", &other)),
            }
        }
        "take" => {
            check_arity("take", 1, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            let n_i = eval_to_long(&args[0], env, cost)?;
            // Scala `List.take(n)` returns empty for `n <= 0`. `restrict_to_int` wraps a negative
            // index to a huge `usize`, which would return the whole list (and mint a negative cost).
            let n = if n_i <= 0 { 0 } else { n_i as usize };
            cost.charge(Costs::take_cost(n as i64))?;
            match base {
                Expr::EList(EList { ps, locally_free, connective_used, remainder }) => {
                    Ok(from_expr(Expr::EList(EList {
                        ps: ps.into_iter().take(n).collect(),
                        locally_free,
                        connective_used,
                        remainder,
                    })))
                }
                other => Err(method_not_defined("take", &other)),
            }
        }
        "toList" => {
            check_arity("toList", 0, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            match base {
                Expr::EList(_) => Ok(target.clone()),
                Expr::ETuple(_) => Ok(target.clone()),
                Expr::ESet(b) => {
                    cost.charge(Costs::to_list_cost(b.ps.len() as i64))?;
                    Ok(from_expr(Expr::EList(EList {
                        ps: b.ps,
                        locally_free: AlwaysEqual(vec![]),
                        connective_used: false,
                        ..Default::default()
                    })))
                }
                Expr::EMap(b) => {
                    cost.charge(Costs::to_list_cost(b.kvs.len() as i64))?;
                    let ps: Vec<Par> = b
                        .kvs
                        .iter()
                        .map(|(k, v)| make_tuple(k.clone(), v.clone()))
                        .collect();
                    Ok(from_expr(Expr::EList(EList {
                        ps,
                        locally_free: AlwaysEqual(vec![]),
                        connective_used: false,
                        ..Default::default()
                    })))
                }
                other => Err(method_not_defined("toList", &other)),
            }
        }
        "toSet" => {
            check_arity("toSet", 0, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            match base {
                Expr::ESet(_) => Ok(target.clone()),
                Expr::EMap(b) => {
                    let ps: Vec<Par> = b
                        .kvs
                        .iter()
                        .map(|(k, v)| make_tuple(k.clone(), v.clone()))
                        .collect();
                    Ok(from_expr(Expr::ESet(par_set(ps))))
                }
                Expr::EList(EList { ps, connective_used, remainder, .. }) => {
                    Ok(from_expr(Expr::ESet(ParSet {
                        ps: par_set(ps).ps,
                        connective_used,
                        locally_free: AlwaysEqual(vec![]),
                        remainder,
                    })))
                }
                other => Err(method_not_defined("toSet", &other)),
            }
        }
        "toMap" => {
            check_arity("toMap", 0, args.len())?;
            let base = eval_single_expr(target, env, cost)?;
            match base {
                Expr::EMap(_) => Ok(target.clone()),
                Expr::ESet(b) => {
                    let mut kvs = Vec::new();
                    for p in b.ps {
                        match unapply_tuple2(&p) {
                            Some(kv) => kvs.push(kv),
                            None => {
                                return Err(method_not_defined(
                                    "toMap",
                                    &Expr::ESet(ParSet::default()),
                                ))
                            }
                        }
                    }
                    Ok(from_expr(Expr::EMap(par_map(kvs))))
                }
                Expr::EList(EList { ps, .. }) => {
                    let mut kvs = Vec::new();
                    for p in ps {
                        match unapply_tuple2(&p) {
                            Some(kv) => kvs.push(kv),
                            None => {
                                return Err(method_not_defined(
                                    "toMap",
                                    &Expr::EList(EList::default()),
                                ))
                            }
                        }
                    }
                    Ok(from_expr(Expr::EMap(par_map(kvs))))
                }
                other => Err(method_not_defined("toMap", &other)),
            }
        }
        _ => Err(RholangError::ReduceError(format!(
            "Unimplemented method: {method}"
        ))),
    }
}

/// The result of a tuplespace produce/consume: the matched continuation, the list of
/// (channel, matched data, removed data, persistent), and whether it was a peek.
pub type Application =
    Option<(TaggedContinuation, Vec<(SortedProc, ListParWithRandom, ListParWithRandom, bool)>, bool)>;

/// The tuplespace interface the evaluator produces/consumes against (port of `RhoTuplespace`).
#[async_trait]
pub trait Tuplespace: std::marker::Send + std::marker::Sync {
    async fn produce(
        &self,
        channel: &SortedProc,
        data: ListParWithRandom,
        persist: bool,
    ) -> Result<Application, RholangError>;

    async fn consume(
        &self,
        channels: &[SortedProc],
        patterns: &[BindPattern],
        continuation: TaggedContinuation,
        persist: bool,
        peeks: BTreeSet<usize>,
    ) -> Result<Application, RholangError>;
}

/// Dispatches a continuation with its matched data (port of `Dispatch`).
#[async_trait]
pub trait Dispatch: std::marker::Send + std::marker::Sync {
    async fn dispatch(
        &self,
        continuation: TaggedContinuation,
        data_list: Vec<ListParWithRandom>,
    ) -> Result<(), RholangError>;
}

enum Term<'a> {
    Send(&'a Send),
    Receive(&'a Receive),
    New(&'a New),
    Match(&'a Match),
    Bundle(&'a Bundle),
    Expr(&'a Expr),
}

/// The reducer (port of `DebruijnInterpreter`).
pub struct DebruijnInterpreter<T: Tuplespace, D: Dispatch> {
    space: T,
    dispatcher: D,
    urn_map: BTreeMap<String, Par>,
    merge_chs: Mutex<Vec<SortedProc>>,
    mergeable_tag_name: SortedProc,
}

impl<T: Tuplespace, D: Dispatch> DebruijnInterpreter<T, D> {
    pub fn new(
        space: T,
        dispatcher: D,
        urn_map: BTreeMap<String, Par>,
        mergeable_tag_name: SortedProc,
    ) -> Self {
        DebruijnInterpreter {
            space,
            dispatcher,
            urn_map,
            merge_chs: Mutex::new(Vec::new()),
            mergeable_tag_name,
        }
    }

    /// Evaluate a top-level `Par` (port of `Reduce.eval(par)`).
    pub async fn eval(
        &self,
        par: &Par,
        env: &Env<Par>,
        rand: &Blake2b512Random,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        let mut terms: Vec<Term> = Vec::new();
        for s in &par.sends {
            terms.push(Term::Send(s));
        }
        for r in &par.receives {
            terms.push(Term::Receive(r));
        }
        for n in &par.news {
            terms.push(Term::New(n));
        }
        for m in &par.matches {
            terms.push(Term::Match(m));
        }
        for b in &par.bundles {
            terms.push(Term::Bundle(b));
        }
        for e in &par.exprs {
            if matches!(e, Expr::EVar(_) | Expr::EMethod(_)) {
                terms.push(Term::Expr(e));
            }
        }
        if terms.len() > i16::MAX as usize {
            return Err(RholangError::ReduceError(format!(
                "The number of terms in the Par is {}, which exceeds the limit of {}.",
                terms.len(),
                i16::MAX
            )));
        }
        for (i, term) in terms.iter().enumerate() {
            let r = if terms.len() == 1 {
                rand.clone()
            } else if terms.len() > 256 {
                rand.split_short(u16::try_from(i).map_err(|e| RholangError::ReduceError(e.to_string()))?)
            } else {
                rand.split_byte(u8::try_from(i).map_err(|e| RholangError::ReduceError(e.to_string()))?)
            };
            Box::pin(self.eval_term(term, env, &r, cost)).await?;
        }
        Ok(())
    }

    async fn eval_term<'a>(
        &self,
        term: &Term<'a>,
        env: &Env<Par>,
        rand: &Blake2b512Random,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        match term {
            Term::Send(s) => self.eval_send(s, env, rand, cost).await,
            Term::Receive(r) => self.eval_receive(r, env, rand, cost).await,
            Term::New(n) => self.eval_new(n, env, rand, cost).await,
            Term::Match(m) => self.eval_match(m, env, rand, cost).await,
            Term::Bundle(b) => self.eval_bundle(b, env, rand, cost).await,
            Term::Expr(e) => match e {
                Expr::EVar(v) => {
                    let p = eval_var(v, env, cost)?;
                    self.eval(&p, env, rand, cost).await
                }
                Expr::EMethod(_) => {
                    let p = eval_expr_to_par(e, env, cost)?;
                    self.eval(&p, env, rand, cost).await
                }
                _ => Err(RholangError::BugFoundError(format!(
                    "Undefined term: {e:?}"
                ))),
            },
        }
    }

    async fn eval_send(
        &self,
        send: &Send,
        env: &Env<Par>,
        rand: &Blake2b512Random,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        cost.charge(Costs::send_eval_cost())?;
        let eval_chan = eval_expr(send.chan.as_ref(), env, cost)?;
        let sub_chan = substitute_par_and_charge(&eval_chan, 0, env, cost)?;
        let unbundled = match single_bundle(&sub_chan) {
            Some(value) => {
                if !value.write_flag {
                    return Err(RholangError::ReduceError(
                        "Trying to send on non-writeable channel.".to_string(),
                    ));
                }
                (*value.body).clone()
            }
            None => sub_chan.eval(),
        };
        let data: Vec<Name> = send
            .data
            .iter()
            .map(|d| eval_expr(d, env, cost))
            .collect::<Result<_, _>>()?;
        let subst_data: Vec<Name> = data
            .iter()
            .map(|d| substitute_par_and_charge(d, 0, env, cost))
            .collect::<Result<_, _>>()?;
        self.produce(
            &SortedProc::new(unbundled),
            ListParWithRandom {
                pars: subst_data.into_iter().map(|d| SortedProc::new(d.eval())).collect(),
                random_state: rand.clone(),
            },
            send.persistent,
            cost,
        )
        .await
    }

    async fn eval_receive(
        &self,
        receive: &Receive,
        env: &Env<Par>,
        rand: &Blake2b512Random,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        cost.charge(Costs::receive_eval_cost())?;
        let mut binds: Vec<(BindPattern, SortedProc)> = Vec::new();
        for rb in &receive.binds {
            let q = self.unbundle_receive(rb, env, cost)?;
            let subst_patterns: Vec<Name> = rb
                .patterns
                .iter()
                .map(|p| substitute_par_and_charge(p, 1, env, cost))
                .collect::<Result<_, _>>()?;
            binds.push((
                BindPattern {
                    patterns: subst_patterns.into_iter().map(|p| SortedProc::new(p.eval())).collect(),
                    remainder: rb.remainder.as_deref().cloned(),
                    free_count: i32::from(rb.free_count),
                },
                SortedProc::new(q),
            ));
        }
        let subst_body = substitute_par(&receive.body, 0, &env.shift(receive.bind_count))?;
        self.consume(
            &binds,
            ParWithRandom {
                body: SortedProc::new(subst_body),
                random_state: rand.clone(),
            },
            receive.persistent,
            receive.peek,
            cost,
        )
        .await
    }

    fn unbundle_receive(
        &self,
        rb: &ReceiveBind,
        env: &Env<Par>,
        cost: &CostAccounting,
    ) -> Result<Par, RholangError> {
        let eval_src = eval_expr(rb.source.as_ref(), env, cost)?;
        let subst = substitute_par_and_charge(&eval_src, 0, env, cost)?;
        match single_bundle(&subst) {
            Some(value) => {
                if !value.read_flag {
                    Err(RholangError::ReduceError(
                        "Trying to read from non-readable channel.".to_string(),
                    ))
                } else {
                    Ok((*value.body).clone())
                }
            }
            None => Ok(subst.eval()),
        }
    }

    async fn eval_new(
        &self,
        new: &New,
        env: &Env<Par>,
        rand: &Blake2b512Random,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        cost.charge(Costs::new_bindings_cost(new.bind_count as i64))?;
        let mut r = rand.clone();
        let new_env = self.alloc(new.bind_count, &new.uri, &new.injections, env, &mut r)?;
        self.eval(&new.p, &new_env, rand, cost).await
    }

    fn alloc(
        &self,
        count: i32,
        urns: &[String],
        injections: &BTreeMap<String, Par>,
        env: &Env<Par>,
        rand: &mut Blake2b512Random,
    ) -> Result<Env<Par>, RholangError> {
        let mut new_env = env.clone();
        for _ in 0..(count - urns.len() as i32) {
            let bytes = rand.next();
            let addr = Par {
                unforgeables: vec![GUnforgeable::GPrivate(GPrivate { id: bytes })],
                ..Default::default()
            };
            new_env = new_env.put(addr);
        }
        for urn in urns {
            new_env = self.add_urn(new_env, urn, injections)?;
        }
        Ok(new_env)
    }

    fn add_urn(
        &self,
        env: Env<Par>,
        urn: &str,
        injections: &BTreeMap<String, Par>,
    ) -> Result<Env<Par>, RholangError> {
        if let Some(p) = self.urn_map.get(urn) {
            Ok(env.put(p.clone()))
        } else if let Some(p) = injections.get(urn) {
            Ok(env.put(p.clone()))
        } else {
            Err(RholangError::BugFoundError(format!(
                "No value set for `{urn}`. This is a bug in the normalizer or on the path from it."
            )))
        }
    }

    async fn eval_match(
        &self,
        m: &Match,
        env: &Env<Par>,
        rand: &Blake2b512Random,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        cost.charge(Costs::match_eval_cost())?;
        let evaled_target = eval_expr(m.target.as_ref(), env, cost)?;
        let subst_target = substitute_par_and_charge(&evaled_target, 0, env, cost)?;
        self.first_match(&subst_target.eval(), &m.cases, env, rand, cost)
            .await
    }

    async fn first_match(
        &self,
        target: &Par,
        cases: &[MatchCase],
        env: &Env<Par>,
        rand: &Blake2b512Random,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        for case in cases {
            let pattern = substitute_par_and_charge(&case.pattern, 1, env, cost)?;
            if let Some(free_map) = spatial_match_result(target, &pattern.eval())? {
                let mut new_env = env.clone();
                for e in 0..i32::from(case.free_count) {
                    new_env = new_env.put(free_map.get(&e).cloned().unwrap_or_default());
                }
                return self.eval(&case.source, &new_env, rand, cost).await;
            }
        }
        Ok(())
    }

    async fn eval_bundle(
        &self,
        bundle: &Bundle,
        env: &Env<Par>,
        rand: &Blake2b512Random,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        self.eval(&bundle.body, env, rand, cost).await
    }

    async fn produce(
        &self,
        chan: &SortedProc,
        data: ListParWithRandom,
        persistent: bool,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        self.update_mergeable_channels(chan);
        let result = self.space.produce(chan, data.clone(), persistent).await?;
        match result {
            Some((continuation, data_list, peek)) => {
                self.dispatch(&continuation, &data_list).await?;
                if persistent {
                    Box::pin(self.produce(chan, data, persistent, cost)).await?;
                } else if peek {
                    self.produce_peeks(&data_list, cost).await?;
                }
            }
            None => {}
        }
        Ok(())
    }

    async fn consume(
        &self,
        binds: &[(BindPattern, SortedProc)],
        body: ParWithRandom,
        persistent: bool,
        peek: bool,
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        let patterns: Vec<BindPattern> = binds.iter().map(|(p, _)| p.clone()).collect();
        let sources: Vec<SortedProc> = binds.iter().map(|(_, s)| s.clone()).collect();
        for s in &sources {
            self.update_mergeable_channels(s);
        }
        let peeks: BTreeSet<usize> = if peek {
            (0..sources.len()).collect()
        } else {
            BTreeSet::new()
        };
        let result = self
            .space
            .consume(
                &sources,
                &patterns,
                TaggedContinuation::ParBody(body.clone()),
                persistent,
                peeks.clone(),
            )
            .await?;
        match result {
            Some((continuation, data_list, p)) => {
                self.dispatch(&continuation, &data_list).await?;
                if persistent {
                    Box::pin(self.consume(binds, body, persistent, peek, cost)).await?;
                } else if p {
                    self.produce_peeks(&data_list, cost).await?;
                }
            }
            None => {}
        }
        Ok(())
    }

    async fn produce_peeks(
        &self,
        data_list: &[(SortedProc, ListParWithRandom, ListParWithRandom, bool)],
        cost: &CostAccounting,
    ) -> Result<(), RholangError> {
        for (chan, _, removed_data, persist) in data_list {
            if !persist {
                Box::pin(self.produce(chan, removed_data.clone(), false, cost)).await?;
            }
        }
        Ok(())
    }

    async fn dispatch(
        &self,
        continuation: &TaggedContinuation,
        data_list: &[(SortedProc, ListParWithRandom, ListParWithRandom, bool)],
    ) -> Result<(), RholangError> {
        let data: Vec<ListParWithRandom> = data_list.iter().map(|(_, d, _, _)| d.clone()).collect();
        self.dispatcher.dispatch(continuation.clone(), data).await
    }

    fn update_mergeable_channels(&self, chan: &SortedProc) {
        if self.is_mergeable_channel(chan) {
            let mut chs = self.merge_chs.lock().unwrap_or_else(|p| p.into_inner());
            if !chs.contains(chan) {
                chs.push(chan.clone());
            }
        }
    }

    fn is_mergeable_channel(&self, chan: &SortedProc) -> bool {
        chan.as_par()
            .exprs
            .iter()
            .find_map(|e| match e {
                Expr::ETuple(ETuple { ps, .. }) => ps.first(),
                _ => None,
            })
            .map_or(false, |head| head == self.mergeable_tag_name.as_par())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn evaluates_arithmetic() {
        let cost = CostAccounting::from_initial(Costs::unsafe_max());
        let e = Env::new();
        let p = from_expr(Expr::EPlus(
            Box::new(from_expr(Expr::GInt(2))),
            Box::new(from_expr(Expr::GInt(3))),
        ));
        assert_eq!(
            eval_single_expr(&p, &e, &cost).unwrap(),
            Expr::GInt(5)
        );
    }

    #[test]
    fn division_and_modulo_by_zero_are_errors() {
        let cost = CostAccounting::from_initial(Costs::unsafe_max());
        let e = Env::new();

        let div_zero = from_expr(Expr::EDiv(
            Box::new(from_expr(Expr::GInt(1))),
            Box::new(from_expr(Expr::GInt(0))),
        ));
        assert!(eval_single_expr(&div_zero, &e, &cost).is_err());

        let mod_zero = from_expr(Expr::EMod(
            Box::new(from_expr(Expr::GInt(1))),
            Box::new(from_expr(Expr::GInt(0))),
        ));
        assert!(eval_single_expr(&mod_zero, &e, &cost).is_err());

        let big_div_zero = from_expr(Expr::EDiv(
            Box::new(from_expr(Expr::GBigInt(BigInt::from(1i64)))),
            Box::new(from_expr(Expr::GBigInt(BigInt::from(0i64)))),
        ));
        assert!(eval_single_expr(&big_div_zero, &e, &cost).is_err());

        let big_mod_zero = from_expr(Expr::EMod(
            Box::new(from_expr(Expr::GBigInt(BigInt::from(1i64)))),
            Box::new(from_expr(Expr::GBigInt(BigInt::from(0i64)))),
        ));
        assert!(eval_single_expr(&big_mod_zero, &e, &cost).is_err());
    }

    #[test]
    fn evaluates_boolean_and() {
        let cost = CostAccounting::from_initial(Costs::unsafe_max());
        let e = Env::new();
        let p = from_expr(Expr::EAnd(
            Box::new(from_expr(Expr::GBool(true))),
            Box::new(from_expr(Expr::GBool(false))),
        ));
        assert_eq!(eval_single_expr(&p, &e, &cost).unwrap(), Expr::GBool(false));
    }

    #[test]
    fn string_append() {
        let cost = CostAccounting::from_initial(Costs::unsafe_max());
        let e = Env::new();
        let p = from_expr(Expr::EPlusPlus(
            Box::new(from_expr(Expr::GString("a".to_string()))),
            Box::new(from_expr(Expr::GString("b".to_string()))),
        ));
        assert_eq!(
            eval_single_expr(&p, &e, &cost).unwrap(),
            Expr::GString("ab".to_string())
        );
    }

    struct MockSpace {
        produced: Mutex<Vec<(SortedProc, ListParWithRandom, bool)>>,
    }
    #[async_trait]
    impl Tuplespace for MockSpace {
        async fn produce(
            &self,
            channel: &SortedProc,
            data: ListParWithRandom,
            persist: bool,
        ) -> Result<Application, RholangError> {
            self.produced
                .lock()
                .unwrap()
                .push((channel.clone(), data, persist));
            Ok(None)
        }
        async fn consume(
            &self,
            _channels: &[SortedProc],
            _patterns: &[BindPattern],
            _continuation: TaggedContinuation,
            _persist: bool,
            _peeks: BTreeSet<usize>,
        ) -> Result<Application, RholangError> {
            Ok(None)
        }
    }
    struct MockDispatch;
    #[async_trait]
    impl Dispatch for MockDispatch {
        async fn dispatch(
            &self,
            _continuation: TaggedContinuation,
            _data_list: Vec<ListParWithRandom>,
        ) -> Result<(), RholangError> {
            Ok(())
        }
    }

    #[tokio::test]
    async fn eval_send_produces_on_evaluated_channel() {
        let space = MockSpace {
            produced: Mutex::new(Vec::new()),
        };
        let interp = DebruijnInterpreter::new(
            space,
            MockDispatch,
            BTreeMap::new(),
            SortedProc::default(),
        );
        let cost = CostAccounting::from_initial(Costs::unsafe_max());
        let env = Env::new();
        let rand = Blake2b512Random::new_random(128);

        let send = Send {
            chan: Box::new(from_expr(Expr::GInt(1)).quote()),
            data: vec![from_expr(Expr::GInt(2)).quote()],
            persistent: false,
            locally_free: AlwaysEqual(vec![]),
            connective_used: false,
        };
        let par = Par {
            sends: vec![send],
            ..Default::default()
        };
        interp.eval(&par, &env, &rand, &cost).await.unwrap();

        let produced = interp.space.produced.lock().unwrap_or_else(|p| p.into_inner());
        assert_eq!(produced.len(), 1);
        assert_eq!(produced[0].0.as_par().exprs, vec![Expr::GInt(1)]);
        assert_eq!(produced[0].1.pars, vec![SortedProc::new(from_expr(Expr::GInt(2)))]);
        assert!(!produced[0].2);
    }

    #[test]
    fn byte_methods_round_trip() {
        let cost = CostAccounting::from_initial(Costs::unsafe_max());
        let e = Env::new();
        let target = from_expr(Expr::GString("ab".to_string()));
        let bytes = eval_method("toUtf8Bytes", &target, &[], &e, &cost).unwrap();
        let hex = eval_method("bytesToHex", &bytes, &[], &e, &cost).unwrap();
        assert_eq!(hex, from_expr(Expr::GString("6162".to_string())));
    }

    #[test]
    fn set_union() {
        let cost = CostAccounting::from_initial(Costs::unsafe_max());
        let e = Env::new();
        let s1 = from_expr(Expr::ESet(par_set(vec![
            from_expr(Expr::GInt(1)),
            from_expr(Expr::GInt(2)),
        ])));
        let s2 = from_expr(Expr::ESet(par_set(vec![
            from_expr(Expr::GInt(2)),
            from_expr(Expr::GInt(3)),
        ])));
        let result = eval_method("union", &s1, &[s2], &e, &cost).unwrap();
        match single_expr(&result).unwrap() {
            Expr::ESet(set) => assert_eq!(set.ps.len(), 3),
            _ => panic!("expected a set"),
        }
    }

    #[test]
    fn set_difference_operator() {
        let cost = CostAccounting::from_initial(Costs::unsafe_max());
        let e = Env::new();
        let s1 = from_expr(Expr::ESet(par_set(vec![
            from_expr(Expr::GInt(1)),
            from_expr(Expr::GInt(2)),
        ])));
        let s2 = from_expr(Expr::ESet(par_set(vec![
            from_expr(Expr::GInt(2)),
            from_expr(Expr::GInt(3)),
        ])));
        let diff = eval_expr_to_expr(&Expr::EMinusMinus(Box::new(s1), Box::new(s2)), &e, &cost).unwrap();
        match diff {
            Expr::ESet(set) => {
                assert_eq!(set.ps.len(), 1);
                assert_eq!(set.ps[0], from_expr(Expr::GInt(1)));
            }
            _ => panic!("expected a set"),
        }
    }
}
