//! Capture-avoiding de Bruijn substitution (Law 3).
//!
//! Mirrors `rholang/src/main/scala/coop/rchain/rholang/interpreter/Substitute.scala`. The cats
//! `Substitute[M, A]` typeclass collapses to concrete `fn`s returning `Result<_, RholangError>`;
//! the `substituteAndCharge` wrapper is deferred (the `Chargeable` proto-size instances need wire
//! serialization).

use rchain_models::ast::{
    AlwaysEqual, Bundle, Connective, ConnectiveBody, EMethod, EList, ETuple, Expr, Match,
    MatchCase, Name, New, Par, ParMap, ParSet, Receive, ReceiveBind, Send, Sort, Var, VarRef,
};
use rchain_models::par_ops::{par_concat, prepend_connective, prepend_expr, single_bundle, until_free};
use rchain_models::sorter::{sort_par_term, sort_pairs, sort_pars};

use crate::accounting::{Chargeable, Cost, CostAccounting};
use crate::env::Env;
use crate::errors::RholangError;

/// The result of a variable lookup: either substituted to a `Par`, or kept as the original term.
type MaybeSubst<A> = std::result::Result<Par, A>;

fn maybe_substitute_var(
    var: &Var,
    depth: i32,
    env: &Env<Par>,
) -> Result<MaybeSubst<Var>, RholangError> {
    if depth != 0 {
        Ok(Err(var.clone()))
    } else {
        match var {
            Var::BoundVar(index) => match env.get(*index) {
                Some(par) => Ok(Ok(par)),
                None => Ok(Err(var.clone())),
            },
            _ => Err(RholangError::SubstituteError { term: var.clone() }),
        }
    }
}

fn maybe_substitute_varref(var_ref: &VarRef, depth: i32, env: &Env<Par>) -> MaybeSubst<VarRef> {
    if var_ref.depth != depth {
        Err(var_ref.clone())
    } else {
        env.get(var_ref.index)
            .map_or_else(|| Err(var_ref.clone()), Ok)
    }
}

fn sub_exprs(exprs: &[Expr], depth: i32, env: &Env<Par>) -> Result<Name, RholangError> {
    let mut par = Name::default();
    for expr in exprs.iter().rev() {
        match expr {
            Expr::EVar(v) => match maybe_substitute_var(v, depth, env)? {
                Ok(sub_par) => par = par_concat(&sub_par.quote(), &par),
                Err(evar) => par = prepend_expr(&par, Expr::EVar(Box::new(evar)), depth),
            },
            _ => {
                let sub_expr = substitute_expr_no_sort(expr, depth, env)?;
                par = prepend_expr(&par, sub_expr, depth);
            }
        }
    }
    Ok(par)
}

fn sub_conns(conns: &[Connective], depth: i32, env: &Env<Par>) -> Result<Name, RholangError> {
    let mut par = Name::default();
    for conn in conns.iter().rev() {
        match conn {
            Connective::VarRef(var_ref) => match maybe_substitute_varref(var_ref, depth, env) {
                Ok(sub_par) => par = par_concat(&sub_par.quote(), &par),
                Err(vr) => par = prepend_connective(&par, Connective::VarRef(vr), depth),
            },
            Connective::Empty => {}
            Connective::ConnAnd(ConnectiveBody { ps }) => {
                let sub_ps = ps
                    .iter()
                    .map(|p| substitute_par_no_sort(p, depth, env))
                    .collect::<Result<Vec<_>, _>>()?;
                par = prepend_connective(
                    &par,
                    Connective::ConnAnd(ConnectiveBody { ps: sub_ps }),
                    depth,
                );
            }
            Connective::ConnOr(ConnectiveBody { ps }) => {
                let sub_ps = ps
                    .iter()
                    .map(|p| substitute_par_no_sort(p, depth, env))
                    .collect::<Result<Vec<_>, _>>()?;
                par = prepend_connective(
                    &par,
                    Connective::ConnOr(ConnectiveBody { ps: sub_ps }),
                    depth,
                );
            }
            Connective::ConnNot(p) => {
                let sub_p = substitute_par_no_sort(p, depth, env)?;
                par = prepend_connective(&par, Connective::ConnNot(Box::new(sub_p)), depth);
            }
            Connective::ConnBool(_)
            | Connective::ConnInt(_)
            | Connective::ConnBigInt(_)
            | Connective::ConnString(_)
            | Connective::ConnUri(_)
            | Connective::ConnByteArray(_) => {
                par = prepend_connective(&par, conn.clone(), depth);
            }
        }
    }
    Ok(par)
}

pub fn substitute_par_no_sort<S: Sort>(
    par: &Par<S>,
    depth: i32,
    env: &Env<Par>,
) -> Result<Par<S>, RholangError> {
    let exprs_par = sub_exprs(&par.exprs, depth, env)?;
    let connectives_par = sub_conns(&par.connectives, depth, env)?;
    let sends = par
        .sends
        .iter()
        .map(|s| substitute_send_no_sort(s, depth, env))
        .collect::<Result<Vec<_>, _>>()?;
    let bundles = par
        .bundles
        .iter()
        .map(|b| substitute_bundle_no_sort(b, depth, env))
        .collect::<Result<Vec<_>, _>>()?;
    let receives = par
        .receives
        .iter()
        .map(|r| substitute_receive_no_sort(r, depth, env))
        .collect::<Result<Vec<_>, _>>()?;
    let news = par
        .news
        .iter()
        .map(|n| substitute_new_no_sort(n, depth, env))
        .collect::<Result<Vec<_>, _>>()?;
    let matches = par
        .matches
        .iter()
        .map(|m| substitute_match_no_sort(m, depth, env))
        .collect::<Result<Vec<_>, _>>()?;

    let rest: Par<S> = Par {
        sends,
        bundles,
        receives,
        news,
        matches,
        unforgeables: par.unforgeables.clone(),
        locally_free: AlwaysEqual(until_free(&par.locally_free.0, env.shift_amount())),
        connective_used: par.connective_used,
        ..Default::default()
    };
    let t1 = par_concat(&exprs_par, &connectives_par);
    Ok(par_concat(&t1, &rest))
}

pub fn substitute_par<S: Sort>(par: &Par<S>, depth: i32, env: &Env<Par>) -> Result<Par<S>, RholangError> {
    Ok(sort_par_term(&substitute_par_no_sort(par, depth, env)?))
}

/// `substituteAndCharge` (Law 3 + gas): charge the substitution by the substituted term's wire size
/// (or, on failure, the original term's size).
pub fn substitute_par_and_charge<S: Sort>(
    par: &Par<S>,
    depth: i32,
    env: &Env<Par>,
    cost: &CostAccounting,
) -> Result<Par<S>, RholangError> {
    let failure_cost = Cost::new(<Par<S> as Chargeable<Par<S>>>::cost(par), "substitution");
    match substitute_par(par, depth, env) {
        Ok(subst) => {
            cost.charge(Cost::new(<Par<S> as Chargeable<Par<S>>>::cost(&subst), "substitution"))?;
            Ok(subst)
        }
        Err(e) => {
            cost.charge(failure_cost)?;
            Err(e)
        }
    }
}

pub fn substitute_send_no_sort(
    send: &Send,
    depth: i32,
    env: &Env<Par>,
) -> Result<Send, RholangError> {
    let channels_sub = substitute_par_no_sort(&send.chan, depth, env)?;
    let pars_sub = send
        .data
        .iter()
        .map(|p| substitute_par_no_sort(p, depth, env))
        .collect::<Result<Vec<_>, _>>()?;
    Ok(Send {
        chan: Box::new(channels_sub),
        data: pars_sub,
        persistent: send.persistent,
        locally_free: AlwaysEqual(until_free(&send.locally_free.0, env.shift_amount())),
        connective_used: send.connective_used,
    })
}

pub fn substitute_receive_no_sort(
    receive: &Receive,
    depth: i32,
    env: &Env<Par>,
) -> Result<Receive, RholangError> {
    let binds_sub = receive
        .binds
        .iter()
        .map(|bind| {
            let sub_channel = substitute_par_no_sort(&bind.source, depth, env)?;
            let sub_patterns = bind
                .patterns
                .iter()
                .map(|p| substitute_par_no_sort(p, depth + 1, env))
                .collect::<Result<Vec<_>, _>>()?;
            Ok(ReceiveBind {
                patterns: sub_patterns,
                source: Box::new(sub_channel),
                remainder: bind.remainder.clone(),
                free_count: bind.free_count,
            })
        })
        .collect::<Result<Vec<_>, RholangError>>()?;
    let body_sub = substitute_par_no_sort(&receive.body, depth, &env.shift(receive.bind_count))?;
    Ok(Receive {
        binds: binds_sub,
        body: Box::new(body_sub),
        persistent: receive.persistent,
        peek: receive.peek,
        bind_count: receive.bind_count,
        locally_free: AlwaysEqual(until_free(&receive.locally_free.0, env.shift_amount())),
        connective_used: receive.connective_used,
    })
}

pub fn substitute_new_no_sort(
    new: &New,
    depth: i32,
    env: &Env<Par>,
) -> Result<New, RholangError> {
    let p_sub = substitute_par_no_sort(&new.p, depth, &env.shift(new.bind_count))?;
    Ok(New {
        bind_count: new.bind_count,
        p: Box::new(p_sub),
        uri: new.uri.clone(),
        injections: new.injections.clone(),
        locally_free: AlwaysEqual(until_free(&new.locally_free.0, env.shift_amount())),
    })
}

pub fn substitute_match_no_sort(
    m: &Match,
    depth: i32,
    env: &Env<Par>,
) -> Result<Match, RholangError> {
    let target_sub = substitute_par_no_sort(&m.target, depth, env)?;
    let cases_sub = m
        .cases
        .iter()
        .map(|case| {
            let par = substitute_par_no_sort(&case.source, depth, &env.shift(i32::from(case.free_count)))?;
            let sub_case = substitute_par_no_sort(&case.pattern, depth + 1, env)?;
            Ok(MatchCase {
                pattern: Box::new(sub_case),
                source: Box::new(par),
                free_count: case.free_count,
            })
        })
        .collect::<Result<Vec<_>, RholangError>>()?;
    Ok(Match {
        target: Box::new(target_sub),
        cases: cases_sub,
        locally_free: AlwaysEqual(until_free(&m.locally_free.0, env.shift_amount())),
        connective_used: m.connective_used,
    })
}

pub fn substitute_bundle_no_sort(
    bundle: &Bundle,
    depth: i32,
    env: &Env<Par>,
) -> Result<Bundle, RholangError> {
    let sub = substitute_par_no_sort(&bundle.body, depth, env)?;
    Ok(match single_bundle(&sub) {
        Some(single) => bundle.merge(single),
        None => Bundle {
            body: Box::new(sub),
            ..bundle.clone()
        },
    })
}

fn substitute_expr_delegate(
    expr: &Expr,
    depth: i32,
    env: &Env<Par>,
    sub: fn(&Par, i32, &Env<Par>) -> Result<Par, RholangError>,
) -> Result<Expr, RholangError> {
    let one = |p: &Par| sub(p, depth, env);
    let two = |p1: &Par, p2: &Par| -> Result<(Par, Par), RholangError> {
        Ok((sub(p1, depth, env)?, sub(p2, depth, env)?))
    };
    Ok(match expr {
        Expr::GBool(_)
        | Expr::GInt(_)
        | Expr::GBigInt(_)
        | Expr::GString(_)
        | Expr::GUri(_)
        | Expr::GByteArray(_) => expr.clone(),
        Expr::ENot(p) => Expr::ENot(Box::new(one(p)?)),
        Expr::ENeg(p) => Expr::ENeg(Box::new(one(p)?)),
        Expr::EMult(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EMult(Box::new(a), Box::new(b))
        }
        Expr::EDiv(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EDiv(Box::new(a), Box::new(b))
        }
        Expr::EMod(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EMod(Box::new(a), Box::new(b))
        }
        Expr::EPlus(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EPlus(Box::new(a), Box::new(b))
        }
        Expr::EMinus(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EMinus(Box::new(a), Box::new(b))
        }
        Expr::ELt(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::ELt(Box::new(a), Box::new(b))
        }
        Expr::ELte(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::ELte(Box::new(a), Box::new(b))
        }
        Expr::EGt(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EGt(Box::new(a), Box::new(b))
        }
        Expr::EGte(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EGte(Box::new(a), Box::new(b))
        }
        Expr::EEq(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EEq(Box::new(a), Box::new(b))
        }
        Expr::ENeq(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::ENeq(Box::new(a), Box::new(b))
        }
        Expr::EAnd(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EAnd(Box::new(a), Box::new(b))
        }
        Expr::EOr(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EOr(Box::new(a), Box::new(b))
        }
        Expr::EShortAnd(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EShortAnd(Box::new(a), Box::new(b))
        }
        Expr::EShortOr(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EShortOr(Box::new(a), Box::new(b))
        }
        Expr::EPercentPercent(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EPercentPercent(Box::new(a), Box::new(b))
        }
        Expr::EPlusPlus(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EPlusPlus(Box::new(a), Box::new(b))
        }
        Expr::EMinusMinus(p1, p2) => {
            let (a, b) = two(p1, p2)?;
            Expr::EMinusMinus(Box::new(a), Box::new(b))
        }
        Expr::EMatches(target, pattern) => {
            let (a, b) = two(target, pattern)?;
            Expr::EMatches(Box::new(a), Box::new(b))
        }
        Expr::EList(EList {
            ps,
            locally_free,
            connective_used,
            remainder,
        }) => {
            let sub_ps = ps.iter().map(&one).collect::<Result<Vec<_>, _>>()?;
            Expr::EList(EList {
                ps: sub_ps,
                locally_free: AlwaysEqual(until_free(&locally_free.0, env.shift_amount())),
                connective_used: *connective_used,
                remainder: remainder.clone(),
            })
        }
        Expr::ETuple(ETuple {
            ps,
            locally_free,
            connective_used,
        }) => {
            let sub_ps = ps.iter().map(&one).collect::<Result<Vec<_>, _>>()?;
            Expr::ETuple(ETuple {
                ps: sub_ps,
                locally_free: AlwaysEqual(until_free(&locally_free.0, env.shift_amount())),
                connective_used: *connective_used,
            })
        }
        Expr::ESet(ParSet {
            ps,
            connective_used,
            locally_free,
            remainder,
        }) => {
            let sub_ps = ps.iter().map(&one).collect::<Result<Vec<_>, _>>()?;
            Expr::ESet(ParSet {
                ps: sort_pars(sub_ps),
                connective_used: *connective_used,
                locally_free: AlwaysEqual(until_free(&locally_free.0, env.shift_amount())),
                remainder: remainder.clone(),
            })
        }
        Expr::EMap(ParMap {
            kvs,
            connective_used,
            locally_free,
            remainder,
        }) => {
            let sub_kvs = kvs
                .iter()
                .map(|(k, v)| Ok((sub(k, depth, env)?, sub(v, depth, env)?)))
                .collect::<Result<Vec<_>, RholangError>>()?;
            Expr::EMap(ParMap {
                kvs: sort_pairs(sub_kvs),
                connective_used: *connective_used,
                locally_free: AlwaysEqual(until_free(&locally_free.0, env.shift_amount())),
                remainder: remainder.clone(),
            })
        }
        Expr::EMethod(EMethod {
            method_name,
            target,
            arguments,
            locally_free,
            connective_used,
        }) => {
            let sub_target = one(target)?;
            let sub_arguments = arguments.iter().map(&one).collect::<Result<Vec<_>, _>>()?;
            Expr::EMethod(EMethod {
                method_name: method_name.clone(),
                target: Box::new(sub_target),
                arguments: sub_arguments,
                locally_free: AlwaysEqual(until_free(&locally_free.0, env.shift_amount())),
                connective_used: *connective_used,
            })
        }
        Expr::EVar(_) => {
            // Handled in sub_exprs; a bare EVar reaching here is returned unchanged.
            expr.clone()
        }
    })
}

pub fn substitute_expr_no_sort(
    expr: &Expr,
    depth: i32,
    env: &Env<Par>,
) -> Result<Expr, RholangError> {
    substitute_expr_delegate(expr, depth, env, substitute_par_no_sort)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn substitutes_bound_var() {
        let env = Env::new().put(Par {
            exprs: vec![Expr::GInt(42)],
            ..Default::default()
        });
        let par: Par = Par {
            exprs: vec![Expr::EVar(Box::new(Var::BoundVar(0)))],
            ..Default::default()
        };
        let result = substitute_par(&par, 0, &env).unwrap();
        assert_eq!(result.exprs, vec![Expr::GInt(42)]);
    }

    #[test]
    fn free_var_at_depth_zero_is_illegal() {
        let env = Env::new();
        let par: Par = Par {
            exprs: vec![Expr::EVar(Box::new(Var::FreeVar(0)))],
            ..Default::default()
        };
        assert!(substitute_par(&par, 0, &env).is_err());
    }
}
