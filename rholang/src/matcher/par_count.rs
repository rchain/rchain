//! Counting the fields of a `Par` for spatial matching (port of `matcher/ParCount.scala`).

use rchain_models::ast::{Connective, ConnectiveBody, Expr, Par, Sort, Var};

use crate::matcher::par_spatial_matcher_utils::no_frees;

/// The count of each `Par` field (port of `ParCount`).
#[derive(Clone, Debug, Default, PartialEq, Eq)]
pub struct ParCount {
    pub sends: i32,
    pub receives: i32,
    pub news: i32,
    pub exprs: i32,
    pub matches: i32,
    pub unforgeables: i32,
    pub bundles: i32,
}

/// Saturating add: saturates to `i32::MAX` on positive overflow (port of `saturatingAdd`).
fn saturating_add(l: i32, r: i32) -> i32 {
    let res = l.wrapping_add(r);
    let mask = if res < l { -1 } else { 0 };
    (res | mask) & !i32::MIN
}

impl ParCount {
    fn bin_op(&self, op: fn(i32, i32) -> i32, other: &ParCount) -> ParCount {
        ParCount {
            sends: op(self.sends, other.sends),
            receives: op(self.receives, other.receives),
            news: op(self.news, other.news),
            exprs: op(self.exprs, other.exprs),
            matches: op(self.matches, other.matches),
            unforgeables: op(self.unforgeables, other.unforgeables),
            bundles: op(self.bundles, other.bundles),
        }
    }

    pub fn min(&self, other: &ParCount) -> ParCount {
        self.bin_op(i32::min, other)
    }

    pub fn max(&self, other: &ParCount) -> ParCount {
        self.bin_op(i32::max, other)
    }

    pub fn add(&self, other: &ParCount) -> ParCount {
        self.bin_op(saturating_add, other)
    }

    pub fn from_par<S: Sort>(par: &Par<S>) -> ParCount {
        ParCount {
            sends: par.sends.len() as i32,
            receives: par.receives.len() as i32,
            news: par.news.len() as i32,
            matches: par.matches.len() as i32,
            exprs: par.exprs.len() as i32,
            unforgeables: par.unforgeables.len() as i32,
            bundles: par.bundles.len() as i32,
        }
    }

    pub fn max_count() -> ParCount {
        ParCount {
            sends: i32::MAX,
            receives: i32::MAX,
            news: i32::MAX,
            matches: i32::MAX,
            exprs: i32::MAX,
            unforgeables: i32::MAX,
            bundles: i32::MAX,
        }
    }

    fn is_free_var(expr: &Expr) -> bool {
        match expr {
            Expr::EVar(v) => matches!(**v, Var::FreeVar(_) | Var::Wildcard),
            _ => false,
        }
    }

    /// Compute the min/max field counts a `Par` pattern can match (port of `ParCount.minMax`).
    pub fn min_max(par: &Par) -> (ParCount, ParCount) {
        let pc = ParCount::from_par(&no_frees(par));
        let wildcard = par.exprs.iter().any(ParCount::is_free_var);
        let min_init = pc.clone();
        let max_init = if wildcard {
            ParCount::max_count()
        } else {
            pc
        };
        par.connectives.iter().fold((min_init, max_init), |(min, max), con| {
            let (cmin, cmax) = ParCount::min_max_connective(con);
            (min.add(&cmin), max.add(&cmax))
        })
    }

    pub fn min_max_connective(con: &Connective) -> (ParCount, ParCount) {
        match con {
            Connective::ConnAnd(ConnectiveBody { ps }) => {
                let p_min_max: Vec<(ParCount, ParCount)> =
                    ps.iter().map(ParCount::min_max).collect();
                let min = p_min_max
                    .iter()
                    .fold(ParCount::default(), |acc, (m, _)| acc.max(m));
                let max = p_min_max
                    .iter()
                    .fold(ParCount::max_count(), |acc, (_, m)| acc.min(m));
                (min, max)
            }
            Connective::ConnOr(ConnectiveBody { ps }) => {
                let p_min_max: Vec<(ParCount, ParCount)> =
                    ps.iter().map(ParCount::min_max).collect();
                let min = p_min_max
                    .iter()
                    .fold(ParCount::max_count(), |acc, (m, _)| acc.min(m));
                let max = p_min_max
                    .iter()
                    .fold(ParCount::default(), |acc, (_, m)| acc.max(m));
                (min, max)
            }
            Connective::ConnNot(_) => (ParCount::default(), ParCount::max_count()),
            Connective::Empty | Connective::VarRef(_) => {
                (ParCount::default(), ParCount::default())
            }
            Connective::ConnBool(_)
            | Connective::ConnInt(_)
            | Connective::ConnBigInt(_)
            | Connective::ConnString(_)
            | Connective::ConnUri(_)
            | Connective::ConnByteArray(_) => (
                ParCount {
                    exprs: 1,
                    ..ParCount::default()
                },
                ParCount {
                    exprs: 1,
                    ..ParCount::default()
                },
            ),
        }
    }
}
