//! The canonicalizing sorter (Law 1).
//!
//! Mirrors `models/src/main/scala/coop/rchain/models/rholang/sorter/` (`ScoreTree.scala`,
//! `ordering.scala`, and the `*SortMatcher.scala` files). `sort` recursively builds a total-order
//! score for each term, sorts each of `Par`'s eight list fields by that score, and reconstructs a
//! canonical `Par`. Ordered sub-lists (a `Send`'s data, an `EMethod`'s arguments, …) keep their
//! input order — only `Par`'s fields and `New.uri` are canonicalized here. Byte comparison is
//! signed (`bsCompare` uses `Byte.compareTo`).

use std::cmp::Ordering;

use num_bigint::BigInt;

use crate::ast::*;

// --- Score constants (mirroring `ScoreTree.Score`) ---------------------------------------------

const ABSENT: i64 = 0;
const BOOL: i64 = 1;
const INT: i64 = 2;
const STRING: i64 = 3;
const URI: i64 = 4;
const PRIVATE: i64 = 5;
const ELIST: i64 = 6;
const ETUPLE: i64 = 7;
const ESET: i64 = 8;
const EMAP: i64 = 9;
const DEPLOYER_AUTH: i64 = 10;
const DEPLOY_ID: i64 = 11;
const SYS_AUTH_TOKEN: i64 = 12;
const BIG_INT: i64 = 13;
const BOUND_VAR: i64 = 50;
const FREE_VAR: i64 = 51;
const WILDCARD: i64 = 52;
const EVAR: i64 = 100;
const ENEG: i64 = 101;
const EMULT: i64 = 102;
const EDIV: i64 = 103;
const EPLUS: i64 = 104;
const EMINUS: i64 = 105;
const ELT: i64 = 106;
const ELTE: i64 = 107;
const EGT: i64 = 108;
const EGTE: i64 = 109;
const EEQ: i64 = 110;
const ENEQ: i64 = 111;
const ENOT: i64 = 112;
const EAND: i64 = 113;
const EOR: i64 = 114;
const EMETHOD: i64 = 115;
const EBYTEARR: i64 = 116;
const EMATCHES: i64 = 118;
const EPERCENT: i64 = 119;
const EPLUSPLUS: i64 = 120;
const EMINUSMINUS: i64 = 121;
const EMOD: i64 = 122;
const ESHORTAND: i64 = 123;
const ESHORTOR: i64 = 124;
const SEND: i64 = 300;
const RECEIVE: i64 = 301;
const NEW: i64 = 303;
const MATCH: i64 = 304;
const BUNDLE_EQUIV: i64 = 305;
const BUNDLE_READ: i64 = 306;
const BUNDLE_WRITE: i64 = 307;
const BUNDLE_READ_WRITE: i64 = 308;
const CONNECTIVE_NOT: i64 = 400;
const CONNECTIVE_AND: i64 = 401;
const CONNECTIVE_OR: i64 = 402;
const CONNECTIVE_VARREF: i64 = 403;
const CONNECTIVE_BOOL: i64 = 404;
const CONNECTIVE_INT: i64 = 405;
const CONNECTIVE_STRING: i64 = 406;
const CONNECTIVE_URI: i64 = 407;
const CONNECTIVE_BYTEARRAY: i64 = 408;
const CONNECTIVE_BIG_INT: i64 = 409;
const PAR: i64 = 999;

// --- Score tree ---------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
enum ScoreAtom {
    Int(i64),
    BigInt(BigInt),
    Str(String),
    Bytes(Vec<u8>),
}

impl ScoreAtom {
    fn cmp(&self, other: &Self) -> Ordering {
        use ScoreAtom::*;
        match (self, other) {
            (Int(a), Int(b)) => a.cmp(b),
            (Int(_), _) => Ordering::Less,
            (_, Int(_)) => Ordering::Greater,
            (BigInt(a), BigInt(b)) => a.cmp(b),
            (BigInt(_), _) => Ordering::Less,
            (_, BigInt(_)) => Ordering::Greater,
            (Str(a), Str(b)) => a.cmp(b),
            (Str(_), _) => Ordering::Less,
            (_, Str(_)) => Ordering::Greater,
            (Bytes(a), Bytes(b)) => compare_bytes_signed(a, b),
        }
    }
}

/// Signed byte comparison (the Scala `bsCompare`, which uses Java's `Byte.compareTo`).
fn compare_bytes_signed(a: &[u8], b: &[u8]) -> Ordering {
    let mut i = 0;
    loop {
        match (a.get(i), b.get(i)) {
            (Some(x), Some(y)) => match rchain_crypto::util::sorting::cmp_signed_byte(*x, *y) {
                Ordering::Equal => i += 1,
                other => return other,
            },
            (Some(_), None) => return Ordering::Greater,
            (None, Some(_)) => return Ordering::Less,
            (None, None) => return Ordering::Equal,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
enum Tree {
    Leaf(ScoreAtom),
    Node(Vec<Tree>),
}

impl Ord for Tree {
    fn cmp(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Tree::Leaf(a), Tree::Leaf(b)) => a.cmp(b),
            (Tree::Leaf(_), Tree::Node(_)) => Ordering::Less,
            (Tree::Node(_), Tree::Leaf(_)) => Ordering::Greater,
            (Tree::Node(a), Tree::Node(b)) => compare_children(a, b),
        }
    }
}

impl PartialOrd for Tree {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn compare_children(a: &[Tree], b: &[Tree]) -> Ordering {
    match (a.first(), b.first()) {
        (None, None) => Ordering::Equal,
        (None, Some(_)) => Ordering::Less,
        (Some(_), None) => Ordering::Greater,
        (Some(x), Some(y)) => match x.cmp(y) {
            Ordering::Equal => compare_children(&a[1..], &b[1..]),
            other => other,
        },
    }
}

/// A term paired with its score tree.
#[derive(Clone, Debug, PartialEq, Eq)]
struct ScoredTerm<T> {
    term: T,
    score: Tree,
}

fn leaf_i64(v: i64) -> Tree {
    Tree::Leaf(ScoreAtom::Int(v))
}

fn leaf_bigint(v: BigInt) -> Tree {
    Tree::Leaf(ScoreAtom::BigInt(v))
}

fn leaf_str(v: String) -> Tree {
    Tree::Leaf(ScoreAtom::Str(v))
}

fn leaf_bytes(v: Vec<u8>) -> Tree {
    Tree::Leaf(ScoreAtom::Bytes(v))
}

fn leaves(vals: &[i64]) -> Tree {
    Tree::Node(vals.iter().map(|&v| leaf_i64(v)).collect())
}

fn node_score(score: i64, children: Vec<Tree>) -> Tree {
    let mut all = vec![leaf_i64(score)];
    all.extend(children);
    Tree::Node(all)
}

/// Sort scored terms by score and drop the scores (the Scala `List[ScoredTerm[T]].sorted.map(_.term)`).
fn sort_scored<T>(mut scored: Vec<ScoredTerm<T>>) -> Vec<T> {
    scored.sort_by(|a, b| a.score.cmp(&b.score));
    scored.into_iter().map(|st| st.term).collect()
}

// --- Matchers -----------------------------------------------------------------------------------

fn sort_var(v: &Var) -> ScoredTerm<Var> {
    match v {
        Var::BoundVar(level) => ScoredTerm {
            term: v.clone(),
            score: leaves(&[BOUND_VAR, *level as i64]),
        },
        Var::FreeVar(level) => ScoredTerm {
            term: v.clone(),
            score: leaves(&[FREE_VAR, *level as i64]),
        },
        Var::Wildcard => ScoredTerm {
            term: v.clone(),
            score: leaves(&[WILDCARD]),
        },
        Var::Empty => ScoredTerm {
            term: Var::Empty,
            score: leaf_i64(ABSENT),
        },
    }
}

fn sort_gbool(g: bool) -> ScoredTerm<bool> {
    if g {
        ScoredTerm {
            term: g,
            score: leaves(&[BOOL, 0]),
        }
    } else {
        ScoredTerm {
            term: g,
            score: leaves(&[BOOL, 1]),
        }
    }
}

fn sort_send(s: &Send) -> ScoredTerm<Send> {
    let sorted_chan = sort_par(&s.chan);
    let scored_data: Vec<ScoredTerm<Name>> = s.data.iter().map(sort_par).collect();
    let persistent_score = if s.persistent { 1 } else { 0 };
    let connective_used_score = if s.connective_used { 1 } else { 0 };

    let mut children = vec![leaf_i64(persistent_score), sorted_chan.score.clone()];
    children.extend(scored_data.iter().map(|st| st.score.clone()));
    children.push(leaf_i64(connective_used_score));

    ScoredTerm {
        term: Send {
            chan: Box::new(sorted_chan.term),
            data: scored_data.into_iter().map(|st| st.term).collect(),
            persistent: s.persistent,
            locally_free: s.locally_free.clone(),
            connective_used: s.connective_used,
        },
        score: node_score(SEND, children),
    }
}

fn sort_receive_bind(bind: &ReceiveBind) -> ScoredTerm<ReceiveBind> {
    let scored_patterns: Vec<ScoredTerm<Name>> = bind.patterns.iter().map(sort_par).collect();
    let sorted_channel = sort_par(&bind.source);
    let sorted_remainder = match &bind.remainder {
        Some(var) => {
            let scored = sort_var(var);
            ScoredTerm {
                term: Some(Box::new(scored.term)),
                score: scored.score,
            }
        }
        None => ScoredTerm {
            term: None,
            score: leaf_i64(ABSENT),
        },
    };

    let mut children = vec![sorted_channel.score.clone()];
    children.extend(scored_patterns.iter().map(|st| st.score.clone()));
    children.push(sorted_remainder.score.clone());

    ScoredTerm {
        term: ReceiveBind {
            patterns: scored_patterns.into_iter().map(|st| st.term).collect(),
            source: Box::new(sorted_channel.term),
            remainder: bind.remainder.clone(),
            free_count: bind.free_count,
        },
        score: Tree::Node(children),
    }
}

fn sort_receive(r: &Receive) -> ScoredTerm<Receive> {
    let scored_binds: Vec<ScoredTerm<ReceiveBind>> = r.binds.iter().map(sort_receive_bind).collect();
    let persistent_score = if r.persistent { 1 } else { 0 };
    let peek_score = if r.peek { 1 } else { 0 };
    let connective_used_score = if r.connective_used { 1 } else { 0 };
    let sorted_body = sort_par(&r.body);

    let mut children = vec![leaf_i64(persistent_score), leaf_i64(peek_score)];
    children.extend(scored_binds.iter().map(|st| st.score.clone()));
    children.push(sorted_body.score.clone());
    children.push(leaf_i64(r.bind_count as i64));
    children.push(leaf_i64(connective_used_score));

    ScoredTerm {
        term: Receive {
            binds: scored_binds.into_iter().map(|st| st.term).collect(),
            body: Box::new(sorted_body.term),
            persistent: r.persistent,
            peek: r.peek,
            bind_count: r.bind_count,
            locally_free: r.locally_free.clone(),
            connective_used: r.connective_used,
        },
        score: node_score(RECEIVE, children),
    }
}

fn sort_new(n: &New) -> ScoredTerm<New> {
    let sorted_par = sort_par(&n.p);
    let mut sorted_uri = n.uri.clone();
    sorted_uri.sort();
    let uri_score: Vec<Tree> = if sorted_uri.is_empty() {
        vec![leaf_i64(ABSENT)]
    } else {
        sorted_uri.iter().map(|u| leaf_str(u.clone())).collect()
    };
    // The Scala iterates a HashMap in nondeterministic order; sort by key for determinism
    // (documented deviation, mirroring the `merging.rs` BTreeMap precedent).
    let mut injections: Vec<(&String, ScoredTerm<Par>)> = n
        .injections
        .iter()
        .map(|(k, v)| (k, sort_par(v)))
        .collect();
    injections.sort_by(|a, b| a.0.cmp(b.0));
    let injections_score: Vec<Tree> = if injections.is_empty() {
        vec![leaf_i64(ABSENT)]
    } else {
        injections
            .iter()
            .map(|(k, st)| Tree::Node(vec![leaf_str((*k).clone()), st.score.clone()]))
            .collect()
    };

    let mut children = vec![leaf_i64(NEW), leaf_i64(n.bind_count as i64)];
    children.extend(uri_score);
    children.extend(injections_score);
    children.push(sorted_par.score.clone());

    ScoredTerm {
        term: New {
            bind_count: n.bind_count,
            p: Box::new(sorted_par.term),
            uri: sorted_uri,
            injections: injections
                .into_iter()
                .map(|(k, st)| ((*k).clone(), st.term))
                .collect(),
            locally_free: n.locally_free.clone(),
        },
        score: Tree::Node(children),
    }
}

fn sort_match_case(case: &MatchCase) -> ScoredTerm<MatchCase> {
    let sorted_pattern = sort_par(&case.pattern);
    let sorted_body = sort_par(&case.source);
    ScoredTerm {
        term: MatchCase {
            pattern: Box::new(sorted_pattern.term),
            source: Box::new(sorted_body.term),
            free_count: case.free_count,
        },
        score: Tree::Node(vec![
            sorted_pattern.score,
            sorted_body.score,
            leaf_i64(i64::from(i32::from(case.free_count))),
        ]),
    }
}

fn sort_match(m: &Match) -> ScoredTerm<Match> {
    let sorted_value = sort_par(&m.target);
    let scored_cases: Vec<ScoredTerm<MatchCase>> = m.cases.iter().map(sort_match_case).collect();
    let connective_used_score = if m.connective_used { 1 } else { 0 };

    let mut children = vec![sorted_value.score.clone()];
    children.extend(scored_cases.iter().map(|st| st.score.clone()));
    children.push(leaf_i64(connective_used_score));

    ScoredTerm {
        term: Match {
            target: Box::new(sorted_value.term),
            cases: scored_cases.into_iter().map(|st| st.term).collect(),
            locally_free: m.locally_free.clone(),
            connective_used: m.connective_used,
        },
        score: node_score(MATCH, children),
    }
}

fn sort_bundle(b: &Bundle) -> ScoredTerm<Bundle> {
    let score = if b.write_flag && b.read_flag {
        BUNDLE_READ_WRITE
    } else if b.write_flag && !b.read_flag {
        BUNDLE_WRITE
    } else if !b.write_flag && b.read_flag {
        BUNDLE_READ
    } else {
        BUNDLE_EQUIV
    };
    let sorted_par = sort_par(&b.body);
    ScoredTerm {
        term: Bundle {
            body: Box::new(sorted_par.term),
            write_flag: b.write_flag,
            read_flag: b.read_flag,
        },
        score: node_score(score, vec![sorted_par.score]),
    }
}

fn sort_connective(c: &Connective) -> ScoredTerm<Connective> {
    match c {
        Connective::ConnAnd(cb) => {
            let scored: Vec<ScoredTerm<Par>> = cb.ps.iter().map(sort_par).collect();
            let children = scored.iter().map(|st| st.score.clone()).collect();
            ScoredTerm {
                term: Connective::ConnAnd(ConnectiveBody {
                    ps: scored.into_iter().map(|st| st.term).collect(),
                }),
                score: node_score(CONNECTIVE_AND, children),
            }
        }
        Connective::ConnOr(cb) => {
            let scored: Vec<ScoredTerm<Par>> = cb.ps.iter().map(sort_par).collect();
            let children = scored.iter().map(|st| st.score.clone()).collect();
            ScoredTerm {
                term: Connective::ConnOr(ConnectiveBody {
                    ps: scored.into_iter().map(|st| st.term).collect(),
                }),
                score: node_score(CONNECTIVE_OR, children),
            }
        }
        Connective::ConnNot(p) => {
            let scored = sort_par(p);
            ScoredTerm {
                term: Connective::ConnNot(Box::new(scored.term)),
                score: node_score(CONNECTIVE_NOT, vec![scored.score]),
            }
        }
        Connective::VarRef(vr) => ScoredTerm {
            term: c.clone(),
            score: leaves(&[CONNECTIVE_VARREF, vr.index as i64, vr.depth as i64]),
        },
        Connective::ConnBool(b) => ScoredTerm {
            term: c.clone(),
            score: leaves(&[CONNECTIVE_BOOL, if *b { 1 } else { 0 }]),
        },
        Connective::ConnInt(b) => ScoredTerm {
            term: c.clone(),
            score: leaves(&[CONNECTIVE_INT, if *b { 1 } else { 0 }]),
        },
        Connective::ConnBigInt(b) => ScoredTerm {
            term: c.clone(),
            score: leaves(&[CONNECTIVE_BIG_INT, if *b { 1 } else { 0 }]),
        },
        Connective::ConnString(b) => ScoredTerm {
            term: c.clone(),
            score: leaves(&[CONNECTIVE_STRING, if *b { 1 } else { 0 }]),
        },
        Connective::ConnUri(b) => ScoredTerm {
            term: c.clone(),
            score: leaves(&[CONNECTIVE_URI, if *b { 1 } else { 0 }]),
        },
        Connective::ConnByteArray(b) => ScoredTerm {
            term: c.clone(),
            score: leaves(&[CONNECTIVE_BYTEARRAY, if *b { 1 } else { 0 }]),
        },
        Connective::Empty => ScoredTerm {
            term: Connective::Empty,
            score: leaf_i64(ABSENT),
        },
    }
}

fn sort_unforgeable(unf: &GUnforgeable) -> ScoredTerm<GUnforgeable> {
    match unf {
        GUnforgeable::GPrivate(gp) => ScoredTerm {
            term: unf.clone(),
            score: node_score(PRIVATE, vec![leaf_bytes(gp.id.clone())]),
        },
        GUnforgeable::GDeployerId(id) => ScoredTerm {
            term: unf.clone(),
            score: node_score(DEPLOYER_AUTH, vec![leaf_bytes(id.public_key.clone())]),
        },
        GUnforgeable::GDeployId(id) => ScoredTerm {
            term: unf.clone(),
            score: node_score(DEPLOY_ID, vec![leaf_bytes(id.sig.clone())]),
        },
        GUnforgeable::GSysAuthToken => ScoredTerm {
            term: unf.clone(),
            score: node_score(SYS_AUTH_TOKEN, vec![]),
        },
        GUnforgeable::Empty => ScoredTerm {
            term: unf.clone(),
            score: node_score(ABSENT, vec![]),
        },
    }
}

fn remainder_score(remainder: &Option<Box<Var>>) -> Tree {
    match remainder {
        Some(var) => sort_var(var).score,
        None => leaf_i64(-1),
    }
}

fn binary_expr(
    score_const: i64,
    p1: &Par,
    p2: &Par,
    rebuild: impl FnOnce(Box<Par>, Box<Par>) -> Expr,
) -> ScoredTerm<Expr> {
    let s1 = sort_par(p1);
    let s2 = sort_par(p2);
    ScoredTerm {
        term: rebuild(Box::new(s1.term), Box::new(s2.term)),
        score: node_score(score_const, vec![s1.score, s2.score]),
    }
}

fn sort_expr(e: &Expr) -> ScoredTerm<Expr> {
    match e {
        Expr::GBool(g) => {
            let scored = sort_gbool(*g);
            ScoredTerm {
                term: e.clone(),
                score: scored.score,
            }
        }
        Expr::GInt(v) => ScoredTerm {
            term: e.clone(),
            score: leaves(&[INT, *v]),
        },
        Expr::GBigInt(v) => ScoredTerm {
            term: e.clone(),
            score: node_score(BIG_INT, vec![leaf_bigint(v.clone())]),
        },
        Expr::GString(v) => ScoredTerm {
            term: e.clone(),
            score: node_score(STRING, vec![leaf_str(v.clone())]),
        },
        Expr::GUri(v) => ScoredTerm {
            term: e.clone(),
            score: node_score(URI, vec![leaf_str(v.clone())]),
        },
        Expr::GByteArray(v) => ScoredTerm {
            term: e.clone(),
            score: node_score(EBYTEARR, vec![leaf_bytes(v.clone())]),
        },
        Expr::ENot(p) => {
            let scored = sort_par(p);
            ScoredTerm {
                term: Expr::ENot(Box::new(scored.term)),
                score: node_score(ENOT, vec![scored.score]),
            }
        }
        Expr::ENeg(p) => {
            let scored = sort_par(p);
            ScoredTerm {
                term: Expr::ENeg(Box::new(scored.term)),
                score: node_score(ENEG, vec![scored.score]),
            }
        }
        Expr::EVar(v) => {
            let scored = sort_var(v);
            ScoredTerm {
                term: Expr::EVar(Box::new(scored.term)),
                score: node_score(EVAR, vec![scored.score]),
            }
        }
        Expr::EMult(p1, p2) => binary_expr(EMULT, p1, p2, |a, b| Expr::EMult(a, b)),
        Expr::EDiv(p1, p2) => binary_expr(EDIV, p1, p2, |a, b| Expr::EDiv(a, b)),
        Expr::EMod(p1, p2) => binary_expr(EMOD, p1, p2, |a, b| Expr::EMod(a, b)),
        Expr::EPlus(p1, p2) => binary_expr(EPLUS, p1, p2, |a, b| Expr::EPlus(a, b)),
        Expr::EMinus(p1, p2) => binary_expr(EMINUS, p1, p2, |a, b| Expr::EMinus(a, b)),
        Expr::ELt(p1, p2) => binary_expr(ELT, p1, p2, |a, b| Expr::ELt(a, b)),
        Expr::ELte(p1, p2) => binary_expr(ELTE, p1, p2, |a, b| Expr::ELte(a, b)),
        Expr::EGt(p1, p2) => binary_expr(EGT, p1, p2, |a, b| Expr::EGt(a, b)),
        Expr::EGte(p1, p2) => binary_expr(EGTE, p1, p2, |a, b| Expr::EGte(a, b)),
        Expr::EEq(p1, p2) => binary_expr(EEQ, p1, p2, |a, b| Expr::EEq(a, b)),
        Expr::ENeq(p1, p2) => binary_expr(ENEQ, p1, p2, |a, b| Expr::ENeq(a, b)),
        Expr::EAnd(p1, p2) => binary_expr(EAND, p1, p2, |a, b| Expr::EAnd(a, b)),
        Expr::EOr(p1, p2) => binary_expr(EOR, p1, p2, |a, b| Expr::EOr(a, b)),
        Expr::EShortAnd(p1, p2) => binary_expr(ESHORTAND, p1, p2, |a, b| Expr::EShortAnd(a, b)),
        Expr::EShortOr(p1, p2) => binary_expr(ESHORTOR, p1, p2, |a, b| Expr::EShortOr(a, b)),
        Expr::EMatches(p1, p2) => binary_expr(EMATCHES, p1, p2, |a, b| Expr::EMatches(a, b)),
        Expr::EPercentPercent(p1, p2) => {
            binary_expr(EPERCENT, p1, p2, |a, b| Expr::EPercentPercent(a, b))
        }
        Expr::EPlusPlus(p1, p2) => binary_expr(EPLUSPLUS, p1, p2, |a, b| Expr::EPlusPlus(a, b)),
        Expr::EMinusMinus(p1, p2) => {
            binary_expr(EMINUSMINUS, p1, p2, |a, b| Expr::EMinusMinus(a, b))
        }
        Expr::EList(list) => {
            let scored: Vec<ScoredTerm<Par>> = list.ps.iter().map(sort_par).collect();
            let remainder = remainder_score(&list.remainder);
            let connective_used_score = if list.connective_used { 1 } else { 0 };
            let mut children = vec![leaf_i64(ELIST), remainder];
            children.extend(scored.iter().map(|st| st.score.clone()));
            children.push(leaf_i64(connective_used_score));
            ScoredTerm {
                term: Expr::EList(EList {
                    ps: scored.into_iter().map(|st| st.term).collect(),
                    locally_free: list.locally_free.clone(),
                    connective_used: list.connective_used,
                    remainder: list.remainder.clone(),
                }),
                score: Tree::Node(children),
            }
        }
        Expr::ETuple(tuple) => {
            let scored: Vec<ScoredTerm<Par>> = tuple.ps.iter().map(sort_par).collect();
            let connective_used_score = if tuple.connective_used { 1 } else { 0 };
            let mut children = vec![leaf_i64(ETUPLE)];
            children.extend(scored.iter().map(|st| st.score.clone()));
            children.push(leaf_i64(connective_used_score));
            ScoredTerm {
                term: Expr::ETuple(ETuple {
                    ps: scored.into_iter().map(|st| st.term).collect(),
                    locally_free: tuple.locally_free.clone(),
                    connective_used: tuple.connective_used,
                }),
                score: Tree::Node(children),
            }
        }
        Expr::ESet(set) => {
            let scored: Vec<ScoredTerm<Par>> = set.ps.iter().map(sort_par).collect();
            let remainder = remainder_score(&set.remainder);
            let connective_used_score = if set.connective_used { 1 } else { 0 };
            let mut children = vec![leaf_i64(ESET), remainder];
            children.extend(scored.iter().map(|st| st.score.clone()));
            children.push(leaf_i64(connective_used_score));
            ScoredTerm {
                term: Expr::ESet(ParSet {
                    ps: scored.into_iter().map(|st| st.term).collect(),
                    connective_used: set.connective_used,
                    locally_free: set.locally_free.clone(),
                    remainder: set.remainder.clone(),
                }),
                score: Tree::Node(children),
            }
        }
        Expr::EMap(map) => {
            let scored: Vec<ScoredTerm<(Par, Par)>> = map
                .kvs
                .iter()
                .map(|(k, v)| {
                    let sk = sort_par(k);
                    let sv = sort_par(v);
                    ScoredTerm {
                        term: (sk.term, sv.term),
                        score: sk.score,
                    }
                })
                .collect();
            let remainder = remainder_score(&map.remainder);
            let connective_used_score = if map.connective_used { 1 } else { 0 };
            let mut children = vec![leaf_i64(EMAP), remainder];
            children.extend(scored.iter().map(|st| st.score.clone()));
            children.push(leaf_i64(connective_used_score));
            ScoredTerm {
                term: Expr::EMap(ParMap {
                    kvs: scored.into_iter().map(|st| st.term).collect(),
                    connective_used: map.connective_used,
                    locally_free: map.locally_free.clone(),
                    remainder: map.remainder.clone(),
                }),
                score: Tree::Node(children),
            }
        }
        Expr::EMethod(em) => {
            let scored_args: Vec<ScoredTerm<Par>> = em.arguments.iter().map(sort_par).collect();
            let sorted_target = sort_par(&em.target);
            let connective_used_score = if em.connective_used { 1 } else { 0 };
            let mut children = vec![
                leaf_i64(EMETHOD),
                leaf_str(em.method_name.clone()),
                sorted_target.score.clone(),
            ];
            children.extend(scored_args.iter().map(|st| st.score.clone()));
            children.push(leaf_i64(connective_used_score));
            ScoredTerm {
                term: Expr::EMethod(EMethod {
                    method_name: em.method_name.clone(),
                    target: Box::new(sorted_target.term),
                    arguments: scored_args.into_iter().map(|st| st.term).collect(),
                    locally_free: em.locally_free.clone(),
                    connective_used: em.connective_used,
                }),
                score: Tree::Node(children),
            }
        }
    }
}

fn sort_par<S: Sort>(par: &Par<S>) -> ScoredTerm<Par<S>> {
    let mut sends = par.sends.iter().map(sort_send).collect::<Vec<_>>();
    let mut receives = par.receives.iter().map(sort_receive).collect::<Vec<_>>();
    let mut exprs = par.exprs.iter().map(sort_expr).collect::<Vec<_>>();
    let mut news = par.news.iter().map(sort_new).collect::<Vec<_>>();
    let mut matches = par.matches.iter().map(sort_match).collect::<Vec<_>>();
    let mut bundles = par.bundles.iter().map(sort_bundle).collect::<Vec<_>>();
    let mut connectives = par.connectives.iter().map(sort_connective).collect::<Vec<_>>();
    let mut unforgeables = par
        .unforgeables
        .iter()
        .map(sort_unforgeable)
        .collect::<Vec<_>>();

    sends.sort_by(|a, b| a.score.cmp(&b.score));
    receives.sort_by(|a, b| a.score.cmp(&b.score));
    exprs.sort_by(|a, b| a.score.cmp(&b.score));
    news.sort_by(|a, b| a.score.cmp(&b.score));
    matches.sort_by(|a, b| a.score.cmp(&b.score));
    bundles.sort_by(|a, b| a.score.cmp(&b.score));
    connectives.sort_by(|a, b| a.score.cmp(&b.score));
    unforgeables.sort_by(|a, b| a.score.cmp(&b.score));

    let connective_used_score = if par.connective_used { 1 } else { 0 };
    let mut children = Vec::new();
    children.extend(sends.iter().map(|st| st.score.clone()));
    children.extend(receives.iter().map(|st| st.score.clone()));
    children.extend(exprs.iter().map(|st| st.score.clone()));
    children.extend(news.iter().map(|st| st.score.clone()));
    children.extend(matches.iter().map(|st| st.score.clone()));
    children.extend(bundles.iter().map(|st| st.score.clone()));
    children.extend(connectives.iter().map(|st| st.score.clone()));
    children.extend(unforgeables.iter().map(|st| st.score.clone()));
    children.push(leaf_i64(connective_used_score));

    ScoredTerm {
        term: Par {
            sends: sends.into_iter().map(|st| st.term).collect(),
            receives: receives.into_iter().map(|st| st.term).collect(),
            exprs: exprs.into_iter().map(|st| st.term).collect(),
            news: news.into_iter().map(|st| st.term).collect(),
            matches: matches.into_iter().map(|st| st.term).collect(),
            bundles: bundles.into_iter().map(|st| st.term).collect(),
            connectives: connectives.into_iter().map(|st| st.term).collect(),
            unforgeables: unforgeables.into_iter().map(|st| st.term).collect(),
            locally_free: par.locally_free.clone(),
            connective_used: par.connective_used,
            ..Default::default()
        },
        score: node_score(PAR, children),
    }
}

// --- Public API ---------------------------------------------------------------------------------

/// Canonicalize a single `Par` (the Scala `Sortable[Par].sortMatch(par).term`).
pub fn sort_par_term<S: Sort>(par: &Par<S>) -> Par<S> {
    sort_par(par).term
}

/// Canonicalize a single `Send` (the Scala `Sortable[Send].sortMatch(s).term`).
pub fn sort_send_term(send: &Send) -> Send {
    sort_send(send).term
}

/// Canonicalize a single `Receive` (the Scala `Sortable[Receive].sortMatch(r).term`).
pub fn sort_receive_term(receive: &Receive) -> Receive {
    sort_receive(receive).term
}

/// Canonicalize a single `New` (the Scala `Sortable[New].sortMatch(n).term`).
pub fn sort_new_term(new: &New) -> New {
    sort_new(new).term
}

/// Canonicalize a single `Match` (the Scala `Sortable[Match].sortMatch(m).term`).
pub fn sort_match_term(m: &Match) -> Match {
    sort_match(m).term
}

/// Canonicalize a single `Bundle` (the Scala `Sortable[Bundle].sortMatch(b).term`).
pub fn sort_bundle_term(bundle: &Bundle) -> Bundle {
    sort_bundle(bundle).term
}

/// Canonicalize a single `Expr` (the Scala `Sortable[Expr].sortMatch(e).term`).
pub fn sort_expr_term(expr: &Expr) -> Expr {
    sort_expr(expr).term
}

/// Sort a list of `Par`s by canonical score (the Scala `List[Par].sort`).
pub fn sort_pars(pars: Vec<Par>) -> Vec<Par> {
    sort_scored(pars.iter().map(sort_par).collect())
}

/// Sort a list of `(key, value)` pairs by canonical key score (the Scala `Map[Par,Par].sort`).
pub fn sort_pairs(pairs: Vec<(Par, Par)>) -> Vec<(Par, Par)> {
    sort_scored(
        pairs
            .iter()
            .map(|(k, v)| {
                let sk = sort_par(k);
                let sv = sort_par(v);
                ScoredTerm {
                    term: (sk.term, sv.term),
                    score: sk.score,
                }
            })
            .collect(),
    )
}

/// Sort `(ReceiveBind, T)` pairs by the bind's canonical sort score (port of
/// `ReceiveBindsSortMatcher.preSortBinds`).
pub fn sort_receive_binds_with<T>(binds: Vec<(ReceiveBind, T)>) -> Vec<(ReceiveBind, T)> {
    let scored: Vec<ScoredTerm<(ReceiveBind, T)>> = binds
        .into_iter()
        .map(|(bind, tag)| {
            let sb = sort_receive_bind(&bind);
            ScoredTerm {
                term: (sb.term, tag),
                score: sb.score,
            }
        })
        .collect();
    sort_scored(scored)
}

/// Construct a `ParSet`, deduplicating (by raw equality) then sorting (the Scala `ParSet.apply`).
pub fn par_set(ps: Vec<Par>) -> ParSet {
    let mut deduped: Vec<Par> = Vec::new();
    for p in ps {
        if !deduped.contains(&p) {
            deduped.push(p);
        }
    }
    ParSet {
        ps: sort_pars(deduped),
        connective_used: false,
        locally_free: AlwaysEqual(BitSet::new()),
        remainder: None,
    }
}

/// Construct a `ParMap`, deduplicating by key (last-write-wins) then sorting by key (the Scala `ParMap.apply`).
pub fn par_map(kvs: Vec<(Par, Par)>) -> ParMap {
    let mut map: Vec<(Par, Par)> = Vec::new();
    for (k, v) in kvs {
        if let Some(slot) = map.iter_mut().find(|(ek, _)| ek == &k) {
            slot.1 = v;
        } else {
            map.push((k, v));
        }
    }
    ParMap {
        kvs: sort_pairs(map),
        connective_used: false,
        locally_free: AlwaysEqual(BitSet::new()),
        remainder: None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;

    fn par() -> Par {
        Par::default()
    }

    fn g_int(i: i64) -> Par {
        Par {
            exprs: vec![Expr::GInt(i)],
            ..Default::default()
        }
    }

    fn g_bool(b: bool) -> Par {
        Par {
            exprs: vec![Expr::GBool(b)],
            ..Default::default()
        }
    }

    fn expr(e: Expr) -> Par {
        Par {
            exprs: vec![e],
            ..Default::default()
        }
    }

    fn send(chan: Par, data: Vec<Par>, persistent: bool) -> Send {
        Send {
            chan: Box::new(chan.quote()),
            data: data.into_iter().map(|d| d.quote()).collect(),
            persistent,
            locally_free: AlwaysEqual(BitSet::new()),
            connective_used: false,
        }
    }

    fn receive(binds: Vec<ReceiveBind>, body: Par, persistent: bool, peek: bool) -> Receive {
        Receive {
            binds,
            body: Box::new(body),
            persistent,
            peek,
            bind_count: 0,
            locally_free: AlwaysEqual(BitSet::new()),
            connective_used: false,
        }
    }

    // --- Oracle cases from SortTest.scala ------------------------------------------------------

    #[test]
    fn smaller_integers_come_first() {
        let par_ground: Par = Par {
            exprs: vec![
                Expr::GInt(2),
                Expr::GInt(1),
                Expr::GInt(-1),
                Expr::GInt(-2),
                Expr::GInt(0),
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            exprs: vec![
                Expr::GInt(-2),
                Expr::GInt(-1),
                Expr::GInt(0),
                Expr::GInt(1),
                Expr::GInt(2),
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_ground), expected);
    }

    #[test]
    fn smaller_bigint_values_come_first() {
        let big = |s: &str| BigInt::parse_bytes(s.as_bytes(), 10).unwrap();
        let par_ground: Par = Par {
            exprs: vec![
                Expr::GBigInt(BigInt::from(2)),
                Expr::GBigInt(big("9999999999999999999999999999999999999999999999")),
                Expr::GBigInt(big("-9999999999999999999999999999999999999999999999")),
                Expr::GBigInt(BigInt::from(-2)),
                Expr::GBigInt(BigInt::from(0)),
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            exprs: vec![
                Expr::GBigInt(big("-9999999999999999999999999999999999999999999999")),
                Expr::GBigInt(BigInt::from(-2)),
                Expr::GBigInt(BigInt::from(0)),
                Expr::GBigInt(BigInt::from(2)),
                Expr::GBigInt(big("9999999999999999999999999999999999999999999999")),
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_ground), expected);
    }

    #[test]
    fn sort_order_of_ground_types() {
        let byte_array = Expr::GByteArray(vec![0x80]);
        let par_ground: Par = Par {
            exprs: vec![
                byte_array.clone(),
                Expr::GBigInt(BigInt::from(0)),
                Expr::GUri("https://www.rchain.coop/".to_string()),
                Expr::GInt(47),
                Expr::GString("Hello".to_string()),
                Expr::GBool(true),
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            exprs: vec![
                Expr::GBool(true),
                Expr::GInt(47),
                Expr::GString("Hello".to_string()),
                Expr::GUri("https://www.rchain.coop/".to_string()),
                Expr::GBigInt(BigInt::from(0)),
                byte_array,
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_ground), expected);
    }

    #[test]
    fn sort_and_deduplicate_sets() {
        let par_ground = par_set(vec![
            g_int(2),
            g_int(1),
            par_set(vec![g_int(1), g_int(2)]).into_expr(),
            par_set(vec![g_int(1), g_int(1)]).into_expr(),
        ]);
        let expected = par_set(vec![
            g_int(1),
            g_int(2),
            par_set(vec![g_int(1)]).into_expr(),
            par_set(vec![g_int(1), g_int(2)]).into_expr(),
        ]);
        assert_eq!(
            sort_par_term(&par_ground.into_expr()),
            expected.into_expr()
        );
    }

    #[test]
    fn sort_map_by_key_last_write_wins() {
        let par_ground = par_map(vec![
            (g_int(2), par_set(vec![g_int(2), g_int(1)]).into_expr()),
            (g_int(2), g_int(1)),
            (g_int(1), g_int(1)),
        ]);
        let expected = par_map(vec![(g_int(1), g_int(1)), (g_int(2), g_int(1))]);
        assert_eq!(
            sort_par_term(&par_ground.into_expr()),
            expected.into_expr()
        );
    }

    #[test]
    fn use_sorted_subtrees_and_their_scores() {
        let s1 = send(par(), vec![], false);
        let s2 = send(
            Par {
                receives: vec![receive(vec![], par(), false, false)],
                ..Default::default()
            },
            vec![],
            false,
        );
        let p21: Par = Par {
            sends: vec![s2.clone(), s1.clone()],
            ..Default::default()
        };
        let p12: Par = Par {
            sends: vec![s1, s2],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&p12), sort_par_term(&p21));
    }

    #[test]
    fn keep_order_when_adding_numbers() {
        let par_expr = expr(Expr::EPlus(
            Box::new(expr(Expr::EPlus(Box::new(g_int(1)), Box::new(g_int(3))))),
            Box::new(g_int(2)),
        ));
        assert_eq!(sort_par_term(&par_expr), par_expr);
    }

    #[test]
    fn sort_according_to_pemdas() {
        let par_expr: Par = Par {
            exprs: vec![
                Expr::EMinus(Box::new(g_int(4)), Box::new(g_int(3))),
                Expr::EDiv(Box::new(g_int(1)), Box::new(g_int(5))),
                Expr::EPlus(Box::new(g_int(1)), Box::new(g_int(3))),
                Expr::EMult(Box::new(g_int(6)), Box::new(g_int(3))),
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            exprs: vec![
                Expr::EMult(Box::new(g_int(6)), Box::new(g_int(3))),
                Expr::EDiv(Box::new(g_int(1)), Box::new(g_int(5))),
                Expr::EPlus(Box::new(g_int(1)), Box::new(g_int(3))),
                Expr::EMinus(Box::new(g_int(4)), Box::new(g_int(3))),
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_expr), expected);
    }

    #[test]
    fn sort_comparisons_in_order() {
        let par_expr: Par = Par {
            exprs: vec![
                Expr::EEq(Box::new(g_int(4)), Box::new(g_int(3))),
                Expr::ENeq(Box::new(g_int(1)), Box::new(g_int(5))),
                Expr::ELt(Box::new(g_int(1)), Box::new(g_int(5))),
                Expr::EGt(Box::new(g_bool(false)), Box::new(g_bool(true))),
                Expr::ELte(Box::new(g_int(1)), Box::new(g_int(5))),
                Expr::EGte(Box::new(g_bool(false)), Box::new(g_bool(true))),
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            exprs: vec![
                Expr::ELt(Box::new(g_int(1)), Box::new(g_int(5))),
                Expr::ELte(Box::new(g_int(1)), Box::new(g_int(5))),
                Expr::EGt(Box::new(g_bool(false)), Box::new(g_bool(true))),
                Expr::EGte(Box::new(g_bool(false)), Box::new(g_bool(true))),
                Expr::EEq(Box::new(g_int(4)), Box::new(g_int(3))),
                Expr::ENeq(Box::new(g_int(1)), Box::new(g_int(5))),
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_expr), expected);
    }

    #[test]
    fn sort_evars_based_on_type_and_levels() {
        let par_ground: Par = Par {
            exprs: vec![
                Expr::EVar(Box::new(Var::FreeVar(2))),
                Expr::EVar(Box::new(Var::FreeVar(1))),
                Expr::EVar(Box::new(Var::BoundVar(2))),
                Expr::EVar(Box::new(Var::BoundVar(1))),
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            exprs: vec![
                Expr::EVar(Box::new(Var::BoundVar(1))),
                Expr::EVar(Box::new(Var::BoundVar(2))),
                Expr::EVar(Box::new(Var::FreeVar(1))),
                Expr::EVar(Box::new(Var::FreeVar(2))),
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_ground), expected);
    }

    #[test]
    fn sort_exprs_in_order_of_ground_vars_arithmetic_comparisons_logical() {
        let par_expr: Par = Par {
            exprs: vec![
                Expr::EEq(Box::new(g_int(4)), Box::new(g_int(3))),
                Expr::EDiv(Box::new(g_int(1)), Box::new(g_int(5))),
                Expr::EVar(Box::new(Var::BoundVar(1))),
                Expr::EOr(Box::new(g_bool(false)), Box::new(g_bool(true))),
                Expr::GInt(1),
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            exprs: vec![
                Expr::GInt(1),
                Expr::EVar(Box::new(Var::BoundVar(1))),
                Expr::EDiv(Box::new(g_int(1)), Box::new(g_int(5))),
                Expr::EEq(Box::new(g_int(4)), Box::new(g_int(3))),
                Expr::EOr(Box::new(g_bool(false)), Box::new(g_bool(true))),
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_expr), expected);
    }

    #[test]
    fn sort_sends_based_on_persistence_channel_data() {
        let par_expr: Par = Par {
            sends: vec![
                send(g_int(5), vec![g_int(3)], false),
                send(g_int(5), vec![g_int(3)], true),
                send(g_int(4), vec![g_int(2)], false),
                send(g_int(5), vec![g_int(2)], false),
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            sends: vec![
                send(g_int(4), vec![g_int(2)], false),
                send(g_int(5), vec![g_int(2)], false),
                send(g_int(5), vec![g_int(3)], false),
                send(g_int(5), vec![g_int(3)], true),
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_expr), expected);
    }

    #[test]
    fn sort_news_based_on_bindcount_uris_and_body() {
        let new = |bind_count: i32, uri: &[&str], p: Par| New {
            bind_count,
            uri: uri.iter().map(|s| s.to_string()).collect(),
            p: Box::new(p),
            ..New::default()
        };
        let par_new: Par = Par {
            news: vec![
                new(2, &[], par()),
                new(2, &["rho:io:stderr"], par()),
                new(2, &["rho:io:stdout"], par()),
                new(1, &[], par()),
                new(2, &["rho:io:stdout"], g_int(7)),
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            news: vec![
                new(1, &[], par()),
                new(2, &[], par()),
                new(2, &["rho:io:stderr"], par()),
                new(2, &["rho:io:stdout"], par()),
                new(2, &["rho:io:stdout"], g_int(7)),
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_new), expected);
    }

    #[test]
    fn sort_uris_in_news() {
        let par_new: Par = Par {
            news: vec![New {
                bind_count: 1,
                uri: vec!["rho:io:stdout".to_string(), "rho:io:stderr".to_string()],
                ..New::default()
            }],
            ..Default::default()
        };
        let expected: Par = Par {
            news: vec![New {
                bind_count: 1,
                uri: vec!["rho:io:stderr".to_string(), "rho:io:stdout".to_string()],
                ..New::default()
            }],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_new), expected);
    }

    #[test]
    fn sort_matches_based_on_value_and_cases() {
        let match_case = |p: i64, s: i64| MatchCase {
            pattern: Box::new(g_int(p).quote()),
            source: Box::new(g_int(s)),
            free_count: crate::types::FreeCount::ZERO,
        };
        let par_match: Par = Par {
            matches: vec![
                Match {
                    target: Box::new(g_int(5).quote()),
                    cases: vec![match_case(5, 5), match_case(4, 4)],
                    ..Match::default()
                },
                Match {
                    target: Box::new(g_bool(true).quote()),
                    cases: vec![match_case(5, 5), match_case(4, 4)],
                    ..Match::default()
                },
                Match {
                    target: Box::new(g_bool(true).quote()),
                    cases: vec![match_case(4, 4), match_case(3, 3)],
                    ..Match::default()
                },
            ],
            ..Default::default()
        };
        let expected: Par = Par {
            matches: vec![
                Match {
                    target: Box::new(g_bool(true).quote()),
                    cases: vec![match_case(4, 4), match_case(3, 3)],
                    ..Match::default()
                },
                Match {
                    target: Box::new(g_bool(true).quote()),
                    cases: vec![match_case(5, 5), match_case(4, 4)],
                    ..Match::default()
                },
                Match {
                    target: Box::new(g_int(5).quote()),
                    cases: vec![match_case(5, 5), match_case(4, 4)],
                    ..Match::default()
                },
            ],
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_match), expected);
    }

    #[test]
    fn score_in_different_bytestring_is_unequal() {
        let a = expr(Expr::GByteArray(vec![0x80]));
        let b = expr(Expr::GByteArray(vec![0xd9]));
        assert_ne!(sort_par(&a).score, sort_par(&b).score);
    }

    #[test]
    fn sort_logical_connectives_in_not_and_or_order() {
        let evar = |lvl| Expr::EVar(Box::new(Var::FreeVar(lvl))).into_par();
        let par_expr: Par = Par {
            connectives: vec![
                Connective::ConnAnd(ConnectiveBody {
                    ps: vec![evar(0), Par {
                        sends: vec![send(evar(1), vec![evar(2)], false)],
                        ..Default::default()
                    }],
                }),
                Connective::ConnOr(ConnectiveBody {
                    ps: vec![
                        wildcard_new(1),
                        wildcard_new(2),
                    ],
                }),
                Connective::ConnNot(Box::new(par())),
            ],
            connective_used: true,
            ..Default::default()
        };
        let expected: Par = Par {
            connectives: vec![
                Connective::ConnNot(Box::new(par())),
                Connective::ConnAnd(ConnectiveBody {
                    ps: vec![evar(0), Par {
                        sends: vec![send(evar(1), vec![evar(2)], false)],
                        ..Default::default()
                    }],
                }),
                Connective::ConnOr(ConnectiveBody {
                    ps: vec![
                        wildcard_new(1),
                        wildcard_new(2),
                    ],
                }),
            ],
            connective_used: true,
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_expr), expected);
    }

    fn wildcard_new(bind_count: i32) -> Par {
        Par {
            news: vec![New {
                bind_count,
                p: Box::new(Expr::EVar(Box::new(Var::Wildcard)).into_par()),
                ..New::default()
            }],
            ..Default::default()
        }
    }

    #[test]
    fn sort_logical_connectives_varref_bool_int_string_uri_bytearray_bigint() {
        let par_expr: Par = Par {
            connectives: vec![
                Connective::ConnByteArray(true),
                Connective::ConnUri(true),
                Connective::ConnString(true),
                Connective::ConnBigInt(true),
                Connective::ConnInt(true),
                Connective::ConnBool(true),
                Connective::VarRef(VarRef::default()),
            ],
            connective_used: true,
            ..Default::default()
        };
        let expected: Par = Par {
            connectives: vec![
                Connective::VarRef(VarRef::default()),
                Connective::ConnBool(true),
                Connective::ConnInt(true),
                Connective::ConnString(true),
                Connective::ConnUri(true),
                Connective::ConnByteArray(true),
                Connective::ConnBigInt(true),
            ],
            connective_used: true,
            ..Default::default()
        };
        assert_eq!(sort_par_term(&par_expr), expected);
    }

    #[test]
    fn unequal_new_have_unequal_scores() {
        let new1 = New {
            bind_count: 1,
            injections: BTreeMap::from([(
                "".to_string(),
                Par {
                    bundles: vec![Bundle {
                        body: Box::new(par()),
                        write_flag: true,
                        read_flag: false,
                    }],
                    ..Default::default()
                },
            )]),
            ..New::default()
        };
        let new2 = New {
            bind_count: 1,
            ..New::default()
        };
        assert_ne!(new1, new2);
        assert_ne!(sort_new(&new1).score, sort_new(&new2).score);
    }

    #[test]
    fn unequal_emethod_have_unequal_scores() {
        let method1 = Expr::EMethod(EMethod {
            connective_used: true,
            ..EMethod::default()
        });
        let method2 = Expr::EMethod(EMethod {
            connective_used: false,
            ..EMethod::default()
        });
        assert_ne!(sort_expr(&method1).score, sort_expr(&method2).score);
    }

    // --- Law 1 property tests --------------------------------------------------------------------

    #[test]
    fn sort_is_idempotent() {
        let p: Par = Par {
            exprs: vec![
                Expr::EPlus(Box::new(g_int(3)), Box::new(g_int(1))),
                Expr::GInt(7),
                Expr::EVar(Box::new(Var::BoundVar(0))),
            ],
            sends: vec![
                send(g_int(2), vec![g_int(1)], false),
                send(g_int(1), vec![g_int(2)], true),
            ],
            ..Default::default()
        };
        let once = sort_par_term(&p);
        let twice = sort_par_term(&once);
        assert_eq!(once, twice);
    }

    #[test]
    fn sort_par_merge_commutes() {
        let p: Par = Par {
            exprs: vec![Expr::GInt(2), Expr::GInt(1)],
            ..Default::default()
        };
        let q: Par = Par {
            exprs: vec![Expr::GInt(4), Expr::GInt(3)],
            ..Default::default()
        };
        let merged = p.par_merge(&q);
        let merged_rev = q.par_merge(&p);
        assert_eq!(sort_par_term(&merged), sort_par_term(&merged_rev));
    }
}

#[cfg(test)]
trait IntoPar {
    fn into_par(self) -> Par;
}

#[cfg(test)]
impl IntoPar for Expr {
    fn into_par(self) -> Par {
        Par {
            exprs: vec![self],
            ..Default::default()
        }
    }
}

#[cfg(test)]
impl ParSet {
    fn into_expr(self) -> Par {
        Par {
            exprs: vec![Expr::ESet(self)],
            ..Default::default()
        }
    }
}

#[cfg(test)]
impl ParMap {
    fn into_expr(self) -> Par {
        Par {
            exprs: vec![Expr::EMap(self)],
            ..Default::default()
        }
    }
}
