//! Wire bridge between the hand-written rholang AST and the generated `RhoTypes.proto` types.
//!
//! This is the foundation of `Serialize[Par]` (Law 16 content addressing / rspace channel hashing).
//! The three custom scalapb `TypeMapper` encodings are reproduced bit-for-bit:
//! - `locallyFree` (a `scala.collection.immutable.BitSet`) → little-endian `Long` bit-mask with
//!   trailing zero bytes stripped.
//! - `g_big_int` (`scala.math.BigInt`) → signed big-endian two's-complement bytes.
//! - `random_state` (`Blake2b512Random`) → its 128-byte state.

use num_bigint::BigInt;
use prost::Message as _;
use rchain_crypto::hash::blake2b512_random::{Blake2b512Random, SerializedRandom};
use rchain_shared::serialize::Serialize;

use crate::ast as a;
use crate::errors::ModelsError;
use crate::proto::rholang as p;
use crate::types::FreeCount;
use crate::runtime::{BindPattern, ListParWithRandom, ParWithRandom, TaggedContinuation};
use crate::sorted::SortedProc;

// --- TypeMapper encodings -----------------------------------------------------------------------

/// Serialize a `BitSet` (`Vec<i32>` of set positions) to bytes (port of `bitSetToByteString`).
pub fn bitset_to_bytes(bitset: &[i32]) -> Vec<u8> {
    let max = bitset.iter().map(|e| *e as usize).max().unwrap_or(0);
    let num_words = if max == 0 && bitset.is_empty() {
        0
    } else {
        max / 64 + 1
    };
    let mut words = vec![0u64; num_words];
    for &e in bitset {
        let e = e as usize;
        words[e / 64] |= 1u64 << (e % 64);
    }
    let mut bytes = Vec::with_capacity(num_words * 8);
    for w in &words {
        bytes.extend_from_slice(&w.to_le_bytes());
    }
    while bytes.last() == Some(&0) {
        bytes.pop();
    }
    bytes
}

/// Deserialize bytes to a `BitSet` (port of `byteStringToBitSet`).
pub fn bytes_to_bitset(bytes: &[u8]) -> Vec<i32> {
    let buffer_size = (bytes.len() + 7) / 8 * 8;
    let mut padded = vec![0u8; buffer_size];
    padded[..bytes.len()].copy_from_slice(bytes);
    let mut out = Vec::new();
    for (word_idx, chunk) in padded.chunks(8).enumerate() {
        let mut arr = [0u8; 8];
        arr.copy_from_slice(chunk);
        let word = u64::from_le_bytes(arr);
        for bit in 0..64 {
            if (word >> bit) & 1 == 1 {
                out.push((word_idx * 64 + bit) as i32);
            }
        }
    }
    out
}

// --- Var ----------------------------------------------------------------------------------------

pub fn var_to_proto(v: &a::Var) -> p::Var {
    let var_instance = match v {
        a::Var::BoundVar(i) => p::var::VarInstance::BoundVar(*i),
        a::Var::FreeVar(i) => p::var::VarInstance::FreeVar(*i),
        a::Var::Wildcard => p::var::VarInstance::Wildcard(p::var::WildcardMsg {}),
        a::Var::Empty => return p::Var { var_instance: None },
    };
    p::Var {
        var_instance: Some(var_instance),
    }
}

pub fn var_from_proto(p: &p::Var) -> a::Var {
    match &p.var_instance {
        Some(p::var::VarInstance::BoundVar(i)) => a::Var::BoundVar(*i),
        Some(p::var::VarInstance::FreeVar(i)) => a::Var::FreeVar(*i),
        Some(p::var::VarInstance::Wildcard(_)) => a::Var::Wildcard,
        None => a::Var::Empty,
    }
}

// --- Connective ---------------------------------------------------------------------------------

pub fn connective_to_proto(c: &a::Connective) -> p::Connective {
    let inst = match c {
        a::Connective::ConnAnd(b) => {
            p::connective::ConnectiveInstance::ConnAndBody(connective_body_to_proto(b))
        }
        a::Connective::ConnOr(b) => {
            p::connective::ConnectiveInstance::ConnOrBody(connective_body_to_proto(b))
        }
        a::Connective::ConnNot(p_) => p::connective::ConnectiveInstance::ConnNotBody(par_to_proto(p_)),
        a::Connective::VarRef(v) => p::connective::ConnectiveInstance::VarRefBody(p::VarRef {
            index: v.index,
            depth: v.depth,
        }),
        a::Connective::ConnBool(b) => p::connective::ConnectiveInstance::ConnBool(*b),
        a::Connective::ConnInt(b) => p::connective::ConnectiveInstance::ConnInt(*b),
        a::Connective::ConnBigInt(b) => p::connective::ConnectiveInstance::ConnBigInt(*b),
        a::Connective::ConnString(b) => p::connective::ConnectiveInstance::ConnString(*b),
        a::Connective::ConnUri(b) => p::connective::ConnectiveInstance::ConnUri(*b),
        a::Connective::ConnByteArray(b) => p::connective::ConnectiveInstance::ConnByteArray(*b),
        a::Connective::Empty => return p::Connective { connective_instance: None },
    };
    p::Connective {
        connective_instance: Some(inst),
    }
}

fn connective_body_to_proto(b: &a::ConnectiveBody) -> p::ConnectiveBody {
    p::ConnectiveBody {
        ps: b.ps.iter().map(par_to_proto).collect(),
    }
}

pub fn connective_from_proto(p: &p::Connective) -> Result<a::Connective, ModelsError> {
    Ok(match &p.connective_instance {
        Some(p::connective::ConnectiveInstance::ConnAndBody(b)) => {
            a::Connective::ConnAnd(a::ConnectiveBody {
                ps: b.ps.iter().map(par_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
            })
        }
        Some(p::connective::ConnectiveInstance::ConnOrBody(b)) => {
            a::Connective::ConnOr(a::ConnectiveBody {
                ps: b.ps.iter().map(par_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
            })
        }
        Some(p::connective::ConnectiveInstance::ConnNotBody(p_)) => {
            a::Connective::ConnNot(Box::new(par_from_proto(p_)?))
        }
        Some(p::connective::ConnectiveInstance::VarRefBody(v)) => {
            a::Connective::VarRef(a::VarRef { index: v.index, depth: v.depth })
        }
        Some(p::connective::ConnectiveInstance::ConnBool(b)) => a::Connective::ConnBool(*b),
        Some(p::connective::ConnectiveInstance::ConnInt(b)) => a::Connective::ConnInt(*b),
        Some(p::connective::ConnectiveInstance::ConnBigInt(b)) => a::Connective::ConnBigInt(*b),
        Some(p::connective::ConnectiveInstance::ConnString(b)) => a::Connective::ConnString(*b),
        Some(p::connective::ConnectiveInstance::ConnUri(b)) => a::Connective::ConnUri(*b),
        Some(p::connective::ConnectiveInstance::ConnByteArray(b)) => a::Connective::ConnByteArray(*b),
        None => a::Connective::Empty,
    })
}

// --- GUnforgeable -------------------------------------------------------------------------------

pub fn unforgeable_to_proto(u: &a::GUnforgeable) -> p::GUnforgeable {
    let inst = match u {
        a::GUnforgeable::GPrivate(g) => {
            p::g_unforgeable::UnfInstance::GPrivateBody(p::GPrivate { id: g.id.clone() })
        }
        a::GUnforgeable::GDeployId(g) => {
            p::g_unforgeable::UnfInstance::GDeployIdBody(p::GDeployId { sig: g.sig.clone() })
        }
        a::GUnforgeable::GDeployerId(g) => {
            p::g_unforgeable::UnfInstance::GDeployerIdBody(p::GDeployerId {
                public_key: g.public_key.clone(),
            })
        }
        a::GUnforgeable::GSysAuthToken => {
            p::g_unforgeable::UnfInstance::GSysAuthTokenBody(p::GSysAuthToken {})
        }
        a::GUnforgeable::Empty => return p::GUnforgeable { unf_instance: None },
    };
    p::GUnforgeable {
        unf_instance: Some(inst),
    }
}

pub fn unforgeable_from_proto(p: &p::GUnforgeable) -> Result<a::GUnforgeable, ModelsError> {
    Ok(match &p.unf_instance {
        Some(p::g_unforgeable::UnfInstance::GPrivateBody(g)) => {
            a::GUnforgeable::GPrivate(a::GPrivate { id: g.id.clone() })
        }
        Some(p::g_unforgeable::UnfInstance::GDeployIdBody(g)) => {
            a::GUnforgeable::GDeployId(a::GDeployId { sig: g.sig.clone() })
        }
        Some(p::g_unforgeable::UnfInstance::GDeployerIdBody(g)) => {
            a::GUnforgeable::GDeployerId(a::GDeployerId {
                public_key: g.public_key.clone(),
            })
        }
        Some(p::g_unforgeable::UnfInstance::GSysAuthTokenBody(_)) => a::GUnforgeable::GSysAuthToken,
        None => a::GUnforgeable::Empty,
    })
}

// --- Expr ---------------------------------------------------------------------------------------

pub fn expr_to_proto(e: &a::Expr) -> p::Expr {
    use p::expr::ExprInstance as E;
    let inst = match e {
        a::Expr::GBool(b) => E::GBool(*b),
        a::Expr::GInt(i) => E::GInt(*i),
        a::Expr::GBigInt(bi) => E::GBigInt(bi.to_signed_bytes_be()),
        a::Expr::GString(s) => E::GString(s.clone()),
        a::Expr::GUri(u) => E::GUri(u.clone()),
        a::Expr::GByteArray(b) => E::GByteArray(b.clone()),
        a::Expr::ENot(p_) => E::ENotBody(p::ENot { p: Some(par_to_proto(p_)) }),
        a::Expr::ENeg(p_) => E::ENegBody(p::ENeg { p: Some(par_to_proto(p_)) }),
        a::Expr::EMult(p1, p2) => E::EMultBody(p::EMult { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EDiv(p1, p2) => E::EDivBody(p::EDiv { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EMod(p1, p2) => E::EModBody(p::EMod { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EPlus(p1, p2) => E::EPlusBody(p::EPlus { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EMinus(p1, p2) => E::EMinusBody(p::EMinus { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::ELt(p1, p2) => E::ELtBody(p::ELt { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::ELte(p1, p2) => E::ELteBody(p::ELte { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EGt(p1, p2) => E::EGtBody(p::EGt { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EGte(p1, p2) => E::EGteBody(p::EGte { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EEq(p1, p2) => E::EEqBody(p::EEq { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::ENeq(p1, p2) => E::ENeqBody(p::ENeq { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EAnd(p1, p2) => E::EAndBody(p::EAnd { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EOr(p1, p2) => E::EOrBody(p::EOr { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EShortAnd(p1, p2) => E::EShortAndBody(p::EShortAnd { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EShortOr(p1, p2) => E::EShortOrBody(p::EShortOr { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EMatches(t, pat) => E::EMatchesBody(p::EMatches { target: Some(par_to_proto(t)), pattern: Some(par_to_proto(pat)) }),
        a::Expr::EPercentPercent(p1, p2) => E::EPercentPercentBody(p::EPercentPercent { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EPlusPlus(p1, p2) => E::EPlusPlusBody(p::EPlusPlus { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EMinusMinus(p1, p2) => E::EMinusMinusBody(p::EMinusMinus { p1: Some(par_to_proto(p1)), p2: Some(par_to_proto(p2)) }),
        a::Expr::EVar(v) => E::EVarBody(p::EVar { v: Some(var_to_proto(v)) }),
        a::Expr::EList(el) => E::EListBody(elist_to_proto(el)),
        a::Expr::ETuple(et) => E::ETupleBody(etuple_to_proto(et)),
        a::Expr::ESet(es) => E::ESetBody(eset_to_proto(es)),
        a::Expr::EMap(em) => E::EMapBody(emap_to_proto(em)),
        a::Expr::EMethod(em) => E::EMethodBody(emethod_to_proto(em)),
    };
    p::Expr {
        expr_instance: Some(inst),
    }
}

pub fn expr_from_proto(p: &p::Expr) -> Result<a::Expr, ModelsError> {
    use p::expr::ExprInstance as E;
    Ok(match &p.expr_instance {
        Some(E::GBool(b)) => a::Expr::GBool(*b),
        Some(E::GInt(i)) => a::Expr::GInt(*i),
        Some(E::GBigInt(b)) => a::Expr::GBigInt(BigInt::from_signed_bytes_be(b)),
        Some(E::GString(s)) => a::Expr::GString(s.clone()),
        Some(E::GUri(u)) => a::Expr::GUri(u.clone()),
        Some(E::GByteArray(b)) => a::Expr::GByteArray(b.clone()),
        Some(E::ENotBody(m)) => a::Expr::ENot(Box::new(par_from_proto(m.p.as_ref().ok_or(ModelsError::Malformed("p"))?)?)),
        Some(E::ENegBody(m)) => a::Expr::ENeg(Box::new(par_from_proto(m.p.as_ref().ok_or(ModelsError::Malformed("p"))?)?)),
        Some(E::EMultBody(m)) => a::Expr::EMult(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EDivBody(m)) => a::Expr::EDiv(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EModBody(m)) => a::Expr::EMod(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EPlusBody(m)) => a::Expr::EPlus(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EMinusBody(m)) => a::Expr::EMinus(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::ELtBody(m)) => a::Expr::ELt(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::ELteBody(m)) => a::Expr::ELte(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EGtBody(m)) => a::Expr::EGt(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EGteBody(m)) => a::Expr::EGte(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EEqBody(m)) => a::Expr::EEq(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::ENeqBody(m)) => a::Expr::ENeq(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EAndBody(m)) => a::Expr::EAnd(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EOrBody(m)) => a::Expr::EOr(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EShortAndBody(m)) => a::Expr::EShortAnd(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EShortOrBody(m)) => a::Expr::EShortOr(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EMatchesBody(m)) => a::Expr::EMatches(Box::new(par_from_proto(m.target.as_ref().ok_or(ModelsError::Malformed("target"))?)?), Box::new(par_from_proto(m.pattern.as_ref().ok_or(ModelsError::Malformed("pattern"))?)?)),
        Some(E::EPercentPercentBody(m)) => a::Expr::EPercentPercent(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EPlusPlusBody(m)) => a::Expr::EPlusPlus(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EMinusMinusBody(m)) => a::Expr::EMinusMinus(Box::new(par_from_proto(m.p1.as_ref().ok_or(ModelsError::Malformed("p1"))?)?), Box::new(par_from_proto(m.p2.as_ref().ok_or(ModelsError::Malformed("p2"))?)?)),
        Some(E::EVarBody(m)) => a::Expr::EVar(Box::new(var_from_proto(m.v.as_ref().ok_or(ModelsError::Malformed("v"))?))),
        Some(E::EListBody(m)) => a::Expr::EList(elist_from_proto(m)?),
        Some(E::ETupleBody(m)) => a::Expr::ETuple(etuple_from_proto(m)?),
        Some(E::ESetBody(m)) => a::Expr::ESet(eset_from_proto(m)?),
        Some(E::EMapBody(m)) => a::Expr::EMap(emap_from_proto(m)?),
        Some(E::EMethodBody(m)) => a::Expr::EMethod(emethod_from_proto(m)?),
        None => a::Expr::GBool(false),
    })
}

// --- Collection helpers -------------------------------------------------------------------------

fn elist_to_proto(el: &a::EList) -> p::EList {
    p::EList {
        ps: el.ps.iter().map(par_to_proto).collect(),
        locally_free: bitset_to_bytes(&el.locally_free.0),
        connective_used: el.connective_used,
        remainder: el.remainder.as_deref().map(var_to_proto),
    }
}
fn elist_from_proto(p: &p::EList) -> Result<a::EList, ModelsError> {Ok(
    a::EList {
        ps: p.ps.iter().map(par_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
        connective_used: p.connective_used,
        remainder: p.remainder.as_ref().map(var_from_proto).map(Box::new),
    }
)}

fn etuple_to_proto(et: &a::ETuple) -> p::ETuple {
    p::ETuple {
        ps: et.ps.iter().map(par_to_proto).collect(),
        locally_free: bitset_to_bytes(&et.locally_free.0),
        connective_used: et.connective_used,
    }
}
fn etuple_from_proto(p: &p::ETuple) -> Result<a::ETuple, ModelsError> {Ok(
    a::ETuple {
        ps: p.ps.iter().map(par_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
        connective_used: p.connective_used,
    }
)}

fn eset_to_proto(es: &a::ParSet) -> p::ESet {
    p::ESet {
        ps: es.ps.iter().map(par_to_proto).collect(),
        locally_free: bitset_to_bytes(&es.locally_free.0),
        connective_used: es.connective_used,
        remainder: es.remainder.as_deref().map(var_to_proto),
    }
}
fn eset_from_proto(p: &p::ESet) -> Result<a::ParSet, ModelsError> {Ok(
    a::ParSet {
        ps: p.ps.iter().map(par_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
        connective_used: p.connective_used,
        remainder: p.remainder.as_ref().map(var_from_proto).map(Box::new),
    }
)}

fn emap_to_proto(em: &a::ParMap) -> p::EMap {
    p::EMap {
        kvs: em
            .kvs
            .iter()
            .map(|(k, v)| p::KeyValuePair {
                key: Some(par_to_proto(k)),
                value: Some(par_to_proto(v)),
            })
            .collect(),
        locally_free: bitset_to_bytes(&em.locally_free.0),
        connective_used: em.connective_used,
        remainder: em.remainder.as_deref().map(var_to_proto),
    }
}
fn emap_from_proto(p: &p::EMap) -> Result<a::ParMap, ModelsError> {Ok(
    a::ParMap {
        kvs: p
            .kvs
            .iter()
            .map(|kv| {
                Ok((
                    par_from_proto(kv.key.as_ref().ok_or(ModelsError::Malformed("key"))?)?,
                    par_from_proto(kv.value.as_ref().ok_or(ModelsError::Malformed("value"))?)?,
                ))
            })
            .collect::<Result<Vec<_>, ModelsError>>()?,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
        connective_used: p.connective_used,
        remainder: p.remainder.as_ref().map(var_from_proto).map(Box::new),
    }
)}

fn emethod_to_proto(em: &a::EMethod) -> p::EMethod {
    p::EMethod {
        method_name: em.method_name.clone(),
        target: Some(par_to_proto(&em.target)),
        arguments: em.arguments.iter().map(par_to_proto).collect(),
        locally_free: bitset_to_bytes(&em.locally_free.0),
        connective_used: em.connective_used,
    }
}
fn emethod_from_proto(p: &p::EMethod) -> Result<a::EMethod, ModelsError> {Ok(
    a::EMethod {
        method_name: p.method_name.clone(),
        target: Box::new(par_from_proto(p.target.as_ref().ok_or(ModelsError::Malformed("target"))?)?),
        arguments: p.arguments.iter().map(|x| par_from_proto(x)).collect::<Result<Vec<_>, ModelsError>>()?,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
        connective_used: p.connective_used,
    }
)}

// --- Terms --------------------------------------------------------------------------------------

pub fn send_to_proto(s: &a::Send) -> p::Send {
    p::Send {
        chan: Some(par_to_proto(s.chan.as_ref())),
        data: s.data.iter().map(|d| par_to_proto(d)).collect(),
        persistent: s.persistent,
        locally_free: bitset_to_bytes(&s.locally_free.0),
        connective_used: s.connective_used,
    }
}
pub fn send_from_proto(p: &p::Send) -> Result<a::Send, ModelsError> {Ok(
    a::Send {
        chan: Box::new(par_from_proto::<a::NameSort>(p.chan.as_ref().ok_or(ModelsError::Malformed("chan"))?)?),
        data: p.data.iter().map(|d| par_from_proto::<a::NameSort>(d)).collect::<Result<Vec<_>, ModelsError>>()?,
        persistent: p.persistent,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
        connective_used: p.connective_used,
    }
)}

pub fn receive_to_proto(r: &a::Receive) -> p::Receive {
    p::Receive {
        binds: r.binds.iter().map(receive_bind_to_proto).collect(),
        body: Some(par_to_proto(r.body.as_ref())),
        persistent: r.persistent,
        peek: r.peek,
        bind_count: r.bind_count,
        locally_free: bitset_to_bytes(&r.locally_free.0),
        connective_used: r.connective_used,
    }
}
pub fn receive_from_proto(p: &p::Receive) -> Result<a::Receive, ModelsError> {Ok(
    a::Receive {
        binds: p.binds.iter().map(receive_bind_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        body: Box::new(par_from_proto(p.body.as_ref().ok_or(ModelsError::Malformed("body"))?)?),
        persistent: p.persistent,
        peek: p.peek,
        bind_count: p.bind_count,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
        connective_used: p.connective_used,
    }
)}

pub fn receive_bind_to_proto(rb: &a::ReceiveBind) -> p::ReceiveBind {
    p::ReceiveBind {
        patterns: rb.patterns.iter().map(|p| par_to_proto(p)).collect(),
        source: Some(par_to_proto(rb.source.as_ref())),
        remainder: rb.remainder.as_deref().map(var_to_proto),
        free_count: i32::from(rb.free_count),
    }
}
pub fn receive_bind_from_proto(p: &p::ReceiveBind) -> Result<a::ReceiveBind, ModelsError> {Ok(
    a::ReceiveBind {
        patterns: p.patterns.iter().map(|p| par_from_proto(p)).collect::<Result<Vec<_>, ModelsError>>()?,
        source: Box::new(par_from_proto::<a::NameSort>(p.source.as_ref().ok_or(ModelsError::Malformed("source"))?)?),
        remainder: p.remainder.as_ref().map(var_from_proto).map(Box::new),
        free_count: FreeCount::try_from(p.free_count).map_err(ModelsError::Decode)?,
    }
)}

pub fn new_to_proto(n: &a::New) -> p::New {
    p::New {
        bind_count: n.bind_count,
        p: Some(par_to_proto(n.p.as_ref())),
        uri: n.uri.clone(),
        injections: n
            .injections
            .iter()
            .map(|(k, v)| (k.clone(), par_to_proto(v)))
            .collect(),
        locally_free: bitset_to_bytes(&n.locally_free.0),
    }
}
pub fn new_from_proto(p: &p::New) -> Result<a::New, ModelsError> {Ok(
    a::New {
        bind_count: p.bind_count,
        p: Box::new(par_from_proto(p.p.as_ref().ok_or(ModelsError::Malformed("p"))?)?),
        uri: p.uri.clone(),
        injections: p
            .injections
            .iter()
            .map(|(k, v)| Ok((k.clone(), par_from_proto(v)?)))
            .collect::<Result<std::collections::BTreeMap<_, _>, ModelsError>>()?,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
    }
)}

pub fn match_to_proto(m: &a::Match) -> p::Match {
    p::Match {
        target: Some(par_to_proto(m.target.as_ref())),
        cases: m.cases.iter().map(match_case_to_proto).collect(),
        locally_free: bitset_to_bytes(&m.locally_free.0),
        connective_used: m.connective_used,
    }
}
pub fn match_from_proto(p: &p::Match) -> Result<a::Match, ModelsError> {Ok(
    a::Match {
        target: Box::new(par_from_proto::<a::NameSort>(p.target.as_ref().ok_or(ModelsError::Malformed("target"))?)?),
        cases: p.cases.iter().map(match_case_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
        connective_used: p.connective_used,
    }
)}

pub fn match_case_to_proto(mc: &a::MatchCase) -> p::MatchCase {
    p::MatchCase {
        pattern: Some(par_to_proto(mc.pattern.as_ref())),
        source: Some(par_to_proto(mc.source.as_ref())),
        free_count: i32::from(mc.free_count),
    }
}
pub fn match_case_from_proto(p: &p::MatchCase) -> Result<a::MatchCase, ModelsError> {Ok(
    a::MatchCase {
        pattern: Box::new(par_from_proto::<a::NameSort>(p.pattern.as_ref().ok_or(ModelsError::Malformed("pattern"))?)?),
        source: Box::new(par_from_proto(p.source.as_ref().ok_or(ModelsError::Malformed("source"))?)?),
        free_count: FreeCount::try_from(p.free_count).map_err(ModelsError::Decode)?,
    }
)}

pub fn bundle_to_proto(b: &a::Bundle) -> p::Bundle {
    p::Bundle {
        body: Some(par_to_proto(&b.body)),
        write_flag: b.write_flag,
        read_flag: b.read_flag,
    }
}
pub fn bundle_from_proto(p: &p::Bundle) -> Result<a::Bundle, ModelsError> {Ok(
    a::Bundle {
        body: Box::new(par_from_proto(p.body.as_ref().ok_or(ModelsError::Malformed("body"))?)?),
        write_flag: p.write_flag,
        read_flag: p.read_flag,
    }
)}

pub fn par_to_proto<S: a::Sort>(par: &a::Par<S>) -> p::Par {
    p::Par {
        sends: par.sends.iter().map(send_to_proto).collect(),
        receives: par.receives.iter().map(receive_to_proto).collect(),
        news: par.news.iter().map(new_to_proto).collect(),
        exprs: par.exprs.iter().map(expr_to_proto).collect(),
        matches: par.matches.iter().map(match_to_proto).collect(),
        unforgeables: par.unforgeables.iter().map(unforgeable_to_proto).collect(),
        bundles: par.bundles.iter().map(bundle_to_proto).collect(),
        connectives: par.connectives.iter().map(connective_to_proto).collect(),
        locally_free: bitset_to_bytes(&par.locally_free.0),
        connective_used: par.connective_used,
    }
}

pub fn par_from_proto<S: a::Sort>(p: &p::Par) -> Result<a::Par<S>, ModelsError> {Ok(
    a::Par {
        sends: p.sends.iter().map(send_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        receives: p.receives.iter().map(receive_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        news: p.news.iter().map(new_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        exprs: p.exprs.iter().map(expr_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        matches: p.matches.iter().map(match_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        unforgeables: p.unforgeables.iter().map(unforgeable_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        bundles: p.bundles.iter().map(bundle_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        connectives: p.connectives.iter().map(connective_from_proto).collect::<Result<Vec<_>, ModelsError>>()?,
        locally_free: a::AlwaysEqual(bytes_to_bitset(&p.locally_free)),
        connective_used: p.connective_used,
        ..Default::default()
    }
)}

// --- Runtime types ------------------------------------------------------------------------------

pub fn bind_pattern_to_proto(bp: &BindPattern) -> p::BindPattern {
    p::BindPattern {
        patterns: bp.patterns.iter().map(|p| par_to_proto(p.as_par())).collect(),
        remainder: bp.remainder.as_ref().map(var_to_proto),
        free_count: bp.free_count,
    }
}
pub fn bind_pattern_from_proto(p: &p::BindPattern) -> Result<BindPattern, ModelsError> {Ok(
    BindPattern {
        patterns: p.patterns.iter().map(|p| par_from_proto(p).map(SortedProc::new)).collect::<Result<Vec<_>, ModelsError>>()?,
        remainder: p.remainder.as_ref().map(var_from_proto),
        free_count: p.free_count,
    }
)}

pub fn list_par_with_random_to_proto(l: &ListParWithRandom) -> p::ListParWithRandom {
    p::ListParWithRandom {
        pars: l.pars.iter().map(|p| par_to_proto(p.as_par())).collect(),
        random_state: l.random_state.to_bytes(),
    }
}
pub fn list_par_with_random_from_proto(p: &p::ListParWithRandom) -> Result<ListParWithRandom, String> {
    Ok(ListParWithRandom {
        pars: p.pars.iter().map(|p| par_from_proto(p).map(SortedProc::new)).collect::<Result<Vec<_>, ModelsError>>().map_err(|e| e.to_string())?,
        random_state: Blake2b512Random::from_bytes(
            &SerializedRandom::try_from(p.random_state.as_slice()).map_err(|e| e.to_string())?,
        )
        .map_err(|e| e.to_string())?,
    })
}

pub fn par_with_random_to_proto(pw: &ParWithRandom) -> p::ParWithRandom {
    p::ParWithRandom {
        body: Some(par_to_proto(pw.body.as_par())),
        random_state: pw.random_state.to_bytes(),
    }
}
pub fn par_with_random_from_proto(p: &p::ParWithRandom) -> Result<ParWithRandom, String> {
    Ok(ParWithRandom {
        body: SortedProc::new(par_from_proto(p.body.as_ref().ok_or(ModelsError::Malformed("body")).map_err(|e| e.to_string())?).map_err(|e| e.to_string())?),
        random_state: Blake2b512Random::from_bytes(
            &SerializedRandom::try_from(p.random_state.as_slice()).map_err(|e| e.to_string())?,
        )
        .map_err(|e| e.to_string())?,
    })
}

pub fn tagged_continuation_to_proto(tc: &TaggedContinuation) -> p::TaggedContinuation {
    let tagged_cont = match tc {
        TaggedContinuation::ParBody(pw) => {
            p::tagged_continuation::TaggedCont::ParBody(par_with_random_to_proto(pw))
        }
        TaggedContinuation::ScalaBodyRef(r) => {
            p::tagged_continuation::TaggedCont::ScalaBodyRef(*r)
        }
        TaggedContinuation::Empty => return p::TaggedContinuation { tagged_cont: None },
    };
    p::TaggedContinuation {
        tagged_cont: Some(tagged_cont),
    }
}
pub fn tagged_continuation_from_proto(p: &p::TaggedContinuation) -> Result<TaggedContinuation, String> {
    match &p.tagged_cont {
        Some(p::tagged_continuation::TaggedCont::ParBody(pw)) => {
            Ok(TaggedContinuation::ParBody(par_with_random_from_proto(pw)?))
        }
        Some(p::tagged_continuation::TaggedCont::ScalaBodyRef(r)) => {
            Ok(TaggedContinuation::ScalaBodyRef(*r))
        }
        None => Ok(TaggedContinuation::Empty),
    }
}

// --- Serialize instances ------------------------------------------------------------------------

impl<S: a::Sort> Serialize<a::Par<S>> for a::Par<S> {
    fn encode(a: &a::Par<S>) -> Vec<u8> {
        par_to_proto(a).encode_to_vec()
    }
    fn decode(bytes: &[u8]) -> Result<a::Par<S>, String> {
        let proto = <p::Par as ::prost::Message>::decode(bytes).map_err(|e| e.to_string())?;
        par_from_proto(&proto).map_err(|e| e.to_string())
    }
}

impl Serialize<BindPattern> for BindPattern {
    fn encode(a: &BindPattern) -> Vec<u8> {
        bind_pattern_to_proto(a).encode_to_vec()
    }
    fn decode(bytes: &[u8]) -> Result<BindPattern, String> {
        let proto = <p::BindPattern as ::prost::Message>::decode(bytes).map_err(|e| e.to_string())?;
        bind_pattern_from_proto(&proto).map_err(|e| e.to_string())
    }
}

impl Serialize<ListParWithRandom> for ListParWithRandom {
    fn encode(a: &ListParWithRandom) -> Vec<u8> {
        list_par_with_random_to_proto(a).encode_to_vec()
    }
    fn decode(bytes: &[u8]) -> Result<ListParWithRandom, String> {
        let proto =
            <p::ListParWithRandom as ::prost::Message>::decode(bytes).map_err(|e| e.to_string())?;
        list_par_with_random_from_proto(&proto)
    }
}

impl Serialize<TaggedContinuation> for TaggedContinuation {
    fn encode(a: &TaggedContinuation) -> Vec<u8> {
        tagged_continuation_to_proto(a).encode_to_vec()
    }
    fn decode(bytes: &[u8]) -> Result<TaggedContinuation, String> {
        let proto =
            <p::TaggedContinuation as ::prost::Message>::decode(bytes).map_err(|e| e.to_string())?;
        tagged_continuation_from_proto(&proto)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bitset_round_trips() {
        let bits = vec![0, 1, 65, 130];
        let bytes = bitset_to_bytes(&bits);
        assert_eq!(bytes_to_bitset(&bytes), bits);
        assert!(bitset_to_bytes(&[]).is_empty());
        assert_eq!(bytes_to_bitset(&[]), Vec::<i32>::new());
    }

    #[test]
    fn par_serialize_round_trips() {
        let par = a::Par {
            exprs: vec![a::Expr::EPlus(
                Box::new(a::Par {
                    exprs: vec![a::Expr::GInt(2)],
                    ..Default::default()
                }),
                Box::new(a::Par {
                    exprs: vec![a::Expr::GInt(3)],
                    ..Default::default()
                }),
            )],
            locally_free: a::AlwaysEqual(vec![1, 7]),
            ..Default::default()
        };
        let bytes = <a::Par as Serialize<a::Par>>::encode(&par);
        let decoded = <a::Par as Serialize<a::Par>>::decode(&bytes).unwrap();
        assert_eq!(decoded, par);
    }
}

/// Differential tests against the Scala scalapb `TypeMapper` encodings and `Serialize[Par]` wire
/// bytes. Golden vectors are captured in `testdata/differential/wire.tsv`. The custom `locallyFree`
/// `BitSet` encoding (little-endian `Long` mask with trailing zeros stripped) is the Scala-specific
/// behavior that must be reproduced byte-for-byte.
#[cfg(test)]
mod differential {
    use super::*;
    use rchain_shared::base16;

    fn load(case: &str) -> String {
        let path = concat!(env!("CARGO_MANIFEST_DIR"), "/testdata/differential/wire.tsv");
        let data = std::fs::read_to_string(path).unwrap();
        for line in data.lines() {
            let (id, hex) = line.split_once('\t').unwrap_or((line, ""));
            if id == case {
                return hex.to_string();
            }
        }
        panic!("missing differential case: {case}");
    }

    fn hex(bytes: &[u8]) -> String {
        base16::encode(bytes)
    }

    #[test]
    fn differential_bitset_encoding() {
        assert_eq!(hex(&bitset_to_bytes(&[])), load("bitset_empty"));
        assert_eq!(hex(&bitset_to_bytes(&[0])), load("bitset_0"));
        assert_eq!(hex(&bitset_to_bytes(&[7])), load("bitset_7"));
        assert_eq!(hex(&bitset_to_bytes(&[8])), load("bitset_8"));
        assert_eq!(hex(&bitset_to_bytes(&[64])), load("bitset_64"));
    }

    #[test]
    fn differential_empty_par_serializes_to_empty() {
        let bytes = <a::Par as Serialize<a::Par>>::encode(&a::Par::default());
        assert_eq!(hex(&bytes), load("par_empty"));
    }

    #[test]
    fn par_with_g_string_serializes_to_proto() {
        // `Par { exprs: [GString("foo")] }` -> `2a 05 (1a 03 "foo")` (field 5 exprs, field 3 g_string).
        let par = a::Par {
            exprs: vec![a::Expr::GString("foo".to_string())],
            ..Default::default()
        };
        let bytes = <a::Par as Serialize<a::Par>>::encode(&par);
        assert_eq!(hex(&bytes), "2a051a03666f6f");
    }

    #[test]
    fn par_with_g_private_serializes_to_proto() {
        // `Par { unforgeables: [GPrivate([0x01; 32])] }` -> `3a 24 (0a 22 (0a 20 <32x01>))`
        // (field 7 unforgeables, field 1 g_private_body, field 1 id).
        let par = a::Par {
            unforgeables: vec![a::GUnforgeable::GPrivate(a::GPrivate { id: vec![0x01; 32] })],
            ..Default::default()
        };
        let bytes = <a::Par as Serialize<a::Par>>::encode(&par);
        let expected = format!("3a240a220a20{}", "01".repeat(32));
        assert_eq!(hex(&bytes), expected);
    }
}
