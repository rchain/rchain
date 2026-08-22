//! The rholang term structure (hand-written AST mirroring `RhoTypes.proto`).
//!
//! Mirrors `models/src/main/protobuf/RhoTypes.proto`. `locallyFree` is wrapped in [`AlwaysEqual`]
//! so that it is excluded from equality, exactly as in the Scala `AlwaysEqual[BitSet]` mapper.

use std::collections::BTreeMap;
use std::fmt;
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;

use num_bigint::BigInt;
use serde::{Deserialize, Serialize};

use crate::types::FreeCount;

/// serde helpers encoding byte vectors as lowercase hex (the Scala `encodeByteString`/
/// `decodeByteString` via `Base16`).
pub(crate) mod hex_serde {
    use rchain_shared::base16;
    use serde::{Deserialize, Deserializer, Serializer};

    pub fn serialize<S: Serializer>(bytes: &[u8], serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_str(&base16::encode(bytes))
    }

    pub fn deserialize<'de, D: Deserializer<'de>>(deserializer: D) -> Result<Vec<u8>, D::Error> {
        let s = String::deserialize(deserializer)?;
        base16::decode(&s).ok_or_else(|| serde::de::Error::custom("invalid hex"))
    }
}

/// A bitset of free-variable levels (the Scala `scala.collection.immutable.BitSet`).
pub type BitSet = Vec<i32>;

/// A wrapper whose equality and hash are constant, mirroring the Scala `AlwaysEqual`.
///
/// Used for `locallyFree`, which is excluded from `Par`/`Send`/… equality by design.
#[derive(Clone, Debug, Default)]
pub struct AlwaysEqual<T>(pub T);

impl<T> PartialEq for AlwaysEqual<T> {
    fn eq(&self, _other: &Self) -> bool {
        true
    }
}

impl<T> Eq for AlwaysEqual<T> {}

impl<T> Hash for AlwaysEqual<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        121410467i32.hash(state);
    }
}

impl<T> PartialOrd for AlwaysEqual<T> {
    fn partial_cmp(&self, _other: &Self) -> Option<std::cmp::Ordering> {
        Some(std::cmp::Ordering::Equal)
    }
}

impl<T> Ord for AlwaysEqual<T> {
    fn cmp(&self, _other: &Self) -> std::cmp::Ordering {
        std::cmp::Ordering::Equal
    }
}

// `AlwaysEqual` serializes as JSON unit (`null`), matching the Scala `encodeAlwaysEqual`/
// `decodeAlwaysEqual` (`Encoder.encodeUnit` / `Decoder.decodeUnit`).
impl<T> Serialize for AlwaysEqual<T> {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        serializer.serialize_unit()
    }
}

impl<'de, T: Default> Deserialize<'de> for AlwaysEqual<T> {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let () = <() as Deserialize>::deserialize(deserializer)?;
        Ok(AlwaysEqual(T::default()))
    }
}

/// A variable (de Bruijn levels).
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub enum Var {
    BoundVar(i32),
    FreeVar(i32),
    Wildcard,
    #[default]
    Empty,
}

/// The ρ-calculus sort: a term is a `Name` (usable in name position) or a `Proc` (usable in
/// process position). This is the base sort of the Calculus of Constructions (see
/// `spec/RHO-CALCULUS.md`); it is a *compile-time* property, carried as a phantom parameter on
/// [`Par`].
pub trait Sort:
    private::Sealed + Clone + Copy + std::fmt::Debug + Default + PartialEq + Eq + PartialOrd + Ord + Hash + 'static
{
}
mod private {
    pub trait Sealed {}
}
/// The `name` sort — a term usable in name position (pure names, grounds, quoted processes).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct NameSort;
/// The `proc` sort — a term usable in process position (sends/receives/news/matches).
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct ProcSort;
impl private::Sealed for NameSort {}
impl private::Sealed for ProcSort {}
impl Sort for NameSort {}
impl Sort for ProcSort {}

/// The join of two sorts under parallel composition (the sort lattice): `Name ⊔ X = X`, and
/// `Proc ⊔ X = Proc`. `Name` is the identity, `Proc` the absorbing element.
pub trait SortJoin<B: Sort> {
    type Output: Sort;
}
impl<B: Sort> SortJoin<B> for NameSort {
    type Output = B;
}
impl<B: Sort> SortJoin<B> for ProcSort {
    type Output = ProcSort;
}

/// A `Par` — the top-level process, a flat record of eight list fields, sort-indexed by `S`.
///
/// The phantom `S` is not part of the wire encoding (it is `#[serde(skip)]`ed) nor of the
/// structural equality/order, so the flat canonical form of Law 1 is preserved.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
#[serde(bound = "")]
pub struct Par<S: Sort = ProcSort> {
    pub sends: Vec<Send>,
    pub receives: Vec<Receive>,
    pub news: Vec<New>,
    pub exprs: Vec<Expr>,
    pub matches: Vec<Match>,
    pub unforgeables: Vec<GUnforgeable>,
    pub bundles: Vec<Bundle>,
    pub connectives: Vec<Connective>,
    pub locally_free: AlwaysEqual<BitSet>,
    pub connective_used: bool,
    #[serde(skip)]
    pub _sort: PhantomData<S>,
}

/// The `name`-sorted term and the `proc`-sorted term (aliases over the shared flat `Par`).
pub type Name = Par<NameSort>;
pub type Proc = Par<ProcSort>;

impl<S: Sort> Par<S> {
    /// Field-wise list append (the `|` operator), sort-preserving.
    pub fn par_merge(&self, other: &Self) -> Self {
        let mut out = self.clone();
        out.sends.extend(other.sends.iter().cloned());
        out.receives.extend(other.receives.iter().cloned());
        out.news.extend(other.news.iter().cloned());
        out.exprs.extend(other.exprs.iter().cloned());
        out.matches.extend(other.matches.iter().cloned());
        out.unforgeables.extend(other.unforgeables.iter().cloned());
        out.bundles.extend(other.bundles.iter().cloned());
        out.connectives.extend(other.connectives.iter().cloned());
        out.connective_used = self.connective_used || other.connective_used;
        out
    }

    /// Re-mark the phantom sort (the flat record is unchanged). The sort is a marker, so this is
    /// a total operation on the identical flat record.
    ///
    /// This is the unchecked reflection primitive behind `quote`/`eval`: the sort has no runtime
    /// representation, so there is nothing to validate against here. When a structurally-pure name
    /// is required (Law 1 / the name-sort judgment), use `TryFrom<Par> for Name` in `types.rs`,
    /// which checks [`is_pure_name`](crate::types::is_pure_name).
    pub fn re_sort<T: Sort>(self) -> Par<T> {
        Par {
            sends: self.sends,
            receives: self.receives,
            news: self.news,
            exprs: self.exprs,
            matches: self.matches,
            unforgeables: self.unforgeables,
            bundles: self.bundles,
            connectives: self.connectives,
            locally_free: self.locally_free,
            connective_used: self.connective_used,
            _sort: PhantomData,
        }
    }
}

impl Par<ProcSort> {
    /// `@Proc` — quote a process into a name (the reflective `@`; the flat record is unchanged,
    /// only the sort marker changes).
    pub fn quote(self) -> Name {
        self.re_sort()
    }
}

impl Par<NameSort> {
    /// `*Name` — evaluate a name into a process (the reflective `*`).
    pub fn eval(self) -> Proc {
        self.re_sort()
    }
}

/// A send: `chan!(data)` (or `chan!!(data)` when persistent).
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct Send {
    pub chan: Box<Name>,
    pub data: Vec<Name>,
    pub persistent: bool,
    pub locally_free: AlwaysEqual<BitSet>,
    pub connective_used: bool,
}

/// A receive bind: `patterns <- source`.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct ReceiveBind {
    pub patterns: Vec<Name>,
    pub source: Box<Name>,
    pub remainder: Option<Box<Var>>,
    pub free_count: FreeCount,
}

/// A receive: `for (binds) { body }`.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct Receive {
    pub binds: Vec<ReceiveBind>,
    pub body: Box<Par>,
    pub persistent: bool,
    pub peek: bool,
    pub bind_count: i32,
    pub locally_free: AlwaysEqual<BitSet>,
    pub connective_used: bool,
}

/// A `new x1, ..., xn in { p }`.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct New {
    pub bind_count: i32,
    pub p: Box<Par>,
    pub uri: Vec<String>,
    pub injections: BTreeMap<String, Par>,
    pub locally_free: AlwaysEqual<BitSet>,
}

/// A match case: `pattern => source`.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct MatchCase {
    pub pattern: Box<Name>,
    pub source: Box<Par>,
    pub free_count: FreeCount,
}

/// A `match target { cases }`.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct Match {
    pub target: Box<Name>,
    pub cases: Vec<MatchCase>,
    pub locally_free: AlwaysEqual<BitSet>,
    pub connective_used: bool,
}

/// A quoted/unquoted bundle.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct Bundle {
    pub body: Box<Par>,
    pub write_flag: bool,
    pub read_flag: bool,
}

impl Bundle {
    /// Merge bundle flags (port of `BundleOps.merge`): keep `other`'s body, AND the read/write flags.
    pub fn merge(&self, other: &Bundle) -> Bundle {
        Bundle {
            body: other.body.clone(),
            write_flag: self.write_flag && other.write_flag,
            read_flag: self.read_flag && other.read_flag,
        }
    }
}

impl fmt::Display for Bundle {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Port of `BundleOps.showInstance`: a left-justified `bundle` plus the read/write sign.
        let sign = match (self.read_flag, self.write_flag) {
            (true, true) => "",
            (true, false) => "-",
            (false, true) => "+",
            (false, false) => "0",
        };
        write!(f, "{:<8}", format!("bundle{sign}"))
    }
}

/// An expression.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Serialize, Deserialize)]
pub enum Expr {
    GBool(bool),
    GInt(i64),
    GBigInt(BigInt),
    GString(String),
    GUri(String),
    GByteArray(#[serde(with = "hex_serde")] Vec<u8>),
    ENot(Box<Par>),
    ENeg(Box<Par>),
    EVar(Box<Var>),
    EMult(Box<Par>, Box<Par>),
    EDiv(Box<Par>, Box<Par>),
    EMod(Box<Par>, Box<Par>),
    EPlus(Box<Par>, Box<Par>),
    EMinus(Box<Par>, Box<Par>),
    ELt(Box<Par>, Box<Par>),
    ELte(Box<Par>, Box<Par>),
    EGt(Box<Par>, Box<Par>),
    EGte(Box<Par>, Box<Par>),
    EEq(Box<Par>, Box<Par>),
    ENeq(Box<Par>, Box<Par>),
    EAnd(Box<Par>, Box<Par>),
    EOr(Box<Par>, Box<Par>),
    EShortAnd(Box<Par>, Box<Par>),
    EShortOr(Box<Par>, Box<Par>),
    EMatches(Box<Par>, Box<Par>),
    EPercentPercent(Box<Par>, Box<Par>),
    EPlusPlus(Box<Par>, Box<Par>),
    EMinusMinus(Box<Par>, Box<Par>),
    EList(EList),
    ETuple(ETuple),
    ESet(ParSet),
    EMap(ParMap),
    EMethod(EMethod),
}

/// A list expression.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct EList {
    pub ps: Vec<Par>,
    pub locally_free: AlwaysEqual<BitSet>,
    pub connective_used: bool,
    pub remainder: Option<Box<Var>>,
}

/// A tuple expression.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct ETuple {
    pub ps: Vec<Par>,
    pub locally_free: AlwaysEqual<BitSet>,
    pub connective_used: bool,
}

/// A set expression (order-insensitive, deduplicated).
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default)]
pub struct ParSet {
    pub ps: Vec<Par>,
    pub connective_used: bool,
    pub locally_free: AlwaysEqual<BitSet>,
    pub remainder: Option<Box<Var>>,
}

/// A map expression (order-insensitive by key, last-write-wins).
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default)]
pub struct ParMap {
    pub kvs: Vec<(Par, Par)>,
    pub connective_used: bool,
    pub locally_free: AlwaysEqual<BitSet>,
    pub remainder: Option<Box<Var>>,
}

// `ParSet`/`ParMap` serialize as a list (of `Par` / `(Par, Par)`), matching the Scala
// `Encoder.encodeList.contramap` / `Decoder.decodeList` instances.
impl Serialize for ParSet {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.ps.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ParSet {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let ps = Vec::<Par>::deserialize(deserializer)?;
        Ok(ParSet {
            ps,
            connective_used: false,
            locally_free: AlwaysEqual(BitSet::default()),
            remainder: None,
        })
    }
}

impl Serialize for ParMap {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.kvs.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for ParMap {
    fn deserialize<D: serde::Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let kvs = Vec::<(Par, Par)>::deserialize(deserializer)?;
        Ok(ParMap {
            kvs,
            connective_used: false,
            locally_free: AlwaysEqual(BitSet::default()),
            remainder: None,
        })
    }
}

/// A method call: `target.methodName(arguments)`.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct EMethod {
    pub method_name: String,
    pub target: Box<Par>,
    pub arguments: Vec<Par>,
    pub locally_free: AlwaysEqual<BitSet>,
    pub connective_used: bool,
}

/// An unforgeable name.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub enum GUnforgeable {
    GPrivate(GPrivate),
    GDeployId(GDeployId),
    GDeployerId(GDeployerId),
    GSysAuthToken,
    #[default]
    Empty,
}

#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct GPrivate {
    #[serde(with = "hex_serde")]
    pub id: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct GDeployId {
    #[serde(with = "hex_serde")]
    pub sig: Vec<u8>,
}

#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct GDeployerId {
    #[serde(with = "hex_serde")]
    pub public_key: Vec<u8>,
}

/// A logical connective.
#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub enum Connective {
    ConnAnd(ConnectiveBody),
    ConnOr(ConnectiveBody),
    ConnNot(Box<Par>),
    VarRef(VarRef),
    ConnBool(bool),
    ConnInt(bool),
    ConnBigInt(bool),
    ConnString(bool),
    ConnUri(bool),
    ConnByteArray(bool),
    #[default]
    Empty,
}

#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct ConnectiveBody {
    pub ps: Vec<Par>,
}

#[derive(Clone, Debug, PartialEq, Ord, PartialOrd, Eq, Default, Serialize, Deserialize)]
pub struct VarRef {
    pub index: i32,
    pub depth: i32,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn bundle_display_shows_read_write_sign() {
        let mk = |read: bool, write: bool| Bundle {
            body: Box::new(Par::default()),
            write_flag: write,
            read_flag: read,
        };
        assert_eq!(mk(true, true).to_string(), "bundle  ");
        assert_eq!(mk(true, false).to_string(), "bundle- ");
        assert_eq!(mk(false, true).to_string(), "bundle+ ");
        assert_eq!(mk(false, false).to_string(), "bundle0 ");
    }

    #[test]
    fn par_serde_round_trips() {
        let par = Par::default();
        let json = serde_json::to_string(&par).unwrap();
        let par2: Par = serde_json::from_str(&json).unwrap();
        assert_eq!(par, par2);
    }

    #[test]
    fn always_equal_serializes_as_null() {
        let ae = AlwaysEqual(vec![1, 2, 3]);
        assert_eq!(serde_json::to_string(&ae).unwrap(), "null");
        let ae2: AlwaysEqual<BitSet> = serde_json::from_str("null").unwrap();
        assert!(ae2.0.is_empty());
    }

    #[test]
    fn par_set_serializes_as_list() {
        let ps = ParSet {
            ps: vec![Par::default()],
            connective_used: false,
            locally_free: AlwaysEqual(BitSet::default()),
            remainder: None,
        };
        let json = serde_json::to_string(&ps).unwrap();
        let ps2: ParSet = serde_json::from_str(&json).unwrap();
        assert_eq!(ps2.ps.len(), 1);
        assert!(!ps2.connective_used);
    }

    #[test]
    fn byte_array_serializes_as_hex() {
        let expr = Expr::GByteArray(vec![0xde, 0xad, 0xbe, 0xef]);
        let json = serde_json::to_string(&expr).unwrap();
        assert_eq!(json, "{\"GByteArray\":\"deadbeef\"}");
        let expr2: Expr = serde_json::from_str(&json).unwrap();
        assert_eq!(expr, expr2);
    }
}
