//! The concrete rholang syntax tree (port of `rholang_mercury.Absyn` from `rholang_mercury.cf`).
//!
//! The lexer/parser produces this tree; the normalizer folds it into the de Bruijn `Par`. Source
//! variable names are `String` (the `Var` token); the de Bruijn `Var` lives in `rchain_models`.

/// A source variable name (the grammar `Var` token).
pub type SourceVar = String;

/// A long-integer literal (kept as text, matching the BNFC `LongLiteral` token).
pub type LongLiteral = String;

// -------------------------------------------------------------------------------------------------
// Proc (the process)
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Proc {
    PGround(Ground),
    PCollect(Collection),
    PVar(ProcVar),
    PVarRef(VarRefKind, SourceVar),
    PNil,
    PSimpleType(SimpleType),
    PNegation(Box<Proc>),
    PConjunction(Box<Proc>, Box<Proc>),
    PDisjunction(Box<Proc>, Box<Proc>),
    PEval(Name),
    PMethod(Box<Proc>, SourceVar, Vec<Proc>),
    PExprs(Box<Proc>),
    PNot(Box<Proc>),
    PNeg(Box<Proc>),
    PMult(Box<Proc>, Box<Proc>),
    PDiv(Box<Proc>, Box<Proc>),
    PMod(Box<Proc>, Box<Proc>),
    PPercentPercent(Box<Proc>, Box<Proc>),
    PAdd(Box<Proc>, Box<Proc>),
    PMinus(Box<Proc>, Box<Proc>),
    PPlusPlus(Box<Proc>, Box<Proc>),
    PMinusMinus(Box<Proc>, Box<Proc>),
    PLt(Box<Proc>, Box<Proc>),
    PLte(Box<Proc>, Box<Proc>),
    PGt(Box<Proc>, Box<Proc>),
    PGte(Box<Proc>, Box<Proc>),
    PMatches(Box<Proc>, Box<Proc>),
    PEq(Box<Proc>, Box<Proc>),
    PNeq(Box<Proc>, Box<Proc>),
    PAnd(Box<Proc>, Box<Proc>),
    PShortAnd(Box<Proc>, Box<Proc>),
    POr(Box<Proc>, Box<Proc>),
    PShortOr(Box<Proc>, Box<Proc>),
    PSend(Name, Send, Vec<Proc>),
    PContr(Name, Vec<Name>, NameRemainder, Box<Proc>),
    PInput(Vec<Receipt>, Box<Proc>),
    PChoice(Vec<Branch>),
    PMatch(Box<Proc>, Vec<Case>),
    PBundle(Bundle, Box<Proc>),
    PLet(Decl, Decls, Box<Proc>),
    PIf(Box<Proc>, Box<Proc>),
    PIfElse(Box<Proc>, Box<Proc>, Box<Proc>),
    PNew(Vec<NameDecl>, Box<Proc>),
    PSendSynch(Name, Vec<Proc>, SynchSendCont),
    PPar(Box<Proc>, Box<Proc>),
}

// -------------------------------------------------------------------------------------------------
// Ground
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BoolLiteral {
    BoolTrue,
    BoolFalse,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Ground {
    GroundBool(BoolLiteral),
    GroundBigInt(LongLiteral),
    GroundInt(LongLiteral),
    GroundString(String),
    GroundUri(String),
}

// -------------------------------------------------------------------------------------------------
// Collection
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Collection {
    CollectList(Vec<Proc>, ProcRemainder),
    CollectTuple(Tuple),
    CollectSet(Vec<Proc>, ProcRemainder),
    CollectMap(Vec<KeyValuePair>, ProcRemainder),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct KeyValuePair(pub Proc, pub Proc);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tuple {
    TupleSingle(Box<Proc>),
    TupleMultiple(Box<Proc>, Vec<Proc>),
}

// -------------------------------------------------------------------------------------------------
// ProcVar / Name
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProcVar {
    ProcVarWildcard,
    ProcVarVar(SourceVar),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Name {
    NameWildcard,
    NameVar(SourceVar),
    NameQuote(Box<Proc>),
}

// -------------------------------------------------------------------------------------------------
// Bundle
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Bundle {
    BundleWrite,
    BundleRead,
    BundleEquiv,
    BundleReadWrite,
}

// -------------------------------------------------------------------------------------------------
// Receipts / binds
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Receipt {
    ReceiptLinear(ReceiptLinearImpl),
    ReceiptRepeated(ReceiptRepeatedImpl),
    ReceiptPeek(ReceiptPeekImpl),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReceiptLinearImpl {
    LinearSimple(Vec<LinearBind>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearBind(pub Vec<Name>, pub NameRemainder, pub NameSource);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NameSource {
    SimpleSource(Name),
    ReceiveSendSource(Name),
    SendReceiveSource(Name, Vec<Proc>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReceiptRepeatedImpl {
    RepeatedSimple(Vec<RepeatedBind>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct RepeatedBind(pub Vec<Name>, pub NameRemainder, pub Name);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ReceiptPeekImpl {
    PeekSimple(Vec<PeekBind>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct PeekBind(pub Vec<Name>, pub NameRemainder, pub Name);

// -------------------------------------------------------------------------------------------------
// Send / Branch / Case
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Send {
    SendSingle,
    SendMultiple,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Branch(pub ReceiptLinearImpl, pub Box<Proc>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Case(pub Box<Proc>, pub Box<Proc>);

// -------------------------------------------------------------------------------------------------
// Name declarations
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NameDecl {
    NameDeclSimpl(SourceVar),
    NameDeclUrn(SourceVar, String),
}

// -------------------------------------------------------------------------------------------------
// Remainders
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProcRemainder {
    ProcRemainderVar(ProcVar),
    ProcRemainderEmpty,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum NameRemainder {
    NameRemainderVar(ProcVar),
    NameRemainderEmpty,
}

// -------------------------------------------------------------------------------------------------
// VarRefKind / SimpleType
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VarRefKind {
    VarRefKindProc,
    VarRefKindName,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SimpleType {
    SimpleTypeBool,
    SimpleTypeInt,
    SimpleTypeBigInt,
    SimpleTypeString,
    SimpleTypeUri,
    SimpleTypeByteArray,
}

// -------------------------------------------------------------------------------------------------
// Let declarations
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Decl(pub Vec<Name>, pub NameRemainder, pub Vec<Proc>);

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Decls {
    EmptyDeclImpl,
    LinearDeclsImpl(Vec<LinearDecl>),
    ConcDeclsImpl(Vec<ConcDecl>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct LinearDecl(pub Decl);

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConcDecl(pub Decl);

// -------------------------------------------------------------------------------------------------
// Synchronous send continuation
// -------------------------------------------------------------------------------------------------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum SynchSendCont {
    EmptyCont,
    NonEmptyCont(Box<Proc>),
}
