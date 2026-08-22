//! Hand-written lexer + recursive-descent parser for rholang (port of the BNFC-generated
//! `Yylex`/`parser` from `rholang_mercury.cf`).

use crate::errors::RholangError;
use crate::proc_ast::*;

#[derive(Clone, Debug, PartialEq)]
enum Tok {
    Long(i64),
    Str(String),
    Uri(String),
    Ident(String),
    Tilde,
    Conj,  // /\
    Disj,  // \/
    Star,
    Dot,
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Pipe,
    Percent,
    PercentPercent,
    Slash,
    Plus,
    PlusPlus,
    Minus,
    MinusMinus,
    Lt,
    Lte,
    Gt,
    Gte,
    EqEq,
    Neq,
    AndAnd,
    OrOr,
    Bang,
    BangBang,
    BangQ,
    Semicolon,
    Amp,
    Arrow,   // =>
    LArrow,  // <-
    LLArrow, // <<-
    QMark,
    Colon,
    Comma,
    Ellipsis,
    Underscore,
    At,
    Eq,
    Eof,
}

fn is_reserved(s: &str) -> bool {
    matches!(
        s,
        "Nil"
            | "true"
            | "false"
            | "not"
            | "and"
            | "or"
            | "matches"
            | "contract"
            | "for"
            | "select"
            | "match"
            | "bundle"
            | "let"
            | "if"
            | "else"
            | "new"
            | "in"
            | "Set"
            | "Bool"
            | "Int"
            | "BigInt"
            | "String"
            | "Uri"
            | "ByteArray"
    )
}

fn lex(src: &str) -> Result<Vec<Tok>, RholangError> {
    let mut toks = Vec::new();
    let chars: Vec<char> = src.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];
        if c.is_whitespace() {
            i += 1;
            continue;
        }
        // comments
        if c == '/' && i + 1 < chars.len() && chars[i + 1] == '/' {
            while i < chars.len() && chars[i] != '\n' {
                i += 1;
            }
            continue;
        }
        if c == '/' && i + 1 < chars.len() && chars[i + 1] == '*' {
            i += 2;
            while i + 1 < chars.len() && !(chars[i] == '*' && chars[i + 1] == '/') {
                i += 1;
            }
            i += 2;
            continue;
        }
        // string literal
        if c == '"' {
            let start = i;
            i += 1;
            while i < chars.len() && chars[i] != '"' {
                i += 1;
            }
            i += 1; // closing quote
            toks.push(Tok::Str(chars[start..i].iter().collect()));
            continue;
        }
        // uri literal
        if c == '`' {
            let start = i;
            i += 1;
            while i < chars.len() && chars[i] != '`' {
                i += 1;
            }
            i += 1;
            toks.push(Tok::Uri(chars[start..i].iter().collect()));
            continue;
        }
        // number
        if c.is_ascii_digit() {
            let start = i;
            while i < chars.len() && chars[i].is_ascii_digit() {
                i += 1;
            }
            let n: i64 = chars[start..i]
                .iter()
                .collect::<String>()
                .parse::<i64>()
                .map_err(|e| RholangError::LexerError(e.to_string()))?;
            toks.push(Tok::Long(n));
            continue;
        }
        // identifier / keyword. A standalone `_` is the wildcard token (not a `Var`), so a leading
        // `_` only starts an identifier when followed by another identifier-continuation char
        // (mirrors the BNFC `Var` token: `'_' (letter | digit | '_' | '\'')+`).
        let ident_continue = |c: char| c.is_ascii_alphanumeric() || c == '_' || c == '\'';
        if c.is_ascii_alphabetic()
            || (c == '_' && chars.get(i + 1).map(|&n| ident_continue(n)).unwrap_or(false))
        {
            let start = i;
            while i < chars.len() && ident_continue(chars[i]) {
                i += 1;
            }
            let ident: String = chars[start..i].iter().collect();
            // `bundle0` is the bundle-equivalence keyword; tokenize it as `bundle` + `0` so
            // `parse_bundle` can read the suffix (see the comment there).
            if ident == "bundle0" {
                toks.push(Tok::Ident("bundle".to_string()));
                toks.push(Tok::Long(0));
            } else {
                toks.push(Tok::Ident(ident));
            }
            continue;
        }
        // operators
        let peek = |n: usize| chars.get(i + n).copied();
        let (tok, advance): (Tok, usize) = match c {
            '~' => (Tok::Tilde, 1),
            '\\' if peek(1) == Some('/') => (Tok::Conj, 2),
            '\\' if peek(1) == Some('\\') => (Tok::Disj, 2),
            '*' => (Tok::Star, 1),
            '.' if peek(1) == Some('.') && peek(2) == Some('.') => (Tok::Ellipsis, 3),
            '.' => (Tok::Dot, 1),
            '(' => (Tok::LParen, 1),
            ')' => (Tok::RParen, 1),
            '{' => (Tok::LBrace, 1),
            '}' => (Tok::RBrace, 1),
            '[' => (Tok::LBracket, 1),
            ']' => (Tok::RBracket, 1),
            '|' if peek(1) == Some('|') => (Tok::OrOr, 2),
            '|' => (Tok::Pipe, 1),
            '%' if peek(1) == Some('%') => (Tok::PercentPercent, 2),
            '%' => (Tok::Percent, 1),
            '/' => (Tok::Slash, 1),
            '+' if peek(1) == Some('+') => (Tok::PlusPlus, 2),
            '+' => (Tok::Plus, 1),
            '-' if peek(1) == Some('-') => (Tok::MinusMinus, 2),
            '-' if peek(1) == Some('>') => (Tok::Arrow, 2),
            '-' => (Tok::Minus, 1),
            '<' if peek(1) == Some('=') => (Tok::Lte, 2),
            '<' if peek(1) == Some('-') => (Tok::LArrow, 2),
            '<' if peek(1) == Some('<') && peek(2) == Some('-') => (Tok::LLArrow, 3),
            '<' => (Tok::Lt, 1),
            '>' if peek(1) == Some('=') => (Tok::Gte, 2),
            '>' => (Tok::Gt, 1),
            '=' if peek(1) == Some('=') => (Tok::EqEq, 2),
            '=' if peek(1) == Some('>') => (Tok::Arrow, 2),
            '=' => (Tok::Eq, 1),
            '!' if peek(1) == Some('=') => (Tok::Neq, 2),
            '!' if peek(1) == Some('!') => (Tok::BangBang, 2),
            '!' if peek(1) == Some('?') => (Tok::BangQ, 2),
            '!' => (Tok::Bang, 1),
            '&' if peek(1) == Some('&') => (Tok::AndAnd, 2),
            '&' => (Tok::Amp, 1),
            ';' => (Tok::Semicolon, 1),
            '?' => (Tok::QMark, 1),
            ':' => (Tok::Colon, 1),
            ',' => (Tok::Comma, 1),
            '_' => (Tok::Underscore, 1),
            '@' => (Tok::At, 1),
            _ => {
                return Err(RholangError::LexerError(format!(
                    "Illegal character {c} at {i}"
                )))
            }
        };
        i += advance;
        toks.push(tok);
    }
    toks.push(Tok::Eof);
    Ok(toks)
}

/// Maximum recursive-descent nesting depth. Reached by the depth guard in `with_depth` so a
/// maliciously deep term (up to the gRPC message-size limit) cannot overflow the task stack
/// (documented Scala deviation: the BNFC parser has no depth guard). 128 levels is far deeper than
/// any real rholang term yet well within a 2 MiB async-thread stack.
const MAX_PARSE_DEPTH: usize = 128;

struct Parser {
    toks: Vec<Tok>,
    pos: usize,
    depth: usize,
}

impl Parser {
    fn peek(&self) -> &Tok {
        &self.toks[self.pos]
    }
    fn next(&mut self) -> Tok {
        let t = self.toks[self.pos].clone();
        self.pos += 1;
        t
    }
    /// Run `f` under the recursion-depth guard: increment on entry (rejecting once
    /// `MAX_PARSE_DEPTH` is exceeded) and decrement on every exit path, including `?` early
    /// returns inside `f`.
    fn with_depth<T>(
        &mut self,
        f: impl FnOnce(&mut Self) -> Result<T, RholangError>,
    ) -> Result<T, RholangError> {
        self.depth += 1;
        if self.depth > MAX_PARSE_DEPTH {
            self.depth -= 1;
            return Err(RholangError::SyntaxError("parse depth exceeded".to_string()));
        }
        let result = f(self);
        self.depth -= 1;
        result
    }
    fn eat_ident(&mut self, kw: &str) -> bool {
        if let Tok::Ident(id) = self.peek() {
            if id == kw {
                self.pos += 1;
                return true;
            }
        }
        false
    }
    fn expect(&mut self, t: Tok) -> Result<(), RholangError> {
        if self.peek() == &t {
            self.pos += 1;
            Ok(())
        } else {
            Err(RholangError::SyntaxError(format!(
                "expected {t:?}, got {:?} (pos={})",
                self.peek(),
                self.pos
            )))
        }
    }
}

/// Parse a source string into a `Proc` (port of `Compiler.sourceToAST`).
pub fn parse(source: &str) -> Result<Proc, RholangError> {
    let toks = lex(source)?;
    let mut p = Parser { toks, pos: 0, depth: 0 };
    let proc = p.parse_proc()?;
    Ok(proc)
}

impl Parser {
    fn parse_proc(&mut self) -> Result<Proc, RholangError> {
        self.with_depth(|p| {
            let mut left = p.parse_proc1()?;
            while p.peek() == &Tok::Pipe {
                p.next();
                let right = p.parse_proc1()?;
                left = Proc::PPar(Box::new(left), Box::new(right));
            }
            Ok(left)
        })
    }

    fn parse_proc1(&mut self) -> Result<Proc, RholangError> {
        self.with_depth(|p| {
            if p.eat_ident("if") {
                p.expect(Tok::LParen)?;
                let cond = p.parse_proc()?;
                p.expect(Tok::RParen)?;
                let then = p.parse_proc2()?;
                if p.eat_ident("else") {
                    let els = p.parse_proc1()?;
                    Ok(Proc::PIfElse(Box::new(cond), Box::new(then), Box::new(els)))
                } else {
                    Ok(Proc::PIf(Box::new(cond), Box::new(then)))
                }
            } else if p.eat_ident("new") {
                let mut decls = Vec::new();
                while p.peek() != &Tok::Ident("in".to_string()) {
                    decls.push(p.parse_name_decl()?);
                    if p.peek() == &Tok::Comma {
                        p.next();
                    } else {
                        break;
                    }
                }
                p.eat_ident("in");
                let body = p.parse_proc1()?;
                Ok(Proc::PNew(decls, Box::new(body)))
            } else {
                p.parse_proc2()
            }
        })
    }

    fn parse_proc2(&mut self) -> Result<Proc, RholangError> {
        if self.eat_ident("contract") {
            let name = self.parse_name()?;
            self.expect(Tok::LParen)?;
            let mut names = Vec::new();
            while self.peek() != &Tok::RParen {
                names.push(self.parse_name()?);
                if self.peek() == &Tok::Comma {
                    self.next();
                } else {
                    break;
                }
            }
            let remainder = if self.peek() == &Tok::Ellipsis {
                self.next();
                NameRemainder::NameRemainderVar(self.parse_proc_var()?)
            } else {
                NameRemainder::NameRemainderEmpty
            };
            self.expect(Tok::RParen)?;
            self.expect(Tok::Eq)?;
            self.expect(Tok::LBrace)?;
            let body = self.parse_proc()?;
            self.expect(Tok::RBrace)?;
            Ok(Proc::PContr(name, names, remainder, Box::new(body)))
        } else if self.eat_ident("for") {
            self.expect(Tok::LParen)?;
            let mut receipts = Vec::new();
            while self.peek() != &Tok::RParen {
                receipts.push(self.parse_receipt()?);
                if self.peek() == &Tok::Semicolon {
                    self.next();
                } else {
                    break;
                }
            }
            // A `for()` with zero receipts would desugar to `PInput(vec![], …)`, which the
            // normalizer indexes at `receipts[0]` (panic). Reject it here instead.
            if receipts.is_empty() {
                return Err(RholangError::SyntaxError(
                    "for(...) requires at least one receive".to_string(),
                ));
            }
            self.expect(Tok::RParen)?;
            self.expect(Tok::LBrace)?;
            let body = self.parse_proc()?;
            self.expect(Tok::RBrace)?;
            Ok(Proc::PInput(receipts, Box::new(body)))
        } else if self.eat_ident("select") {
            self.expect(Tok::LBrace)?;
            let mut branches = Vec::new();
            while self.peek() != &Tok::RBrace {
                branches.push(self.parse_branch()?);
            }
            self.expect(Tok::RBrace)?;
            Ok(Proc::PChoice(branches))
        } else if self.eat_ident("match") {
            let target = self.parse_proc4()?;
            self.expect(Tok::LBrace)?;
            let mut cases = Vec::new();
            while self.peek() != &Tok::RBrace {
                cases.push(self.parse_case()?);
            }
            self.expect(Tok::RBrace)?;
            Ok(Proc::PMatch(Box::new(target), cases))
        } else if self.is_bundle() {
            let bundle = self.parse_bundle()?;
            self.expect(Tok::LBrace)?;
            let body = self.parse_proc()?;
            self.expect(Tok::RBrace)?;
            Ok(Proc::PBundle(bundle, Box::new(body)))
        } else if self.eat_ident("let") {
            let decl = self.parse_decl()?;
            let decls = self.parse_decls()?;
            self.eat_ident("in");
            self.expect(Tok::LBrace)?;
            let body = self.parse_proc()?;
            self.expect(Tok::RBrace)?;
            Ok(Proc::PLet(decl, decls, Box::new(body)))
        } else {
            self.parse_proc3()
        }
    }

    fn parse_proc3(&mut self) -> Result<Proc, RholangError> {
        // A send is `Name Send "(" [Proc] ")"`; otherwise fall through to Proc4.
        let is_name_start = match self.peek() {
            Tok::At | Tok::Underscore => true,
            Tok::Ident(s) => !is_reserved(s),
            _ => false,
        };
        if is_name_start {
            let save = self.pos;
            let name = self.parse_name()?;
            if matches!(self.peek(), Tok::Bang | Tok::BangBang) {
                let send = self.parse_send()?;
                self.expect(Tok::LParen)?;
                let mut data = Vec::new();
                while self.peek() != &Tok::RParen {
                    data.push(self.parse_proc()?);
                    if self.peek() == &Tok::Comma {
                        self.next();
                    } else {
                        break;
                    }
                }
                self.expect(Tok::RParen)?;
                return Ok(Proc::PSend(name, send, data));
            }
            // A bare `Var`/`_` in process position is a process-variable reference (`PVar`/
            // `PVarWildcard`), handled by `parse_proc16`; backtrack and fall through. Only `@`
            // (a quoted name) remains a bare-name error.
            if matches!(name, Name::NameVar(_) | Name::NameWildcard) {
                self.pos = save;
            } else {
                return Err(RholangError::SyntaxError(
                    "bare name in process position".into(),
                ));
            }
        }
        self.parse_proc4()
    }

    fn parse_proc4(&mut self) -> Result<Proc, RholangError> {
        let mut left = self.parse_proc5()?;
        loop {
            if self.eat_ident("or") {
                let right = self.parse_proc5()?;
                left = Proc::POr(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::OrOr {
                self.next();
                let right = self.parse_proc5()?;
                left = Proc::PShortOr(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_proc5(&mut self) -> Result<Proc, RholangError> {
        let mut left = self.parse_proc6()?;
        loop {
            if self.eat_ident("and") {
                let right = self.parse_proc6()?;
                left = Proc::PAnd(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::AndAnd {
                self.next();
                let right = self.parse_proc6()?;
                left = Proc::PShortAnd(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_proc6(&mut self) -> Result<Proc, RholangError> {
        let mut left = self.parse_proc7()?;
        loop {
            if self.peek() == &Tok::EqEq {
                self.next();
                let right = self.parse_proc7()?;
                left = Proc::PEq(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::Neq {
                self.next();
                let right = self.parse_proc7()?;
                left = Proc::PNeq(Box::new(left), Box::new(right));
            } else if self.eat_ident("matches") {
                let right = self.parse_proc7()?;
                left = Proc::PMatches(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_proc7(&mut self) -> Result<Proc, RholangError> {
        let mut left = self.parse_proc8()?;
        loop {
            if self.peek() == &Tok::Lt {
                self.next();
                let right = self.parse_proc8()?;
                left = Proc::PLt(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::Lte {
                self.next();
                let right = self.parse_proc8()?;
                left = Proc::PLte(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::Gt {
                self.next();
                let right = self.parse_proc8()?;
                left = Proc::PGt(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::Gte {
                self.next();
                let right = self.parse_proc8()?;
                left = Proc::PGte(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_proc8(&mut self) -> Result<Proc, RholangError> {
        let mut left = self.parse_proc9()?;
        loop {
            if self.peek() == &Tok::Plus {
                self.next();
                let right = self.parse_proc9()?;
                left = Proc::PAdd(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::Minus {
                self.next();
                let right = self.parse_proc9()?;
                left = Proc::PMinus(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::PlusPlus {
                self.next();
                let right = self.parse_proc9()?;
                left = Proc::PPlusPlus(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::MinusMinus {
                self.next();
                let right = self.parse_proc9()?;
                left = Proc::PMinusMinus(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_proc9(&mut self) -> Result<Proc, RholangError> {
        let mut left = self.parse_proc10()?;
        loop {
            if self.peek() == &Tok::Star {
                self.next();
                let right = self.parse_proc10()?;
                left = Proc::PMult(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::Percent {
                self.next();
                let right = self.parse_proc10()?;
                left = Proc::PMod(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::PercentPercent {
                self.next();
                let right = self.parse_proc10()?;
                left = Proc::PPercentPercent(Box::new(left), Box::new(right));
            } else if self.peek() == &Tok::Slash {
                self.next();
                let right = self.parse_proc10()?;
                left = Proc::PDiv(Box::new(left), Box::new(right));
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_proc10(&mut self) -> Result<Proc, RholangError> {
        self.with_depth(|p| {
            if p.eat_ident("not") {
                let q = p.parse_proc10()?;
                Ok(Proc::PNot(Box::new(q)))
            } else if p.peek() == &Tok::Minus {
                p.next();
                let q = p.parse_proc10()?;
                Ok(Proc::PNeg(Box::new(q)))
            } else {
                p.parse_proc11()
            }
        })
    }

    fn parse_proc11(&mut self) -> Result<Proc, RholangError> {
        let mut left = self.parse_proc12()?;
        loop {
            if self.peek() == &Tok::Dot {
                self.next();
                if let Tok::Ident(method) = self.next() {
                    self.expect(Tok::LParen)?;
                    let mut args = Vec::new();
                    while self.peek() != &Tok::RParen {
                        args.push(self.parse_proc()?);
                        if self.peek() == &Tok::Comma {
                            self.next();
                        } else {
                            break;
                        }
                    }
                    self.expect(Tok::RParen)?;
                    left = Proc::PMethod(Box::new(left), method, args);
                } else {
                    return Err(RholangError::SyntaxError("expected method name".into()));
                }
            } else {
                break;
            }
        }
        Ok(left)
    }

    fn parse_proc12(&mut self) -> Result<Proc, RholangError> {
        if self.peek() == &Tok::Star {
            self.next();
            let name = self.parse_name()?;
            Ok(Proc::PEval(name))
        } else {
            self.parse_proc13()
        }
    }

    fn parse_proc13(&mut self) -> Result<Proc, RholangError> {
        if self.peek() == &Tok::Eq {
            // VarRefKindProc Var  or  VarRefKindName Var ("=" "*" Var)
            self.next();
            let kind = if self.peek() == &Tok::Star {
                self.next();
                VarRefKind::VarRefKindName
            } else {
                VarRefKind::VarRefKindProc
            };
            let var = self.parse_source_var()?;
            Ok(Proc::PVarRef(kind, var))
        } else {
            self.parse_proc14()
        }
    }

    fn parse_proc14(&mut self) -> Result<Proc, RholangError> {
        let mut left = self.parse_proc15()?;
        while self.peek() == &Tok::Conj {
            self.next();
            let right = self.parse_proc15()?;
            left = Proc::PConjunction(Box::new(left), Box::new(right));
        }
        Ok(left)
    }

    fn parse_proc15(&mut self) -> Result<Proc, RholangError> {
        self.with_depth(|p| {
            if p.peek() == &Tok::Tilde {
                p.next();
                let q = p.parse_proc15()?;
                Ok(Proc::PNegation(Box::new(q)))
            } else {
                p.parse_proc16()
            }
        })
    }

    fn parse_proc16(&mut self) -> Result<Proc, RholangError> {
        let mut target = if self.peek() == &Tok::LBrace {
            self.parse_braced_or_map()?
        } else if self.eat_ident("Nil") {
            Proc::PNil
        } else if let Some(t) = self.parse_simple_type()? {
            Proc::PSimpleType(t)
        } else if self.is_ground() {
            Proc::PGround(self.parse_ground()?)
        } else if self.is_collection() {
            Proc::PCollect(self.parse_collection()?)
        } else {
            Proc::PVar(self.parse_proc_var()?)
        };
        // Method calls (`receiver.method` / `receiver.method(args...)`) bind tighter than the
        // operators above and chain left-to-right.
        while self.peek() == &Tok::Dot {
            self.next();
            let method = self.parse_source_var()?;
            let args = if self.peek() == &Tok::LParen {
                self.next();
                let mut args = Vec::new();
                while self.peek() != &Tok::RParen {
                    args.push(self.parse_proc()?);
                    if self.peek() == &Tok::Comma {
                        self.next();
                    } else {
                        break;
                    }
                }
                self.expect(Tok::RParen)?;
                args
            } else {
                Vec::new()
            };
            target = Proc::PMethod(Box::new(target), method, args);
        }
        Ok(target)
    }

    /// Disambiguate `{ Proc }` (a braced process) from `{ KeyValuePair, ... }` (a map collection).
    /// Both productions start with `{` at the same precedence (`Proc16`), so the parser decides
    /// after reading the first process: a following `:` marks a map; otherwise it is a braced
    /// process. Empty braces are an empty map (there is no empty braced process).
    fn parse_braced_or_map(&mut self) -> Result<Proc, RholangError> {
        self.expect(Tok::LBrace)?;
        if self.peek() == &Tok::RBrace {
            self.next();
            return Ok(Proc::PCollect(Collection::CollectMap(
                Vec::new(),
                ProcRemainder::ProcRemainderEmpty,
            )));
        }
        let first = self.parse_proc()?;
        if self.peek() != &Tok::Colon {
            self.expect(Tok::RBrace)?;
            return Ok(first);
        }
        let mut kvs = Vec::new();
        let mut key = first;
        loop {
            self.expect(Tok::Colon)?;
            let value = self.parse_proc()?;
            kvs.push(KeyValuePair(key, value));
            if self.peek() == &Tok::Comma {
                self.next();
                if self.peek() == &Tok::RBrace || self.peek() == &Tok::Ellipsis {
                    break;
                }
                key = self.parse_proc()?;
            } else {
                break;
            }
        }
        let remainder = self.parse_proc_remainder()?;
        self.expect(Tok::RBrace)?;
        Ok(Proc::PCollect(Collection::CollectMap(kvs, remainder)))
    }

    fn parse_source_var(&mut self) -> Result<String, RholangError> {
        match self.next() {
            Tok::Ident(s) => Ok(s),
            Tok::Underscore => Ok("_".to_string()),
            t => Err(RholangError::SyntaxError(format!("expected variable, got {t:?}"))),
        }
    }

    fn parse_proc_var(&mut self) -> Result<ProcVar, RholangError> {
        if self.peek() == &Tok::Underscore {
            self.next();
            Ok(ProcVar::ProcVarWildcard)
        } else {
            Ok(ProcVar::ProcVarVar(self.parse_source_var()?))
        }
    }

    fn parse_name(&mut self) -> Result<Name, RholangError> {
        self.with_depth(|p| {
            if p.peek() == &Tok::Underscore {
                p.next();
                Ok(Name::NameWildcard)
            } else if p.peek() == &Tok::At {
                p.next();
                let q = p.parse_proc12()?;
                Ok(Name::NameQuote(Box::new(q)))
            } else {
                Ok(Name::NameVar(p.parse_source_var()?))
            }
        })
    }

    fn parse_send(&mut self) -> Result<Send, RholangError> {
        match self.next() {
            Tok::Bang => Ok(Send::SendSingle),
            Tok::BangBang => Ok(Send::SendMultiple),
            t => Err(RholangError::SyntaxError(format!("expected send, got {t:?}"))),
        }
    }

    fn parse_simple_type(&mut self) -> Result<Option<SimpleType>, RholangError> {
        let t = match self.peek() {
            Tok::Ident(s) if s == "Bool" => Some(SimpleType::SimpleTypeBool),
            Tok::Ident(s) if s == "Int" => Some(SimpleType::SimpleTypeInt),
            Tok::Ident(s) if s == "BigInt" => Some(SimpleType::SimpleTypeBigInt),
            Tok::Ident(s) if s == "String" => Some(SimpleType::SimpleTypeString),
            Tok::Ident(s) if s == "Uri" => Some(SimpleType::SimpleTypeUri),
            Tok::Ident(s) if s == "ByteArray" => Some(SimpleType::SimpleTypeByteArray),
            _ => None,
        };
        if t.is_some() {
            self.next();
        }
        Ok(t)
    }

    fn is_ground(&self) -> bool {
        match self.peek() {
            Tok::Long(_) | Tok::Str(_) | Tok::Uri(_) => true,
            Tok::Ident(s) => s == "true" || s == "false",
            _ => false,
        }
    }

    fn parse_ground(&mut self) -> Result<Ground, RholangError> {
        match self.next() {
            Tok::Long(n) => Ok(Ground::GroundInt(n.to_string())),
            Tok::Str(s) => Ok(Ground::GroundString(s)),
            Tok::Uri(u) => Ok(Ground::GroundUri(u)),
            Tok::Ident(s) if s == "true" => Ok(Ground::GroundBool(BoolLiteral::BoolTrue)),
            Tok::Ident(s) if s == "false" => Ok(Ground::GroundBool(BoolLiteral::BoolFalse)),
            t => Err(RholangError::SyntaxError(format!("unexpected ground {t:?}"))),
        }
    }

    fn is_collection(&self) -> bool {
        match self.peek() {
            Tok::LBracket | Tok::LParen | Tok::LBrace => true,
            Tok::Ident(s) => s == "Set",
            _ => false,
        }
    }

    fn parse_collection(&mut self) -> Result<Collection, RholangError> {
        match self.peek().clone() {
            Tok::LBracket => {
                self.next();
                let mut procs = Vec::new();
                while self.peek() != &Tok::RBracket {
                    procs.push(self.parse_proc()?);
                    if self.peek() == &Tok::Comma {
                        self.next();
                    } else {
                        break;
                    }
                }
                let remainder = self.parse_proc_remainder()?;
                self.expect(Tok::RBracket)?;
                Ok(Collection::CollectList(procs, remainder))
            }
            Tok::LParen => {
                self.next();
                let first = self.parse_proc()?;
                if self.peek() == &Tok::Comma {
                    self.next();
                    if self.peek() == &Tok::RParen {
                        self.next();
                        Ok(Collection::CollectTuple(Tuple::TupleSingle(Box::new(first))))
                    } else {
                        let mut rest = Vec::new();
                        while self.peek() != &Tok::RParen {
                            rest.push(self.parse_proc()?);
                            if self.peek() == &Tok::Comma {
                                self.next();
                            } else {
                                break;
                            }
                        }
                        self.expect(Tok::RParen)?;
                        Ok(Collection::CollectTuple(Tuple::TupleMultiple(
                            Box::new(first),
                            rest,
                        )))
                    }
                } else {
                    self.expect(Tok::RParen)?;
                    Ok(Collection::CollectTuple(Tuple::TupleSingle(Box::new(first))))
                }
            }
            Tok::LBrace => {
                self.next();
                let mut kvs = Vec::new();
                while self.peek() != &Tok::RBrace {
                    let k = self.parse_proc()?;
                    self.expect(Tok::Colon)?;
                    let v = self.parse_proc()?;
                    kvs.push(KeyValuePair(k, v));
                    if self.peek() == &Tok::Comma {
                        self.next();
                    } else {
                        break;
                    }
                }
                let remainder = self.parse_proc_remainder()?;
                self.expect(Tok::RBrace)?;
                Ok(Collection::CollectMap(kvs, remainder))
            }
            Tok::Ident(s) if s == "Set" => {
                self.next();
                self.expect(Tok::LParen)?;
                let mut procs = Vec::new();
                while self.peek() != &Tok::RParen {
                    procs.push(self.parse_proc()?);
                    if self.peek() == &Tok::Comma {
                        self.next();
                    } else {
                        break;
                    }
                }
                let remainder = self.parse_proc_remainder()?;
                self.expect(Tok::RParen)?;
                Ok(Collection::CollectSet(procs, remainder))
            }
            _ => Err(RholangError::SyntaxError("expected collection".into())),
        }
    }

    fn parse_proc_remainder(&mut self) -> Result<ProcRemainder, RholangError> {
        if self.peek() == &Tok::Ellipsis {
            self.next();
            Ok(ProcRemainder::ProcRemainderVar(self.parse_proc_var()?))
        } else {
            Ok(ProcRemainder::ProcRemainderEmpty)
        }
    }

    fn is_bundle(&self) -> bool {
        matches!(self.peek(), Tok::Ident(s) if s == "bundle")
    }

    fn parse_bundle(&mut self) -> Result<Bundle, RholangError> {
        // The lexer emits `bundle+`/`bundle-`/`bundle0` as `Ident("bundle")` followed by the suffix
        // token, so read the optional suffix to pick the bundle kind.
        match self.next() {
            Tok::Ident(s) if s == "bundle" => match self.peek() {
                Tok::Plus => {
                    self.next();
                    Ok(Bundle::BundleWrite)
                }
                Tok::Minus => {
                    self.next();
                    Ok(Bundle::BundleRead)
                }
                Tok::Long(0) => {
                    self.next();
                    Ok(Bundle::BundleEquiv)
                }
                _ => Ok(Bundle::BundleReadWrite),
            },
            t => Err(RholangError::SyntaxError(format!("expected bundle, got {t:?}"))),
        }
    }

    fn parse_name_decl(&mut self) -> Result<NameDecl, RholangError> {
        let var = self.parse_source_var()?;
        if self.peek() == &Tok::LParen {
            self.next();
            if let Tok::Uri(u) = self.next() {
                self.expect(Tok::RParen)?;
                Ok(NameDecl::NameDeclUrn(var, u))
            } else {
                Err(RholangError::SyntaxError("expected uri in name decl".into()))
            }
        } else {
            Ok(NameDecl::NameDeclSimpl(var))
        }
    }

    fn parse_receipt(&mut self) -> Result<Receipt, RholangError> {
        // Distinguish the arrow: linear "<-", peek "<<-", repeated "<=".
        let (names, remainder) = self.parse_bind_head()?;
        match self.peek() {
            Tok::LArrow => {
                self.next();
                let source = self.parse_name_source()?;
                let mut binds = vec![LinearBind(names, remainder, source)];
                while self.peek() == &Tok::Amp {
                    self.next();
                    let (n, r) = self.parse_bind_head()?;
                    self.expect(Tok::LArrow)?;
                    let s = self.parse_name_source()?;
                    binds.push(LinearBind(n, r, s));
                }
                Ok(Receipt::ReceiptLinear(ReceiptLinearImpl::LinearSimple(binds)))
            }
            Tok::LLArrow => {
                self.next();
                let source = self.parse_name()?;
                let mut binds = vec![PeekBind(names, remainder, source)];
                while self.peek() == &Tok::Amp {
                    self.next();
                    let (n, r) = self.parse_bind_head()?;
                    self.expect(Tok::LLArrow)?;
                    let s = self.parse_name()?;
                    binds.push(PeekBind(n, r, s));
                }
                Ok(Receipt::ReceiptPeek(ReceiptPeekImpl::PeekSimple(binds)))
            }
            Tok::Lte => {
                self.next();
                let source = self.parse_name()?;
                let mut binds = vec![RepeatedBind(names, remainder, source)];
                while self.peek() == &Tok::Amp {
                    self.next();
                    let (n, r) = self.parse_bind_head()?;
                    self.expect(Tok::Lte)?;
                    let s = self.parse_name()?;
                    binds.push(RepeatedBind(n, r, s));
                }
                Ok(Receipt::ReceiptRepeated(ReceiptRepeatedImpl::RepeatedSimple(binds)))
            }
            t => Err(RholangError::SyntaxError(format!(
                "expected <-, <<-, or <=, got {t:?}"
            ))),
        }
    }

    fn parse_bind_head(&mut self) -> Result<(Vec<Name>, NameRemainder), RholangError> {
        let mut names = Vec::new();
        while !matches!(self.peek(), Tok::LArrow | Tok::LLArrow | Tok::Lte | Tok::Ellipsis) {
            names.push(self.parse_name()?);
            if self.peek() == &Tok::Comma {
                self.next();
            } else {
                break;
            }
        }
        let remainder = if self.peek() == &Tok::Ellipsis {
            self.next();
            NameRemainder::NameRemainderVar(self.parse_proc_var()?)
        } else {
            NameRemainder::NameRemainderEmpty
        };
        Ok((names, remainder))
    }

    fn parse_name_source(&mut self) -> Result<NameSource, RholangError> {
        let name = self.parse_name()?;
        if self.peek() == &Tok::BangQ {
            self.next();
            self.expect(Tok::LParen)?;
            let mut procs = Vec::new();
            while self.peek() != &Tok::RParen {
                procs.push(self.parse_proc()?);
                if self.peek() == &Tok::Comma {
                    self.next();
                } else {
                    break;
                }
            }
            self.expect(Tok::RParen)?;
            Ok(NameSource::SendReceiveSource(name, procs))
        } else if self.peek() == &Tok::QMark {
            self.next();
            Ok(NameSource::ReceiveSendSource(name))
        } else {
            Ok(NameSource::SimpleSource(name))
        }
    }

    fn parse_branch(&mut self) -> Result<Branch, RholangError> {
        let (names, remainder) = self.parse_bind_head()?;
        self.expect(Tok::LArrow)?;
        let source = self.parse_name_source()?;
        self.expect(Tok::Arrow)?;
        let body = self.parse_proc3()?;
        Ok(Branch(
            ReceiptLinearImpl::LinearSimple(vec![LinearBind(names, remainder, source)]),
            Box::new(body),
        ))
    }

    fn parse_case(&mut self) -> Result<Case, RholangError> {
        let pattern = self.parse_proc13()?;
        self.expect(Tok::Arrow)?;
        let body = self.parse_proc3()?;
        Ok(Case(Box::new(pattern), Box::new(body)))
    }

    fn parse_decl(&mut self) -> Result<Decl, RholangError> {
        let mut names = Vec::new();
        while self.peek() != &Tok::LArrow && self.peek() != &Tok::Ellipsis {
            names.push(self.parse_name()?);
            if self.peek() == &Tok::Comma {
                self.next();
            } else {
                break;
            }
        }
        let remainder = if self.peek() == &Tok::Ellipsis {
            self.next();
            NameRemainder::NameRemainderVar(self.parse_proc_var()?)
        } else {
            NameRemainder::NameRemainderEmpty
        };
        self.expect(Tok::LArrow)?;
        let mut procs = Vec::new();
        while self.peek() != &Tok::Semicolon
            && self.peek() != &Tok::Amp
            && self.peek() != &Tok::Ident("in".to_string())
        {
            procs.push(self.parse_proc()?);
            if self.peek() == &Tok::Comma {
                self.next();
            } else {
                break;
            }
        }
        Ok(Decl(names, remainder, procs))
    }

    fn parse_decls(&mut self) -> Result<Decls, RholangError> {
        if self.peek() == &Tok::Semicolon {
            self.next();
            let mut decls = Vec::new();
            while self.peek() != &Tok::Ident("in".to_string()) {
                decls.push(LinearDecl(self.parse_decl()?));
                if self.peek() == &Tok::Semicolon {
                    self.next();
                } else {
                    break;
                }
            }
            Ok(Decls::LinearDeclsImpl(decls))
        } else if self.peek() == &Tok::Amp {
            self.next();
            let mut decls = Vec::new();
            while self.peek() != &Tok::Ident("in".to_string()) {
                decls.push(ConcDecl(self.parse_decl()?));
                if self.peek() == &Tok::Amp {
                    self.next();
                } else {
                    break;
                }
            }
            Ok(Decls::ConcDeclsImpl(decls))
        } else {
            Ok(Decls::EmptyDeclImpl)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_nil() {
        let p = parse("Nil").unwrap();
        assert_eq!(p, Proc::PNil);
    }

    #[test]
    fn rejects_excessive_nesting_depth() {
        // Deeply nested `new`/`not`/`~` terms must error rather than overflow the stack.
        let deep_new = format!("{}Nil", "new x in ".repeat(MAX_PARSE_DEPTH + 10));
        assert!(parse(&deep_new).is_err());

        let deep_not = format!("{}Nil", "not ".repeat(MAX_PARSE_DEPTH + 10));
        assert!(parse(&deep_not).is_err());
    }

    #[test]
    fn parses_int() {
        let p = parse("42").unwrap();
        assert_eq!(p, Proc::PGround(Ground::GroundInt("42".to_string())));
    }

    #[test]
    fn parses_send() {
        let p = parse("x!(1)").unwrap();
        assert_eq!(
            p,
            Proc::PSend(
                Name::NameVar("x".to_string()),
                Send::SendSingle,
                vec![Proc::PGround(Ground::GroundInt("1".to_string()))],
            )
        );
    }

    #[test]
    fn parses_arith_precedence() {
        let p = parse("1 + 2 * 3").unwrap();
        assert_eq!(
            p,
            Proc::PAdd(
                Box::new(Proc::PGround(Ground::GroundInt("1".to_string()))),
                Box::new(Proc::PMult(
                    Box::new(Proc::PGround(Ground::GroundInt("2".to_string()))),
                    Box::new(Proc::PGround(Ground::GroundInt("3".to_string()))),
                )),
            )
        );
    }
}
