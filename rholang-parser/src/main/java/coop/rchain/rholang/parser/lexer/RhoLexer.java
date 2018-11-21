package coop.rchain.rholang.parser.lexer;

import coop.rchain.rholang.parser.RhoToken;
import coop.rchain.rholang.parser.RhoTokenType;
import coop.rchain.rholang.parser.log.Diagnostic;
import coop.rchain.rholang.parser.log.DiagnosticListener;
import coop.rchain.rholang.parser.log.LineMap;
import coop.rchain.rholang.parser.log.impl.LineMapImpl;
import coop.rchain.rholang.parser.log.impl.NopListener;
import coop.rchain.rholang.parser.msg.AbsentKeywords;
import coop.rchain.rholang.parser.msg.MisspelledKeywords;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Properties;
import java.util.function.Supplier;
import java.util.regex.Matcher;

import static java.lang.Character.*;
import static java.lang.String.format;
import static java.util.Arrays.asList;
import static java.util.regex.Pattern.compile;
import static java.util.stream.Stream.generate;
import static coop.rchain.rholang.parser.RhoTokenType.*;
import static coop.rchain.rholang.parser.lexer.LexerConst.*;
import static coop.rchain.rholang.parser.log.Diagnostic.Kind.ERROR;
import static coop.rchain.rholang.parser.log.Diagnostic.Kind.*;

public class RhoLexer {
    private final LineMap lineMap;
    private final DiagnosticListener listener;
    private final Properties messages;
    //
    // need this for Long.MIN_VALUE parsing workaround
    private RhoToken prevToken = null;
    private LexerState state;
    //

    public RhoLexer(String content) {
        this(content, new NopListener(), loadDefaultMessages());
    }

    public RhoLexer(String content, DiagnosticListener listener) {
        this(content, listener, loadDefaultMessages());
    }

    public RhoLexer(String content, DiagnosticListener listener, Properties messages) {
        this.state = new LexerState(content);
        this.listener = listener;
        this.messages = messages;
        this.lineMap = new LineMapImpl(content);
    }

    private static Properties loadDefaultMessages() {
        try {
            Properties result = new Properties();
            result.load(RhoLexer.class.getResourceAsStream(FILE_LEXER_NOTES));
            result.load(RhoLexer.class.getResourceAsStream(FILE_LEXER_WARNS));
            result.load(RhoLexer.class.getResourceAsStream(FILE_LEXER_ERRORS));
            return result;
        } catch (IOException ex) {
            throw new RuntimeException(ex); //todo: RuntimeException?
        }
    }

    public RhoToken readToken() {
        return prevToken = readToken0();
    }

    private RhoToken readToken0() {
        cleanMem();

        if (state.isEOF()) {
            listener.eof();
            return EOF.T;
        }

        if (state.isLetter()) {
            return processLetter();
        }

        return match(
                _default(this::processDefault),
                // ========== Whitespaces, Line terminations
                _case(" ", this::readToken),
                _case("\t", this::readToken),
                _case("\f", this::readToken),
                _case("\n", this::readToken),
                _case("\r\n", this::readToken), //todo:remove
                _case("\r", this::readToken),

                // ========== Under
                _case("_", this::processUnder),

                // ========== String-like
                _case("\"", this::processString),
                _case("`", this::processUri),
                _case("'", this::processQuote),

                // ========== Separators: (, ), [, ], {, }
                _case("(", () -> LPAREN.T),
                _case(")", () -> RPAREN.T),
                _case("[", () -> LBRACKET.T),
                _case("]", () -> RBRACKET.T),
                _case("{", () -> LBRACE.T),
                _case("}", () -> RBRACE.T),

                // ========== Digits
                _case("0x", this::readNumberHex),
                _case("0X", this::readNumberHex),
                _case("0", () -> memChars(0), this::readNumberDecimal),
                _case("1", () -> memChars(0), this::readNumberDecimal),
                _case("2", () -> memChars(0), this::readNumberDecimal),
                _case("3", () -> memChars(0), this::readNumberDecimal),
                _case("4", () -> memChars(0), this::readNumberDecimal),
                _case("5", () -> memChars(0), this::readNumberDecimal),
                _case("6", () -> memChars(0), this::readNumberDecimal),
                _case("7", () -> memChars(0), this::readNumberDecimal),
                _case("8", () -> memChars(0), this::readNumberDecimal),
                _case("9", () -> memChars(0), this::readNumberDecimal),

                // ========== Separators: . , ; : =
                _case("...", () -> ELLIPSIS.T),
                _case("..", err("operator.absent.dot-dot")),
                _case(".0", () -> memChars(0), this::readFractionAndSuffix), //todo: remove memChars
                _case(".1", () -> memChars(0), this::readFractionAndSuffix),
                _case(".2", () -> memChars(0), this::readFractionAndSuffix),
                _case(".3", () -> memChars(0), this::readFractionAndSuffix),
                _case(".4", () -> memChars(0), this::readFractionAndSuffix),
                _case(".5", () -> memChars(0), this::readFractionAndSuffix),
                _case(".6", () -> memChars(0), this::readFractionAndSuffix),
                _case(".7", () -> memChars(0), this::readFractionAndSuffix),
                _case(".8", () -> memChars(0), this::readFractionAndSuffix),
                _case(".9", () -> memChars(0), this::readFractionAndSuffix),
                _case(".", () -> DOT.T),

                _case(",", () -> COMMA.T),
                _case(";", () -> SEMI.T),
                _case("::", err("operator.absent.colon-colon")),
                _case(":", () -> COLON.T),
                _case("===", err("operator.absent.eq")), // todo: add =/=
                _case("==", () -> EQ_EQ.T),
                _case("=>", () -> ARROW.T),
                _case("=", () -> EQ.T),

                // ========== Operators (used): +, -, *, /, >, <, |, !, ~, @
                _case("++", () -> PLUS_PLUS.T),
                _case("+=", err("operator.absent.compound-assignment")),
                _case("+", () -> PLUS.T),
                _case("--", () -> MINUS_MINUS.T),
                _case("-=", err("operator.absent.compound-assignment")),
                _case("->", err("operator.absent.arrow")),
                _case("-", () -> MINUS.T),
                _case("**", err("operator.absent.pow")),
                _case("*=", err("operator.absent.compound-assignment")),
                _case("*", () -> STAR.T),
                _case("/\\", () -> CONJUNCTION.T),
                _case("//", () -> skipChars(2), this::processCommentLine),
                _case("/*", () -> memChars(2), this::processCommentBlock),
                _case("/", () -> DIV.T),
                _case("\\/", () -> DISJUNCTION.T),
                _case("\\", err("operator.absent.back-slash")),
                _case("<<<", err("operator.absent.arithmetic")),
                _case("<<", err("operator.absent.arithmetic")),
                _case("<~", err("operator.absent.arrow")),
                _case("<=", () -> BACK_ARROW.T),
                _case("<-", () -> BIND_LINEAR.T),
                _case("<", () -> LT.T),
                _case(">>>", err("operator.absent.arithmetic")),
                _case(">>", err("operator.absent.arithmetic")),
                _case(">=", () -> GT_EQ.T),
                _case(">", () -> GT.T),
                _case("||", err("operator.absent.logic")),
                _case("|", () -> PAR.T),
                _case("!!", () -> SEND_MULTIPLE.T),
                _case("!=", () -> NOT_EQ.T),
                _case("!", () -> SEND_SINGLE.T),
                _case("~>", err("operator.absent.arrow")),
                _case("~", () -> TILDE.T),
                _case("@", () -> QUOTE.T),

                // ========== Operators (unused): &, %, ^, ?, #, $
                _case("&&", err("operator.absent.logic")),
                _case("&", err("operator.absent.logic")),
                _case("%%", () -> PERCENT_PERCENT.T),
                _case("%", err("operator.absent.arithmetic")),
                _case("^^", err("operator.absent.pow")),
                _case("^", err("operator.absent.pow")),
                _case("??", err("operator.absent")),
                _case("?", err("operator.absent")),
                _case("##", err("operator.absent")),
                _case("#", err("operator.absent")),
                _case("$$", err("operator.absent")),
                _case("$", err("operator.absent"))
        );
    }

    public List<RhoToken> readAll() {
        LinkedList<RhoToken> acc = new LinkedList<>();
        return generate(this::readToken)
                .map(e -> {
                    acc.addLast(e);
                    return acc;
                })
                .filter(deq -> deq.getLast() == EOF.T)
                .findFirst().get();
    }

    // ============================== Numbers

    /**
     * Read a decimal number (64-bit integer without 'l'/'L' at the end).
     */
    private RhoToken readNumberDecimal() {
        // === Try integer number with 'l'/'L' postfix (Java long literal)
        Matcher longMatcher = LexerConst.LONG.matcher(state.content);
        if (longMatcher.find()) {
            memChars(longMatcher.toMatchResult().end());
            return lexError("literal.absent.int-L-suffix");
        }

        // === Try "true" floating-point number (without integers like '123')
        Matcher fpMatcher = TRUE_FLOATING_POINT.matcher(state.content);
        if (fpMatcher.find()) {
            memChars(fpMatcher.toMatchResult().end());
            return lexError("literal.absent.floating-point");
        }

        // === Int literal?
        while (state.isDigit()) {
            memChars(1);
        }
        // Workaround for Long.MIN_VALUE
        String prefix = (prevToken == MINUS.T) ? "-" : "";
        try {
            Long.parseLong(prefix + state.mem);
            return LITERAL_INT.T(state.mem);
        } catch (NumberFormatException ex) {
            return lexError("literal.int-too-big");
        }
    }

    /**
     * Read fractional part and 'd'/'D' or 'f'/'F' suffix of floating point number.
     */
    private RhoToken readFractionAndSuffix() {
        Matcher matcher = FRACTION_AND_SUFFIX.matcher(state.content);
        if (matcher.find()) {
            memChars(matcher.toMatchResult().end());
            return lexError("literal.absent.floating-point");
        } else {
            throw new IllegalStateException("Called for inappropriate state.content");
        }


//        if (res.find()) {
//            System.out.println(m.toMatchResult().start());
//            System.out.println(m.toMatchResult().end());
//        } else {
//
//        }

//        return p.matcher(state.content).find();

//        while (state.isDigit()) {
//            memChars(1);
//        }
//
//        if (state.isOneOf('e', 'E')) {
//            memChars(1);
//        }
//
//        if (state.isOneOf('-', '+')) {
//            memChars(1);
//        }
//
//        while (state.isDigit()) {
//            memChars(1);
//        }
//
//        if (state.isOneOf('f', 'F', 'd', 'D')) {
//            memChars(1);
//        }
//
//        return state.lexError("literal.absent.floating-point");
    }

    /**
     * Read a hex number.
     */
    private RhoToken readNumberHex() {

        while (!state.isEOF() && (state.isDigit()
                || state.isOneOf('a', 'b', 'c', 'd', 'e', 'f')
                || state.isOneOf('A', 'B', 'C', 'D', 'E', 'F'))) {
            memChars(1);
        }

        return lexError("literal.absent.int-hex-format");
    }

    // ============================== Identifiers
    private RhoToken processLetter() {
        while (state.isLetter() || state.isDigit() || state.is('_')) {
            memChars(1);
        }

        // 'bundle0', 'bundle+', 'bundle-'
        if (state.mem.equals("bundle") && state.isOneOf('0', '+', '-')) {
            memChars(1);
        }

        RhoToken result = keywordOrIdent(state.mem);

        if (result.type.group == TokenGroup.Identifier) {
            // check: identifier like keyword (typo)
            MisspelledKeywords.tryCorrectMisspelled(state.mem)
                    .ifPresent(c -> lexWarn("identifier.like-existing-keyword", c));

            // check: identifier like absent keyword (language misunderstanding)
            if (AbsentKeywords.contains(state.mem)) {
                lexNote("identifier.like-absent-keyword");
            }
        }

        return result;
    }

    private RhoToken processUnder() {
        while (state.isLetter() || state.isDigit() || state.is('_')) {
            memChars(1);
        }

        return (state.mem.equals("_")) ? WILDCARD.T : keywordOrIdent(state.mem);
    }

    // ============================== Comments
    private RhoToken processCommentLine() {
        while (!state.isEOF() && !state.isCRLF()) {
            skipChars(1);
        }
        if (!state.isEOF()) {
            skipChars(1);
        }
        return readToken();
    }

    private RhoToken processCommentBlock() {
        while (!state.isEOF() && !state.isCRLF()) {
            if (state.is('*')) {     // '*'
                memChars(1);
                if (state.is('/')) { // '*/'
                    skipChars(1);
                    return readToken();
                }
            } else {
                memChars(1);
            }
        }

        return lexError("comment.unclosed");
    }

    // ============================== String-like
    // todo: warn UTF-unsupported - lambda in string literal
    private RhoToken processString() {
        while (!state.isEOF() && !state.isOneOf('"', '\r', '\n')) {
            memChars(1);
        }
        if (state.is('"')) {
            memChars(1);
            return LITERAL_STRING.T(state.mem.substring(1, state.mem.length() - 1));
        } else {
            return lexError("literal.string.unclosed");
        }
    }

    private RhoToken processUri() {
        while (!state.isEOF() && !state.isOneOf('`', '\r', '\n')) {
            memChars(1);
        }
        if (state.is('`')) {
            memChars(1);
            return LITERAL_URI.T(state.mem.substring(1, state.mem.length() - 1));
        } else {
            return lexError("literal.uri.unclosed");
        }
    }

    private RhoToken processQuote() {
        while (!state.isEOF() && !state.isOneOf('\'', '\r', '\n')) {
            memChars(1);
        }
        if (state.is('\'')) {
            memChars(1);
            return lexError("literal.absent.single-quote");
        } else {
            restoreMem();
            memChars(1);
            return lexError("operator.absent.single-quote");
        }
    }

    // ============================== Unicode + internals
    private RhoToken processDefault() {
        char ch = state.content.charAt(0);
        String codepoint = String.valueOf((int) ch);
        String hex = format("'\\u%04X'", (int) ch);
        memChars(1);

        // (no surrogate/supplementary checking)
        if (ch <= 127) {
            // Illegal ASCII: 0 <= ch0 < 32
            if (ch < ' ' || ch == 127) {
                return lexError("codepoint.illegal.ascii", hex, codepoint);
            } else {
                throw new AssertionError(hex + " should be processed in readToken()");
            }
        } else if (isDefined(ch)) {
            // Correct but illegal 1-char Unicode: 128 <= ch0 && isDefined(ch0)
            return lexError("codepoint.illegal.unicode-1-char", "" + ch, hex, codepoint);
        } else {
            // Incorrect 1-char unicode: 128 <= ch0 && !isDefined(ch0)
            return lexError("codepoint.illegal.undefined", hex);
        }
    }

    // ============================================================
    // =====================       DSL       ======================
    // ============================================================
    private RhoToken match(Default _default, Case... cases) {
        // Pattern Matching on _case type
        for (Case _case : cases) {
            // === CaseToken
            if (_case instanceof CaseToken) {
                if (state.content.startsWith(_case.prefix)) {
                    if (((CaseToken) _case).task == NOP) {
                        memChars(_case.prefix.length());
                    } else {
                        ((CaseToken) _case).task.run();
                    }
                    return ((CaseToken) _case).tokenSrc.get();
                }
                // === CaseErr
            } else if (_case instanceof CaseErr) {
                if (state.content.startsWith(_case.prefix)) {
                    memChars(_case.prefix.length());
                    return lexError(((CaseErr) _case).err.code, ((CaseErr) _case).err.args);
                }
            } else {
                throw new AssertionError("Any Case is CaseToken or CaseErr (closed hierarchy) not " + _case.getClass());
            }
        }

        return _default.st.get();
    }

    private static CaseToken _case(String s, Runnable r, Supplier<RhoToken> st) {
        return new CaseToken(s, r, st);
    }

    private static CaseToken _case(String s, Supplier<RhoToken> st) {
        return new CaseToken(s, NOP, st);
    }

    private static CaseErr _case(String s, Err err) {
        return new CaseErr(s, err);
    }

    private static abstract class Case {
        final String prefix;

        public Case(String prefix) {
            this.prefix = prefix;
        }
    }

    private static class Default {
        final Supplier<RhoToken> st;

        public Default(Supplier<RhoToken> st) {
            this.st = st;
        }
    }

    private static Default _default(Supplier<RhoToken> st) {
        return new Default(st);
    }

    private static class CaseErr extends Case {
        final Err err;

        public CaseErr(String prefix, Err err) {
            super(prefix);
            this.err = err;
        }
    }

    private static class CaseToken extends Case {
        final Runnable task;
        final Supplier<RhoToken> tokenSrc;

        CaseToken(String prefix, Runnable task, Supplier<RhoToken> tokenSrc) {
            super(prefix);
            this.task = task;
            this.tokenSrc = tokenSrc;
        }
    }

    private static class Err {
        final String code;
        final String[] args;

        Err(String code, String[] args) {
            this.code = code;
            this.args = args;
        }
    }

    private Err err(String code, String... args) {
        return new Err(code, args);
    }

    private static final Runnable NOP = () -> {
    };

    // ============================================================
    // =====================  STATE MACHINE  ======================
    // ============================================================
    private void cleanMem() {
        this.state = new LexerState(
                state.offset,
                state.content,
                "");
    }

    private void memChars(int count) {
        this.state = new LexerState(
                state.offset + count,
                state.content.substring(count),
                state.mem + state.content.substring(0, count));
    }

    private void skipChars(int count) {
        this.state = new LexerState(
                state.offset + count,
                state.content.substring(count),
                "");
    }

    private void restoreMem() {
        this.state = new LexerState(
                state.offset - state.mem.length(),
                state.mem + state.content,
                "");
    }

    // ============================================================
    // ==================  DIAGNOSTIC MESSAGES  ===================
    // ============================================================
    public RhoToken lexNote(String code, String... args) {
        return lexMsg(NOTE, NOTE_PREFIX + code, args);
    }

    public RhoToken lexWarn(String code, String... args) {
        return lexMsg(WARN, WARN_PREFIX + code, args);
    }

    public RhoToken lexError(String code, String... args) {
        return lexMsg(ERROR, ERROR_PREFIX + code, args);
    }

    private RhoToken lexMsg(Diagnostic.Kind kind, String code, String... args) {

        String[] argsWithFirstMem = new String[args.length + 1];
        argsWithFirstMem[0] = state.mem;
        System.arraycopy(args, 0, argsWithFirstMem, 1, args.length);

        RhoLexer.this.listener.report(new Diagnostic(
                kind,
                code,
                format(messages.getProperty(code), (Object[]) argsWithFirstMem),
                messages.getProperty(code),
                asList(argsWithFirstMem),
                //
                lineMap.offsetToSrcLine(state.offset - state.mem.length()),
                lineMap.offsetToCol(state.offset - state.mem.length()),
                state.mem.length(),
                //
                state.offset - state.mem.length(),
                lineMap.offsetToRow(state.offset - state.mem.length())));

        return RhoTokenType.ERROR.T(state.mem);
    }
}