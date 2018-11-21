package coop.rchain.rholang.parser.lexer;

import java.util.regex.Pattern;

import static java.util.regex.Pattern.compile;

class LexerConst {
    static final String NOTE_PREFIX = "lexer.note.";
    static final String WARN_PREFIX = "lexer.warn.";
    static final String ERROR_PREFIX = "lexer.err.";

    static final String FILE_LEXER_NOTES = "/lexer-notes.properties";
    static final String FILE_LEXER_WARNS = "/lexer-warns.properties";
    static final String FILE_LEXER_ERRORS = "/lexer-errors.properties";

    static final Pattern LONG
            = compile("^[0-9]+" + "[lL]");

    static final Pattern INT
            = compile("^[0-9]+");

    static final String EXP = "([eE][-+]?[0-9]+)";
    static final String _DOT = "(\\.[0-9]*)";
    static final String TAIL = "[fFdD]";

    static final Pattern TRUE_FLOATING_POINT = compile("^[0-9]+"
            + "("
            + "(" + _DOT + EXP + "?" + TAIL + "?" + ")" + "|"
            + "(" + _DOT + "?" + EXP + TAIL + "?" + ")" + "|"
            + "(" + _DOT + "?" + EXP + "?" + TAIL + ")"
            + ")"
    );

    static final Pattern FRACTION_AND_SUFFIX
            = compile("^(\\.[0-9]+)" + EXP + "?" + TAIL + "?");
}
