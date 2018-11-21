package coop.rchain.rholang.parser.lexer;

class LexerState {
    final int offset;
    final String content;
    final String mem;

    LexerState(String content) {
        this(0, content, "");
    }

    LexerState(int offset, String content, String mem) {
        this.offset = offset;
        this.content = content;
        this.mem = mem;
    }

    boolean isEOF() {
        return content.isEmpty();
    }

    char currentChar() {
        return content.charAt(0);
    }

    boolean isCRLF() {
        return !isEOF() && (currentChar() == '\r' || currentChar() == '\n');
    }

    boolean isDigit() {
        return !isEOF() && ('0' <= currentChar() && currentChar() <= '9');
    }

    boolean isLetter() {
        return !isEOF() &&
                (('a' <= currentChar() && currentChar() <= 'z') ||
                        ('A' <= currentChar() && currentChar() <= 'Z'));
    }

    boolean is(char c) {
        return !isEOF() && currentChar() == c;
    }

    boolean isOneOf(char... chars) {
        return !isEOF() && (new String(chars).indexOf(currentChar()) >= 0);
    }
}