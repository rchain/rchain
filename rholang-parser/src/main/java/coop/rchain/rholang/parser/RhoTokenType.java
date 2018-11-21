/*
 * Copyright (c) 1999, 2005, Oracle and/or its affiliates. All rights reserved.
 * DO NOT ALTER OR REMOVE COPYRIGHT NOTICES OR THIS FILE HEADER.
 *
 * This code is free software; you can redistribute it and/or modify it
 * under the terms of the GNU General Public License version 2 only, as
 * published by the Free Software Foundation.  Oracle designates this
 * particular file as subject to the "Classpath" exception as provided
 * by Oracle in the LICENSE file that accompanied this code.
 *
 * This code is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
 * version 2 for more details (a copy is included in the LICENSE file that
 * accompanied this code).
 *
 * You should have received a copy of the GNU General Public License version
 * 2 along with this work; if not, write to the Free Software Foundation,
 * Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301 USA.
 *
 * Please contact Oracle, 500 Oracle Parkway, Redwood Shores, CA 94065 USA
 * or visit www.oracle.com if you need additional information or have any
 * questions.
 */

package coop.rchain.rholang.parser;


import static coop.rchain.rholang.parser.RhoTokenType.TokenGroup.*;
import static coop.rchain.rholang.parser.RhoTokenType.TokenGroup.Identifier;
import static coop.rchain.rholang.parser.RhoTokenType.TokenGroup.Literal;

/**
 * An interface that defines codes for Java source tokens
 * returned from lexical analysis.
 * <p/>
 * https://github.com/rchain/rchain/blob/dev/rholang/src/main/bnfc/rholang_mercury.cf
 */
public enum RhoTokenType {

    EOF("<EOF>", Dummy),

    ERROR(Dummy),
    IDENT(Identifier),
    LITERAL_INT(Literal),
    LITERAL_STRING(Literal),
    LITERAL_URI(Literal),

    // === Keywords
    IF("if", Keyword),
    ELSE("else", Keyword),
    MATCH("match", Keyword),
    MATCHES("matches", Keyword),
    SELECT("select", Keyword),
    FOR("for", Keyword),
    NEW("new", Keyword),
    IN("in", Keyword),
    CONTRACT("contract", Keyword),

    NOT("not", Keyword),
    AND("and", Keyword),
    OR("or", Keyword),

    BUNDLE("bundle", Keyword),
    BUNDLE_PLUS("bundle+", Keyword),
    BUNDLE_MINUS("bundle-", Keyword),
    BUNDLE_ZERO("bundle0", Keyword),

    // === Literals
    TRUE("true", Keyword),
    FALSE("false", Keyword),
    NIL("Nil", Keyword),

    // === Simple Types
    BOOL("Bool", Keyword),
    INT("Int", Keyword),
    STRING("String", Keyword),
    URI("Uri", Keyword),
    BYTE_ARRAY("ByteArray", Keyword),

    // === Compound Types
    SET("Set", Keyword),

    // === Separators (punctuators)
    LPAREN("(", Separator),
    RPAREN(")", Separator),
    LBRACE("{", Separator),
    RBRACE("}", Separator),
    LBRACKET("[", Separator),
    RBRACKET("]", Separator),
    COMMA(",", Separator),
    SEMI(";", Separator),
    DOT(".", Separator),
    COLON(":", Separator),
    ELLIPSIS("...", Separator),
    WILDCARD("_", Separator),

    // === Operators
    PERCENT_PERCENT("%%", Operator),

    TILDE("~", Operator),
    CONJUNCTION("/\\", Operator),
    DISJUNCTION("\\/", Operator),

    PLUS("+", Operator),
    MINUS("-", Operator),
    STAR("*", Operator),
    DIV("/", Operator),
    PLUS_PLUS("++", Operator),
    MINUS_MINUS("--", Operator),
    GT(">", Operator),
    LT("<", Operator),
    EQ("=", Operator),
    EQ_EQ("==", Operator),
    NOT_EQ("!=", Operator),
    GT_EQ(">=", Operator),

    PAR("|", Operator),
    QUOTE("@", Operator),

    BIND_LINEAR("<-", Operator),
    SEND_SINGLE("!", Operator),
    SEND_MULTIPLE("!!", Operator),
    ARROW("=>", Operator),
    BACK_ARROW("<=", Operator); // LT_EQ, BIND_REPEATED

    public final RhoToken T;
    public final TokenGroup group;

    private RhoTokenType(TokenGroup group) {
        this.T = null;
        this.group = group;
    }

    private RhoTokenType(String chars, TokenGroup group) {
        this.T = new RhoToken(this, chars);
        this.group = group;
    }

    public RhoToken T(String chars) {
        if (this.T == null) {
            return new RhoToken(this, chars);
        } else {
            throw new UnsupportedOperationException();
        }
    }

    public static RhoToken keywordOrIdent(String val) {
        // linear scan for constants
        for (RhoTokenType type : values()) {
            if (type.T != null && type.T.val.equals(val)) {
                return type.T;
            }
        }
        return IDENT.T(val);
    }

    public enum TokenGroup {
        Keyword, Operator, Separator, Literal, Identifier, Dummy
    };
}