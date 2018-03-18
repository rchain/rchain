define("ace/mode/rholang_highlight_rules",["require","exports","module","ace/lib/oop","ace/mode/text_highlight_rules"], function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var RholangHighlightRules = function() {

    this.$rules = {
        start: [{
            include: "#declarations"
        }],
        "#for-declaration": [{
            token: "text",
            regex: /(?=\bfor\b)/,
            push: [{
                token: "text",
                regex: "(?<=\{)",
                next: "pop"
            }, {
                token: "keyword.control.for.rho",
                regex: /\bfor\b/,
                push: [{
                    token: "text",
                    regex: "(?<=\\))",
                    next: "pop"
                }, {
                    include: "#comment"
                }, {
                    include: "#bind"
                }]
            }, {
                include: "#comment"
            }, {
            }]
        }],
        "#parameter": [{
            token: "variable.name.rho",
            regex: /[-_'a-zA-Z0-9]+/
        }],
        "#appl": [{
            token: "text",
            regex: /(?=\b\.?[-_'a-zA-Z0-9]+\s*\()/,
            push: [{
                token: "text",
                regex: "(?<=\\))",
                next: "pop"
            }, {
                include: "#argument-list"
            }, {
                token: "entity.name.function.rho",
                regex: /[-_'a-zA-Z0-9]+/
            }]
        }],
        "#func-declaration": [{
            token: "text",
            regex: /(?=\bdef\b)/,
            push: [{
                token: "text",
                regex: "(?<=\})",
                next: "pop"
            }, {
                token: "keyword.other.function.rho",
                regex: /\bdef\b/
            }, {
                token: "entity.name.function.rho",
                regex: /[-_''a-zA-Z0-9]+/,
                push: [{
                    token: "text",
                    regex: /(?=\{)/,
                    next: "pop"
                }, {
                    include: "#comment"
                }, {
                    include: "#parameter-list"
                }, {
                    token: "text",
                    regex: "(?<=\\))",
                    push: [{
                        token: "text",
                        regex: /(?=\{)/,
                        next: "pop"
                    }, {
                        token: "keyword.other.function.rho",
                        regex: /\=/
                    }]
                }]
            }, {
                include: "#comment"
            }, {
                include: "#block-declaration"
            }]
        }],
        "#parameter-list": [{
            token: "paren.lparen.rho",
            regex: /\(/,
            push: [{
                token: "paren.rparen.rho",
                regex: /\)/,
                next: "pop"
            }, {
                include: "#comment"
            }, {
                include: "#string"
            }, {
                include: "#constant"
            }, {
                include: "#parameter"
            }]
        }],
        "#block-declaration": [{
            token: "paren.lparen.rho",
            regex: /\{/,
            push: [{
                token: "paren.rparen.rho",
                regex: /\}/,
                next: "pop"
            }, {
                include: "#declarations"
            }]
        }],
        "#comment": [{
            token: "punctuation.definition.comment.rho",
            regex: /\/\*/,
            push: [{
                token: "punctuation.definition.comment.rho",
                regex: /\*\//,
                next: "pop"
            }, {
                defaultToken: "comment.block.rho"
            }]
        }, {
            token: "comment",
            regex: "\\/\\/.*$"
        }],
        "#terminals": [{
            include: "#comment"
        }, {
            include: "#string"
        }, {
            include: "#appl"
        }, {
            include: "#constant"
        }, {
            include: "#chan"
        }],
        "#contract-declaration": [{
            token: "text",
            regex: /(?=\bcontract\b)/,
            push: [{
                token: "text",
                regex: "(?<=\})",
                next: "pop"
            }, {
                token: "keyword.other.contract.rho",
                regex: /\bcontract\b/
            }, {
                token: "entity.name.function.rho",
                regex: /[-_''a-zA-Z0-9]+/,
                push: [{
                    token: "text",
                    regex: /(?=\{)/,
                    next: "pop"
                }, {
                    include: "#comment"
                }, {
                    include: "#parameter-list"
                }, {
                    token: "text",
                    regex: "(?<=\\))",
                    push: [{
                        token: "text",
                        regex: /(?=\{)/,
                        next: "pop"
                    }, {
                        token: "keyword.other.contract.rho",
                        regex: /\=/
                    }]
                }]
            }, {
                include: "#comment"
            }, {
                include: "#block-declaration"
            }]
        }],
        "#sum-total-declaration": [{
            token: "text",
            regex: /(?=\b(?:sum|total)\b)/,
            push: [{
                token: "text",
                regex: "(?<=\})",
                next: "pop"
            }, {
                token: "keyword.control.sum.rho",
                regex: /\b(?:sum|total)\b/,
                push: [{
                    token: "text",
                    regex: "(?<=\\))",
                    next: "pop"
                }, {
                    include: "#comment"
                }, {
                    include: "#bind"
                }]
            }, {
                include: "#comment"
            }, {
                include: "#block-declaration"
            }]
        }],
        "#parameter-bind": [{
            token: "variable.name.rho",
            regex: /[-_'a-zA-Z0-9]+\s*(?=<-)/
        }],
        "#chan": [{
            token: ["keyword.other.chan.rho", "text"],
            regex: /([@*#]*)([-_'a-zA-Z0-9]+)/
        }],
        "#new-declaration": [{
            token: "keyword.other.new.rho",
            regex: /\bnew\b/,
            push: [{
                token: ["keyword.other.in.rho", "text"],
                regex: /(in)(\s*)(?=\{)/,
                next: "pop"
            }, {
                include: "#comment"
            }, {
                include: "#parameter"
            }]
        }, {
            include: "#comment"
        }, {
            include: "#block-declaration"
        }],
        "#match-declaration": [{
            token: "text",
            regex: /(?=\bmatch\b)/,
            push: [{
                token: "text",
                regex: /(?=\})/,
                next: "pop"
            }, {
                token: "keyword.control.match.rho",
                regex: /\bmatch\b/,
                push: [{
                    token: "keyword.control.match.rho",
                    regex: /\bwith\b/,
                    next: "pop"
                }, {
                    include: "#terminals"
                }]
            }, {
                include: "#comment"
            }, {
                include: "#block-declaration"
            }, {
                include: "#terminals"
            }, {
                token: "keyword.control.match.rho",
                regex: /=>/
            }]
        }],
        "#argument-list": [{
            token: "paren.lparen.rho",
            regex: /\(/,
            push: [{
                token: "paren.rparen.rho",
                regex: /\)/,
                next: "pop"
            }, {
                include: "#terminals"
            }]
        }],
        "#bind": [{
            token: "paren.lparen.rho",
            regex: /\(/,
            push: [{
                token: "paren.rparen.rho",
                regex: /\)/,
                next: "pop"
            }, {
                include: "#parameter-bind"
            }, {
                include: "#terminals"
            }, {
                token: "keyword.other.bind.rho",
                regex: /<-|\/:|:\\/
            }]
        }],
        "#select-declaration": [{
            token: "text",
            regex: /(?=\bselect\b)/,
            push: [{
                token: "text",
                regex: "(?<=\})",
                next: "pop"
            }, {
                token: "keyword.control.select.rho",
                regex: /\bselect\b/,
                push: [{
                    token: "text",
                    regex: /(?=\})/,
                    next: "pop"
                }, {
                    include: "#comment"
                }, {
                    include: "#case-declaration"
                }]
            }, {
                include: "#comment"
            }, {
                include: "#block-declaration"
            }]
        }],
        "#string": [{
            token: "string.quoted.double.rho",
            regex: /"/,
            push: [{
                token: "string.quoted.double.rho",
                regex: /"/,
                next: "pop"
            }, {
                token: "constant.character.escape.rho",
                regex: /\\./
            }, {
                defaultToken: "string.quoted.double.rho"
            }]
        }],
        "#declarations": [{
            include: "#comment"
        }, {
            include: "#string"
        }, {
            include: "#new-declaration"
        }, {
            include: "#contract-declaration"
        }, {
            include: "#func-declaration"
        }, {
            include: "#for-declaration"
        }, {
            include: "#match-declaration"
        }, {
            include: "#select-declaration"
        }, {
            include: "#sum-total-declaration"
        }, {
            include: "#block-declaration"
        }, {
            include: "#appl"
        }, {
            include: "#constant"
        }, {
            include: "#chan"
        }],
        "#constant": [{
            token: "constant.language.rho",
            regex: /\b(?:Nil|true|false)\b/
        }, {
            token: "constant.numeric.rho",
            regex: /-?\b[0-9]+\b/
        }],
        "#case-declaration": [{
            token: "text",
            regex: /(?=\bcase\b)/,
            push: [{
                token: "text",
                regex: "(?<=\})",
                next: "pop"
            }, {
                token: "keyword.control.case.rho",
                regex: /\bcase\b/,
                push: [{
                    token: "text",
                    regex: /(?=\{)/,
                    next: "pop"
                }, {
                    include: "#parameter-bind"
                }, {
                    include: "#terminals"
                }, {
                    token: "keyword.control.case.rho",
                    regex: /=>/
                }, {
                    token: "keyword.other.case.rho",
                    regex: /<-/
                }]
            }, {
                include: "#comment"
            }, {
                include: "#block-declaration"
            }]
        }]
    };

    this.normalizeRules();
};

RholangHighlightRules.metaData = {
    name: "Rholang",
    fileTypes: ["rho"],
    scopeName: "source.rho",
    "$schema": "https://raw.githubusercontent.com/martinring/tmlanguage/master/tmlanguage.json"
};

oop.inherits(RholangHighlightRules, TextHighlightRules);

exports.RholangHighlightRules = RholangHighlightRules;
});

define("ace/mode/folding/rholang",["require","exports","module","ace/lib/oop","ace/mode/folding/fold_mode"], function(require, exports, module) {
    "use strict";
    
    var oop = require("../../lib/oop");
    var BaseFoldMode = require("./fold_mode").FoldMode;
    
    var FoldMode = exports.FoldMode = function(markers) {
        this.foldingStartMarker = new RegExp("([\\[{])(?:\\s*)$|(" + markers + ")(?:\\s*)(?:#.*)?$");
    };
    oop.inherits(FoldMode, BaseFoldMode);
    
    (function() {
    
        this.getFoldWidgetRange = function(session, foldStyle, row) {
            var line = session.getLine(row);
            var match = line.match(this.foldingStartMarker);
            if (match) {
                if (match[1])
                    return this.openingBracketBlock(session, match[1], row, match.index);
                if (match[2])
                    return this.indentationBlock(session, row, match.index + match[2].length);
                return this.indentationBlock(session, row);
            }
        };
    
    }).call(FoldMode.prototype);
    
    });

define("ace/mode/matching_brace_outdent",["require","exports","module","ace/range"], function(require, exports, module) {
"use strict";

var Range = require("../range").Range;

var MatchingBraceOutdent = function() {};

(function() {

    this.checkOutdent = function(line, input) {
        if (! /^\s+$/.test(line))
            return false;

        return /^\s*\}/.test(input);
    };

    this.autoOutdent = function(doc, row) {
        var line = doc.getLine(row);
        var match = line.match(/^(\s*\})/);

        if (!match) return 0;

        var column = match[1].length;
        var openBracePos = doc.findMatchingBracket({row: row, column: column});

        if (!openBracePos || openBracePos.row == row) return 0;

        var indent = this.$getIndent(doc.getLine(openBracePos.row));
        doc.replace(new Range(row, 0, row, column-1), indent);
    };

    this.$getIndent = function(line) {
        return line.match(/^\s*/)[0];
    };

}).call(MatchingBraceOutdent.prototype);

exports.MatchingBraceOutdent = MatchingBraceOutdent;
});

define("ace/mode/rholang",["require","exports","module","ace/lib/oop","ace/mode/text","ace/mode/rholang_highlight_rules","ace/mode/folding/rholang","ace/mode/matching_brace_outdent","ace/mode/behaviour/cstyle"], function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextMode = require("./text").Mode;
var RholangHighlightRules = require("./rholang_highlight_rules").RholangHighlightRules;
var FoldMode = require("./folding/rholang").FoldMode;
var MatchingBraceOutdent = require("./matching_brace_outdent").MatchingBraceOutdent;
var CstyleBehaviour = require("./behaviour/cstyle").CstyleBehaviour;

var Mode = function() {
    this.HighlightRules = RholangHighlightRules;
    this.foldingRules = new FoldMode();
    this.$outdent = new MatchingBraceOutdent();
    this.$behaviour = new CstyleBehaviour();
};
oop.inherits(Mode, TextMode);

(function() {
    this.lineCommentStart = "//";
    this.blockComment = {start: "/*", end: "*/"};

    this.getNextLineIndent = function(state, line, tab) {
        var indent = this.$getIndent(line);

        var tokenizedLine = this.getTokenizer().getLineTokens(line, state);
        var tokens = tokenizedLine.tokens;

        if (tokens.length && tokens[tokens.length-1].type == "comment") {
            return indent;
        }

        if (state == "start") {
            var match = line.match(/^.*[\{\(\[]\s*$/);
            if (match) {
                indent += tab;
            }
        }

        return indent;
    };

    this.checkOutdent = function(state, line, input) {
        return this.$outdent.checkOutdent(line, input);
    };

    this.autoOutdent = function(state, doc, row) {
        this.$outdent.autoOutdent(doc, row);
    };

    this.createWorker = function(session) {
        return null;
    };

    this.$id = "ace/mode/rholang";
}).call(Mode.prototype);

exports.Mode = Mode;
});
