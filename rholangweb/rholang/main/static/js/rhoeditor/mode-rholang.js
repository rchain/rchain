define("ace/mode/rholang_highlight_rules",["require","exports","module","ace/lib/oop","ace/mode/text_highlight_rules"], function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextHighlightRules = require("./text_highlight_rules").TextHighlightRules;

var RholangHighlightRules = function() {

    this.$rules = {
        start: [{
            include: "#declarations"
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
            include: "#for-declaration"
        }, {
            include: "#match-declaration"
        }, {
            include: "#select-declaration"
        }, {
            include: "#block-declaration"
        }, {
            include: "#appl"
        }, {
            include: "#constant"
        }, {
            include: "#chan"
        }, {
            token: "keyword.control.rho",
            regex: /=>/
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
        "#constant": [{
            token: "constant.language.rho",
            regex: /\b(?:Nil|true|false)\b/
        }, {
            token: "constant.numeric.rho",
            regex: /-?\b[0-9]+\b/
        }],
        "#contract-declaration": [{
            token: "text",
            regex: /(?=\bcontract\b)/,
            push: [{
                token: "text",
                regex: /(?={)/,
                next: "pop"
            }, {
                token: "keyword.other.contract.rho",
                regex: /\bcontract\b/
            }, {
                token: "entity.name.function.rho",
                regex: /[-_'a-zA-Z0-9]+/
            }, {
                include: "#comment"
            }, {
                include: "#parameter-list"
            }, {
                token: "keyword.other.contract.rho",
                regex: /\=/
            }]
        }],
        "#for-declaration": [{
            token: "text",
            regex: /(?=\bfor\b)/,
            push: [{
                token: "text",
                regex: /(?={)/,
                next: "pop"
            }, {
                token: "keyword.control.for.rho",
                regex: /\bfor\b/
            }, {
                include: "#comment"
            }, {
                include: "#bind"
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
        "#new-declaration": [{
            token: "keyword.other.new.rho",
            regex: /\b(new|export|import)\b/,
            push: [{
                token: ["keyword.other.in.rho", "text"],
                regex: /(in)(\s*)(?={)/,
                next: "pop"
            }, {
                include: "#comment"
            }, {
                include: "#parameter"
            }]
        }],
        "#match-declaration": [{
            token: "keyword.control.match.rho",
            regex: /\bmatch\b/,
            push: [{
                token: "keyword.control.match.rho",
                regex: /\bwith\b/,
                next: "pop"
            }, {
                include: "#comment"
            }, {
                include: "#terminals"
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
        "#select-declaration": [{
            token: "keyword.control.select.rho",
            regex: /\bselect\b/
        }, {
            include: "#comment"
        }, {
            include: "#case-declaration"
        }],
        "#case-declaration": [{
            token: "text",
            regex: /(?=\bcase\b)/,
            push: [{
                token: "text",
                regex: /(?={)/,
                next: "pop"
            }, {
                token: "keyword.control.case.rho",
                regex: /\bcase\b/
            }, {
                include: "#comment"
            }, {
                include: "#parameter-bind"
            }, {
                include: "#terminals"
            }, {
                token: "keyword.other.case.rho",
                regex: /<-/
            }, {
                token: "keyword.control.case.rho",
                regex: /=>/
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
        "#chan": [{
            token: ["keyword.other.chan.rho", "text"],
            regex: /([@*#]*)([-_'a-zA-Z0-9]+)/
        }],
        "#appl": [{
            token: "text",
            regex: /(?=\b\.?[-_'a-zA-Z0-9]+\s*\()/,
            push: [{
                token: "text",
                regex: /(?=\()/,
                next: "pop"
            }, {
                token: "entity.name.function.rho",
                regex: /[-_'a-zA-Z0-9]+/
            }]
        }, {
            include: "#argument-list"
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
        "#parameter-bind": [{
            token: "variable.name.rho",
            regex: /[-_'a-zA-Z0-9]+\s*(?=<-)/
        }],
        "#parameter": [{
            token: "variable.name.rho",
            regex: /[-_'a-zA-Z0-9]+/
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

define("ace/mode/folding/cstyle",["require","exports","module","ace/lib/oop","ace/range","ace/mode/folding/fold_mode"], function(require, exports, module) {
"use strict";

var oop = require("../../lib/oop");
var Range = require("../../range").Range;
var BaseFoldMode = require("./fold_mode").FoldMode;

var FoldMode = exports.FoldMode = function(commentRegex) {
    if (commentRegex) {
        this.foldingStartMarker = new RegExp(
            this.foldingStartMarker.source.replace(/\|[^|]*?$/, "|" + commentRegex.start)
        );
        this.foldingStopMarker = new RegExp(
            this.foldingStopMarker.source.replace(/\|[^|]*?$/, "|" + commentRegex.end)
        );
    }
};
oop.inherits(FoldMode, BaseFoldMode);

(function() {
    
    this.foldingStartMarker = /([\{\[\(])[^\}\]\)]*$|^\s*(\/\*)/;
    this.foldingStopMarker = /^[^\[\{\(]*([\}\]\)])|^[\s\*]*(\*\/)/;
    this.singleLineBlockCommentRe= /^\s*(\/\*).*\*\/\s*$/;
    this.tripleStarBlockCommentRe = /^\s*(\/\*\*\*).*\*\/\s*$/;
    this.startRegionRe = /^\s*(\/\*|\/\/)#?region\b/;
    this._getFoldWidgetBase = this.getFoldWidget;
    this.getFoldWidget = function(session, foldStyle, row) {
        var line = session.getLine(row);
    
        if (this.singleLineBlockCommentRe.test(line)) {
            if (!this.startRegionRe.test(line) && !this.tripleStarBlockCommentRe.test(line))
                return "";
        }
    
        var fw = this._getFoldWidgetBase(session, foldStyle, row);
    
        if (!fw && this.startRegionRe.test(line))
            return "start"; // lineCommentRegionStart
    
        return fw;
    };

    this.getFoldWidgetRange = function(session, foldStyle, row, forceMultiline) {
        var line = session.getLine(row);
        
        if (this.startRegionRe.test(line))
            return this.getCommentRegionBlock(session, line, row);
        
        var match = line.match(this.foldingStartMarker);
        if (match) {
            var i = match.index;

            if (match[1])
                return this.openingBracketBlock(session, match[1], row, i);
                
            var range = session.getCommentFoldRange(row, i + match[0].length, 1);
            
            if (range && !range.isMultiLine()) {
                if (forceMultiline) {
                    range = this.getSectionRange(session, row);
                } else if (foldStyle != "all")
                    range = null;
            }
            
            return range;
        }

        if (foldStyle === "markbegin")
            return;

        var match = line.match(this.foldingStopMarker);
        if (match) {
            var i = match.index + match[0].length;

            if (match[1])
                return this.closingBracketBlock(session, match[1], row, i);

            return session.getCommentFoldRange(row, i, -1);
        }
    };
    
    this.getSectionRange = function(session, row) {
        var line = session.getLine(row);
        var startIndent = line.search(/\S/);
        var startRow = row;
        var startColumn = line.length;
        row = row + 1;
        var endRow = row;
        var maxRow = session.getLength();
        while (++row < maxRow) {
            line = session.getLine(row);
            var indent = line.search(/\S/);
            if (indent === -1)
                continue;
            if  (startIndent > indent)
                break;
            var subRange = this.getFoldWidgetRange(session, "all", row);
            
            if (subRange) {
                if (subRange.start.row <= startRow) {
                    break;
                } else if (subRange.isMultiLine()) {
                    row = subRange.end.row;
                } else if (startIndent == indent) {
                    break;
                }
            }
            endRow = row;
        }
        
        return new Range(startRow, startColumn, endRow, session.getLine(endRow).length);
    };
    this.getCommentRegionBlock = function(session, line, row) {
        var startColumn = line.search(/\s*$/);
        var maxRow = session.getLength();
        var startRow = row;
        
        var re = /^\s*(?:\/\*|\/\/|--)#?(end)?region\b/;
        var depth = 1;
        while (++row < maxRow) {
            line = session.getLine(row);
            var m = re.exec(line);
            if (!m) continue;
            if (m[1]) depth--;
            else depth++;

            if (!depth) break;
        }

        var endRow = row;
        if (endRow > startRow) {
            return new Range(startRow, startColumn, endRow, line.length);
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

define("ace/mode/rholang",["require","exports","module","ace/lib/oop","ace/mode/text","ace/mode/rholang_highlight_rules","ace/mode/folding/cstyle","ace/mode/matching_brace_outdent","ace/mode/behaviour/cstyle"], function(require, exports, module) {
"use strict";

var oop = require("../lib/oop");
var TextMode = require("./text").Mode;
var RholangHighlightRules = require("./rholang_highlight_rules").RholangHighlightRules;
var FoldMode = require("./folding/cstyle").FoldMode;
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

    this.$id = "ace/mode/rholang";
}).call(Mode.prototype);

exports.Mode = Mode;
});
