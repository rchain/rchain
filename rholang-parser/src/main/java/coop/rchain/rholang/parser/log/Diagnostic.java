package coop.rchain.rholang.parser.log;

import java.util.List;

import static coop.rchain.rholang.parser.log.Diagnostic.Kind.ERROR;
import static coop.rchain.rholang.parser.log.Diagnostic.Kind.NOTE;
import static coop.rchain.rholang.parser.log.Diagnostic.Kind.WARN;

/**
 * Class for diagnostics from lexer/parser. A diagnostic reports
 * a problem at a specific position in a source file.
 * <p/>
 * <p>A position is a zero-based character offset from the beginning of
 * a file.  Negative values are not valid positions.
 * <p/>
 * <p>Line and column numbers begin at 1.  Negative values and 0
 * are not valid line or column numbers.
 * <p/>
 * <p> Variation of {@link javax.tools.Diagnostic}.
 */
public class Diagnostic {
    public final Kind kind;
    public final String code;
    public final String msg;
    public final String msgTemplate;
    public final List<String> msgArgs;
    //
    /**
     * The source code line with problem.
     */
    public final String line;
    public final int col;
    public final int len;
    //
    public final int row;
    public final int offset;


    public Diagnostic(Kind kind,
                      String code,
                      String msg,
                      String msgTemplate,
                      List<String> msgArgs,
                      //
                      String line,
                      int col,
                      int len,
                      //
                      int offset,
                      int row
    ) {
        this.kind = kind;
        this.code = code;
        this.msg = msg;
        this.msgTemplate = msgTemplate;
        this.msgArgs = msgArgs;
        //
        this.line = line;
        this.col = col;
        this.len = len;
        //
        this.offset = offset;
        this.row = row;

    }

    public static Diagnostic note(String code,
                                  String msg,
                                  String msgTemplate,
                                  List<String> msgArgs,
                                  //
                                  String line,
                                  int colNum,
                                  int len,
                                  //
                                  int offset,
                                  int rowNum) {
        return new Diagnostic(
                NOTE,
                code,
                msg,
                msgTemplate,
                msgArgs,
                //
                line,
                colNum,
                len,
                //
                offset,
                rowNum);
    }

    public static Diagnostic warn(String code,
                                  String msg,
                                  String msgTemplate,
                                  List<String> msgArgs,
                                  //
                                  String line,
                                  int colNum,
                                  int len,
                                  //
                                  int offset,
                                  int rowNum) {
        return new Diagnostic(
                WARN,
                code,
                msg,
                msgTemplate,
                msgArgs,
                //
                line,
                colNum,
                len,
                //
                offset,
                rowNum);
    }

    public static Diagnostic error(String code,
                                   String msg,
                                   String msgTemplate,
                                   List<String> msgArgs,
                                   //
                                   String line,
                                   int colNum,
                                   int len,
                                   //
                                   int offset,
                                   int rowNum) {
        return new Diagnostic(
                ERROR,
                code,
                msg,
                msgTemplate,
                msgArgs,
                //
                line,
                colNum,
                len,
                //
                offset,
                rowNum);
    }

    /**
     * Kinds of diagnostics: NOTE, WARN, ERROR.
     */
    public static enum Kind {
        /**
         * Informative message from the tool.
         */
        NOTE,
        /**
         * Problem which does not usually prevent the tool from
         * completing normally.
         */
        WARN,
        /**
         * Problem which prevents the tool's normal completion.
         */
        ERROR,
    }
}
