package coop.rchain.rholang.parser.lexer;

import coop.rchain.rholang.parser.log.DiagnosticListener;
import coop.rchain.rholang.parser.log.impl.GroupedPrinter;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;

public class RhoLangLexisChecker {

    public static void main(String[] args) throws IOException {

        if (args == null || args.length != 1 ) {
            System.err.println("Tool need one arg - path to RhoLang source.");
            System.err.println("  Example: java -jar RhoLP.jar hello.rho");
            System.exit(1);
        } else {
            String content = new String(Files.readAllBytes(Paths.get(args[0])));

            DiagnosticListener listener = new GroupedPrinter(System.out);
            RhoLexer lexer = new RhoLexer(content, listener);

            lexer.readAll();
        }
    }
}
