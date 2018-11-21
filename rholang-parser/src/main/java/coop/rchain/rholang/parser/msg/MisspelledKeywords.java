package coop.rchain.rholang.parser.msg;

import java.io.IOException;
import java.util.Optional;
import java.util.Properties;

import static java.util.Optional.ofNullable;

public class MisspelledKeywords {
    private static final Properties map = new Properties();

    static {
        try {
            map.load(AbsentKeywords.class
                    .getResourceAsStream("/lexer-misspelled-keywords.properties"));
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }

    public static Optional<String> tryCorrectMisspelled(String actual) {
        return ofNullable(map.getProperty(actual));
    }
}
