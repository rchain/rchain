package coop.rchain.rholang.parser.msg;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Arrays;
import java.util.Set;

import static java.util.stream.Collectors.toSet;

public class AbsentKeywords {
    private static final Set<String> absent;

    static {
        InputStream resource =
                AbsentKeywords.class.getResourceAsStream("/lexer-absent-keywords.txt");
        absent = new BufferedReader(new InputStreamReader(resource))
                .lines().flatMap(s -> Arrays.stream(s.split(",")))
                .map(String::trim).collect(toSet());
    }

    public static boolean contains(String actual) {
        return absent.contains(actual);
    }
}
