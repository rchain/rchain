package coop.rchain.rholang.parser.log.impl;

import coop.rchain.rholang.parser.log.Diagnostic;
import coop.rchain.rholang.parser.log.DiagnosticListener;

import java.io.PrintStream;
import java.util.*;

import static java.util.Arrays.fill;
import static java.util.Comparator.comparing;
import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.joining;
import static java.util.stream.Collectors.toList;

public class GroupedPrinter implements DiagnosticListener {
    private final PrintStream out;
    private final List<Diagnostic> diagnostics = new ArrayList<>();

    public GroupedPrinter() {
        this(System.err);
    }

    public GroupedPrinter(PrintStream out) {
        this.out = out;
    }

    @Override
    public void report(Diagnostic d) {
        diagnostics.add(d);
    }

    @Override
    public void eof() {

        Map<Key, List<Diagnostic>> grouped = diagnostics.stream()
                .collect(groupingBy(Key::new, TreeMap::new, toList()));

        for (Map.Entry<Key, List<Diagnostic>> e : grouped.entrySet()) {
            Key key = e.getKey();
            List<Diagnostic> diagnostics = e.getValue();

            out.println(key.kind);
            out.println("  Error code: " + key.code);

            printMessages(diagnostics);
            printLinesColumns(diagnostics);
            int leftTrimmedLen = printSourceCodeLine(diagnostics);
            printErrorPointers(diagnostics, leftTrimmedLen);
        }
    }

    private void printMessages(List<Diagnostic> diagnostics) {
        List<String> uniqueMessages = diagnostics.stream()
                .map(d -> d.msg)
                .distinct().collect(toList());

        if (uniqueMessages.size() == 1) {
            out.println("  Message:    " + uniqueMessages.get(0));
        } else {
            out.println("  Messages:");
            uniqueMessages.forEach(m -> out.println("    " + m));
        }
    }

    private void printLinesColumns(List<Diagnostic> diagnostics) {
        out.print("  Line/Column: " + diagnostics.stream()
                .map(d -> "[" + d.row + ", " + d.col + "]")
                .collect(joining(", ")));

        out.println("\n  ----------");
    }

    private int printSourceCodeLine(List<Diagnostic> diagnostics) {
        String line = diagnostics.get(0).line;
        String trimmedLine = line.trim();
        int first = line.indexOf(trimmedLine);
        out.println("  " + trimmedLine);
        return first;
    }

    private void printErrorPointers(List<Diagnostic> ds, int skipCount) {
        char[] buffer = new char[ds.get(0).line.length()];
        fill(buffer, ' ');

        for (Diagnostic d : ds) {
            fill(buffer, d.col - 1, d.col - 1 + d.len, '^');
        }

        out.println(("  " + new String(buffer)).substring(skipCount));
    }

    static class Key implements Comparable<Key> {
        final int row;
        final Diagnostic.Kind kind;
        final String code;

        Key(Diagnostic d) {
            this.row = d.row;
            this.kind = d.kind;
            this.code = d.code;
        }

        @Override
        public boolean equals(Object obj) {
            if (obj == null) return false;
            if (this.getClass() != obj.getClass()) return false;
            Key that = (Key) obj;
            return Objects.equals(this.row, that.row) &&
                    Objects.equals(this.kind, that.kind) &&
                    Objects.equals(this.code, that.code);
        }

        @Override
        public int hashCode() {
            return Objects.hash(row, kind, code);
        }

        @Override
        public int compareTo(Key that) {
            return comparing((Key k) -> k.row)
                    .thenComparing(comparing((Key k) -> k.kind))
                    .thenComparing(comparing((Key k) -> k.code))
                    .compare(this, that);
        }
    }
}