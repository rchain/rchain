package coop.rchain.rholang.parser.log.impl;

import coop.rchain.rholang.parser.log.LineMap;

import java.util.ArrayList;
import java.util.List;

import static java.util.Collections.binarySearch;

public class LineMapImpl implements LineMap {
    // start position of each line
    private final String src;
    private final List<Integer> startPositions = new ArrayList<>();

    public LineMapImpl(String src) {
        this.src = src;

        int charIndex = 0;
        while (charIndex < src.length()) {
            startPositions.add(charIndex);
            do {
                char ch = src.charAt(charIndex);
                if (ch == '\r' || ch == '\n') {
                    if (ch == '\r' && (charIndex + 1) < src.length() && src.charAt(charIndex + 1) == '\n') {
                        charIndex += 2;
                    } else {
                        charIndex += 1;
                    }
                    break;
                }
            } while (++charIndex < src.length());
        }
    }

    @Override
    public int offsetToRow(int offset) {
        int index = binarySearch(startPositions, offset) + 1;
        return (index > 0) ? index : - index;
    }

    @Override
    public int offsetToCol(int offset) {
        return offset - startPositions.get(offsetToRow(offset) - 1) + 1;
    }

    @Override
    public String offsetToSrcLine(int offset) {
        int rowNum = offsetToRow(offset);
        int start = startPositions.get(rowNum - 1);
        if (rowNum < startPositions.size()) {
            int end = startPositions.get(rowNum);
            return src.substring(start, end);
        } else {
            int end = src.length();
            return src.substring(start, end);
        }
    }
}
