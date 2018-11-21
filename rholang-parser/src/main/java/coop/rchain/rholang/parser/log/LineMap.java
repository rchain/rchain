package coop.rchain.rholang.parser.log;

/**
 * Provides methods to convert between character positions
 * and line numbers for a compilation unit.
 * <p/>
 * A class that defines source code positions as simple character
 * offsets from the beginning of the file. The first character
 * is at position 0.
 * <p/>
 * Support is also provided for (line,column) coordinates, but tab
 * expansion is optional and no Unicode excape translation is considered.
 * The first character is at location (1,1).
 * <p/>
 * <p/> Variation of {@link com.sun.source.tree.LineMap}
 */
public interface LineMap {

    /**
     * Find the line containing a position; a line termination
     * character is on the line it terminates.
     *
     * @param offset character offset of the position
     * @return the line number of offset (first line is 1)
     */
    int offsetToRow(int offset);

    /**
     * Find the column for a character position.
     * Tab characters preceding the position on the same line
     * will be expanded when calculating the column number.
     *
     * @param pos character offset of the position
     * @return the tab-expanded column number of pos (first column is 1)
     */
    int offsetToCol(int pos);

    String offsetToSrcLine(int offset);
}
