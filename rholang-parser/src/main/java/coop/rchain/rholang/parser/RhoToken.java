package coop.rchain.rholang.parser;

import static coop.rchain.rholang.parser.RhoTokenType.EOF;

public class RhoToken {
    public final RhoTokenType type;
    public final String val;

    public RhoToken(RhoTokenType type, String val) {
        this.type = type;
        this.val = val;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;

        RhoToken rhoToken = (RhoToken) o;

        if (type != rhoToken.type) return false;
        if (val != null ? !val.equals(rhoToken.val) : rhoToken.val != null) return false;

        return true;
    }

    @Override
    public int hashCode() {
        int result = type != null ? type.hashCode() : 0;
        result = 31 * result + (val != null ? val.hashCode() : 0);
        return result;
    }

    @Override
    public String toString() {
        if (type == EOF) {
            return EOF.toString();
        } else {
            return "(" + type + ", '" + val + "')";
        }
    }
}