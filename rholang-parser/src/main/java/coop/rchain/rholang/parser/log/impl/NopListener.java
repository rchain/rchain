package coop.rchain.rholang.parser.log.impl;

import coop.rchain.rholang.parser.log.Diagnostic;
import coop.rchain.rholang.parser.log.DiagnosticListener;

public class NopListener implements DiagnosticListener {

    @Override
    public void report(Diagnostic diagnostic) {
        /*NOP*/
    }

    @Override
    public void eof() {
        /*NOP*/
    }
}
