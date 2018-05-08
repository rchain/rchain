package coop.rchain.casper.util.rholang

import coop.rchain.models.Par

import coop.rchain.rholang.interpreter.RholangCLI

import java.io.StringReader

object InterpreterUtil {

  def mkTerm(s: String): Either[Throwable, Par] =
    RholangCLI.buildNormalizedTerm(new StringReader(s))

}
