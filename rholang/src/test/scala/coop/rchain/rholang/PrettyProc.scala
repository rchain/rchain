package coop.rchain.rholang
import coop.rchain.rholang.syntax.rholang_mercury.Absyn.Proc
import coop.rchain.rholang.syntax.rholang_mercury.PrettyPrinter
import org.scalacheck.Shrink
import ProcGen.procShrinker

case class PrettyProc(proc: Proc) {
  override def toString = PrettyPrinter.print(proc)
}

object PrettyProc {

  implicit def shrinkProc: Shrink[PrettyProc] = Shrink { pp =>
    Shrink.shrink(pp.proc).map(PrettyProc(_))
  }
}
