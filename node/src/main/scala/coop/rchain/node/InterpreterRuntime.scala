package coop.rchain.node

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.{RholangCLI, Runtime}

object InterpreterRuntime {

  def evaluateFile(fileName: String): Unit = {

    val runtime = Runtime.create()

    val source = RholangCLI.reader(fileName)
    RholangCLI
      .buildNormalizedTerm(source)
      .foreach((par: Par) => RholangCLI.evaluate(runtime.reducer, runtime.store, par))
  }
}
