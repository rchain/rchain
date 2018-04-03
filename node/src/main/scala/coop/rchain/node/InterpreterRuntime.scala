package coop.rchain.node

import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.{RholangCLI, _}

object InterpreterRuntime {

  def evaluateFile(fileName: String): Unit = {
    val persistentStore = RholangCLI.buildStore()
    val interp          = RholangOnlyDispatcher.create(persistentStore).reducer
    val source          = RholangCLI.reader(fileName)
    RholangCLI
      .buildNormalizedTerm(source)
      .foreach((par: Par) => RholangCLI.evaluate(interp, persistentStore, par))
  }

  def repl(): Unit = RholangCLI.repl()
}
