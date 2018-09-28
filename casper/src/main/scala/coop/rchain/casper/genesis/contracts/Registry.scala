package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.models.Par

object Registry extends CompiledRholangSource {
  final override val code: String = "new rr(`rho:registry:initialize`) in { rr!() }"
  final override val term: Par    = InterpreterUtil.mkTerm(code).right.get
}
