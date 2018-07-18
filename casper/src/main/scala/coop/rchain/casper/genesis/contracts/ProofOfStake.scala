package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.util.rholang.InterpreterUtil
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.Par
import coop.rchain.rholang.build.CompiledRholangSource

case class ProofOfStakeValidator(id: Array[Byte], stake: Int)

class ProofOfStake(validators: Seq[ProofOfStakeValidator]) extends CompiledRholangSource {
  val code = s"""
       |@"proofOfStake"!($validatorCode)
       |""".stripMargin

  private def validatorCode: String =
    if (validators.isEmpty) {
      "{}"
    } else {
      val validatorsMap = validators
        .map(validator => s""""${Base16.encode(validator.id)}": ${validator.stake}""")
        .mkString(",")
      s"""{$validatorsMap}"""
    }
  override val term: Par = InterpreterUtil.mkTerm(code).right.get
}
