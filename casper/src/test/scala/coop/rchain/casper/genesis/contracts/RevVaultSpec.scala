package coop.rchain.casper.genesis.contracts

import cats.implicits._
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.NormalizerEnv

class RevVaultSpec
    extends RhoSpec(
      CompiledRholangSource("RevVaultTest.rho", RevVaultSpec.normalizerEnv),
      Seq.empty,
      GENESIS_TEST_TIMEOUT
    )

object RevVaultSpec {
  val genesisPk = PublicKey(
    Base16.unsafeDecode(
      "0000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000"
    )
  )
  val normalizerEnv = NormalizerEnv(none, genesisPk.some)
}
