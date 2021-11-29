package coop.rchain.casper.genesis.contracts

import cats.syntax.all._
import coop.rchain.casper.helper.RhoSpec
import coop.rchain.casper.util.GenesisBuilder
import coop.rchain.crypto.PublicKey
import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangSource
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.models.syntax._
import scala.concurrent.duration._

class PoSSpec
    extends RhoSpec(
      CompiledRholangSource("PoSTest.rho", NormalizerEnv.Empty),
      Seq.empty,
      400.seconds,
      genesisParameters = {
        val p = GenesisBuilder.buildGenesisParameters()
        (p._1, p._2, p._3.copy(vaults = p._3.vaults ++ PoSSpec.testVaults))
      }
    )

object PoSSpec {

  def prepareVault(vaultData: (String, Long)): Vault =
    Vault(RevAddress.fromPublicKey(PublicKey(vaultData._1.unsafeDecodeHex)).get, vaultData._2)

  val testVaults: Seq[Vault] = Seq(
    ("0" * 130, 10000L),
    ("1" * 130, 10000L),
    ("2" * 130, 10000L),
    ("3" * 130, 10000L),
    ("4" * 130, 10000L),
    ("5" * 130, 10000L),
    ("6" * 130, 10000L),
    ("7" * 130, 10000L),
    ("8" * 130, 10000L),
    ("9" * 130, 10000L),
    ("a" * 130, 10000L),
    ("b" * 130, 10000L),
    ("c" * 130, 10000L),
    ("d" * 130, 10000L),
    ("e" * 130, 10000L)
  ).map(prepareVault)
}
