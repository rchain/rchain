package coop.rchain.node.benchmark.utils

import cats.syntax.all._
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.contracts.{ProofOfStake, Validator, Vault}
import coop.rchain.crypto.PublicKey
import coop.rchain.rholang.interpreter.util.RevAddress

import scala.collection.Seq

object GenesisParams {

  val predefinedVaultsAmt = 900000000L

  def genesisParameters(
      bondedValidators: Seq[Validator],
      genesisVaults: List[PublicKey]
  ): Genesis = {
    def predefinedVault(pub: PublicKey): Vault =
      Vault(RevAddress.fromPublicKey(pub).get, predefinedVaultsAmt)

    Genesis(
      shardId = "root",
      timestamp = 0L,
      proofOfStake = ProofOfStake(
        minimumBond = 0L,
        maximumBond = Long.MaxValue,
        // Epoch length is set to large number to prevent trigger of epoch change
        // in PoS close block method, which causes block merge conflicts
        // - epoch change can be set as a parameter in Rholang tests (e.g. PoSSpec)
        epochLength = 1000,
        quarantineLength = 50000,
        numberOfActiveValidators = 100,
        validators = bondedValidators
      ),
      vaults = genesisVaults.map(predefinedVault) ++
        bondedValidators.toList.map {
          case Validator(pk, _) =>
            // Initial validator vaults contain 0 Rev
            RevAddress.fromPublicKey(pk).map(Vault(_, 0))
        }.flattenOption,
      supply = Long.MaxValue,
      blockNumber = 0
    )
  }

}
