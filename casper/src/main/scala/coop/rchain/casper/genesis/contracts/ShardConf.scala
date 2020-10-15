package coop.rchain.casper.genesis.contracts

import coop.rchain.casper.CasperShardConf
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.models.NormalizerEnv
import coop.rchain.rholang.build.CompiledRholangTemplate

// TODO: Eliminate validators argument if unnecessary.
// TODO: eliminate the default for epochLength. Now it is used in order to minimise the impact of adding this parameter
// TODO: Remove hardcoded keys from standard deploys: https://rchain.atlassian.net/browse/RCHAIN-3321?atlOrigin=eyJpIjoiNDc0NjE4YzYxOTRkNDcyYjljZDdlOWMxYjE1NWUxNjIiLCJwIjoiaiJ9
final case class ShardConf(
    casperShardConf: CasperShardConf
) extends CompiledRholangTemplate(
      "ShardConf.rhox",
      NormalizerEnv.withDeployerId(
        PublicKey(
          Base16.unsafeDecode(
            "047b43d6548b72813b89ac1b9f9ca67624a8b372feedd71d4e2da036384a3e1236812227e524e6f237cde5f80dbb921cac12e6500791e9a9ed1254a745a816fe1f"
          )
        )
      ),
      "faultToleranceThreshold"      -> casperShardConf.faultToleranceThreshold,
      "shardName"                    -> casperShardConf.shardName,
      "parentShardId"                -> casperShardConf.parentShardId,
      "finalizationRate"             -> casperShardConf.finalizationRate,
      "maxNumberOfParents"           -> casperShardConf.maxNumberOfParents,
      "maxParentDepth"               -> casperShardConf.maxParentDepth,
      "synchronyConstraintThreshold" -> casperShardConf.synchronyConstraintThreshold,
      "heightConstraintThreshold"    -> casperShardConf.heightConstraintThreshold,
      "deployLifespan"               -> casperShardConf.deployLifespan,
      "casperVersion"                -> casperShardConf.casperVersion,
      "configVersion"                -> casperShardConf.configVersion,
      "bondMinimum"                  -> casperShardConf.bondMinimum,
      "bondMaximum"                  -> casperShardConf.bondMaximum,
      "epochLength"                  -> casperShardConf.epochLength,
      "quarantineLength"             -> casperShardConf.quarantineLength
    ) {

//  require(minimumBond <= maximumBond)
//
//  require(validators.nonEmpty)

}
