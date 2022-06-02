package coop.rchain.node.revvaultexport

import com.google.protobuf.ByteString
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.node.revvaultexport.mainnet1.StateBalanceMain
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.hashing.Blake2b256Hash
import monix.execution.Scheduler.Implicits.global
import org.scalatest.flatspec.AnyFlatSpec

class VaultBalanceGetterTest extends AnyFlatSpec {
  val genesis               = buildGenesis()
  val genesisInitialBalance = 9000000
  "Get balance from VaultPar" should "return balance" in {
    val t = TestNode.standaloneEff(genesis).use { node =>
      val genesisPostStateHash =
        Blake2b256Hash.fromByteString(genesis.genesisBlock.body.state.postStateHash)
      val genesisVaultAddr = RevAddress.fromPublicKey(genesis.genesisVaults.toList(0)._2).get
      val getVault =
        s"""new return, rl(`rho:registry:lookup`), RevVaultCh, vaultCh, balanceCh in {
          |  rl!(`rho:rchain:revVault`, *RevVaultCh) |
          |  for (@(_, RevVault) <- RevVaultCh) {
          |    @RevVault!("findOrCreate", "${genesisVaultAddr.address.toBase58}", *vaultCh) |
          |    for (@(true, vault) <- vaultCh) {
          |      return!(vault)
          |    }
          |  }
          |}
          |""".stripMargin

      for {
        vaultPar <- node.runtimeManager
                     .playExploratoryDeploy(getVault, genesis.genesisBlock.body.state.postStateHash)
        runtime <- node.runtimeManager.spawnRuntime
        _       <- runtime.reset(genesisPostStateHash)
        balance <- VaultBalanceGetter.getBalanceFromVaultPar(vaultPar(0), runtime)
        // 9000000 is hard coded in genesis block generation
        _ = assert(balance.get == genesisInitialBalance)
      } yield ()
    }
    t.runSyncUnsafe()
  }

  "Get all vault" should "return all vault balance" in {
    val t = TestNode.standaloneEff(genesis).use { node =>
      val genesisPostStateHash =
        Blake2b256Hash.fromByteString(genesis.genesisBlock.body.state.postStateHash)
      for {
        runtime               <- node.runtimeManager.spawnRuntime
        _                     <- runtime.reset(genesisPostStateHash)
        vaultTreeHashMapDepth = StateBalanceMain.genesisVaultMapDepth
        vaultChannel          <- StateBalances.getGenesisVaultMapPar(runtime)
        balances <- VaultBalanceGetter.getAllVaultBalance(
                     vaultTreeHashMapDepth,
                     vaultChannel,
                     runtime
                   )
        // 9000000 is hard coded in genesis block generation
        balancesMap = balances.toMap
        _ = genesis.genesisVaults
          .map { case (_, pub) => RevAddress.fromPublicKey(pub).get }
          .map(addr => RhoTrieTraverser.keccakParString(addr.address.toBase58).drop(2))
          .map(
            byteAddr =>
              assert(balancesMap.get(ByteString.copyFrom(byteAddr)).get == genesisInitialBalance)
          )
      } yield ()
    }
    t.runSyncUnsafe()
  }

}
