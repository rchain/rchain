package coop.rchain.casper.util.rholang

import cats.implicits._
import com.google.protobuf.ByteString
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.helper.TestNode.Effect
import coop.rchain.casper.util.GenesisBuilder.buildGenesis
import coop.rchain.crypto.hash.Blake2b512Random
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.rholang.interpreter.accounting.Cost
import coop.rchain.rspace.hashing.Blake2b256Hash
import org.scalatest.FlatSpec
import monix.execution.Scheduler.Implicits.global
import coop.rchain.rholang.interpreter.util.RevAddress

class VaultBalanceGetterTest extends FlatSpec {
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
        _ <- node.runtimeManager.withRuntime(
              runtime =>
                for {
                  _       <- runtime.reset(genesisPostStateHash)
                  balance <- VaultBalanceGetter.getBalanceFromVaultPar(vaultPar(0), runtime)
                  // 9000000 is hard coded in genesis block generation
                  _ = assert(balance.get == genesisInitialBalance)
                } yield ()
            )
      } yield ()
    }
    t.runSyncUnsafe()
  }

  "Get all vault" should "return all vault balance" in {
    val t = TestNode.standaloneEff(genesis).use { node =>
      val genesisPostStateHash =
        Blake2b256Hash.fromByteString(genesis.genesisBlock.body.state.postStateHash)
      for {
        balances <- node.runtimeManager.withRuntime(
                     runtime =>
                       for {
                         _      <- runtime.reset(genesisPostStateHash)
                         result <- VaultBalanceGetter.getAllVaultBalance(runtime)
                         // 9000000 is hard coded in genesis block generation
                       } yield result
                   )
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
