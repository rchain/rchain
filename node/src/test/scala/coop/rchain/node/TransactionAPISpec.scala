package coop.rchain.node

import coop.rchain.casper.{ReportStore, ReportingCasper}
import coop.rchain.casper.api.BlockReportAPI
import coop.rchain.casper.helper.TestNode
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.GenesisBuilder.{buildGenesis, GenesisContext}
import coop.rchain.casper.util.rholang.Resources
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.Par
import coop.rchain.node.web.{PreCharge, Refund, Transaction, UserDeploy}
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.rspace.hashing.Blake2b256Hash
import coop.rchain.rspace.syntax.rspaceSyntaxKeyValueStoreManager
import monix.eval.Task
import org.scalatest.{FlatSpec, Inspectors, Matchers}
import monix.execution.Scheduler.Implicits.global

class TransactionAPISpec extends FlatSpec with Matchers with Inspectors {
  val genesis: GenesisContext = buildGenesis()

  "transfer rev" should "be gotten in transaction api" in {
    TestNode.standaloneEff(genesis).use { node =>
      val fromSk    = genesis.genesisVaultsSks.head
      val fromAddr  = RevAddress.fromPublicKey(Secp256k1.toPublic(fromSk)).get.toBase58
      val toPk      = genesis.genesisVaultsSks.last
      val toAddr    = RevAddress.fromPublicKey(Secp256k1.toPublic(toPk)).get.toBase58
      val amount    = 1L
      val phloPrice = 1L
      val phloLimit = 3000000L
      import node._
      def transferRho(fromAddr: String, toAddr: String, amount: Long) = s"""
                          #new rl(`rho:registry:lookup`), RevVaultCh, vaultCh, toVaultCh, deployerId(`rho:rchain:deployerId`), revVaultKeyCh, resultCh in {
                          #  rl!(`rho:rchain:revVault`, *RevVaultCh) |
                          #  for (@(_, RevVault) <- RevVaultCh) {
                          #    @RevVault!("findOrCreate", "$fromAddr", *vaultCh) |
                          #    @RevVault!("findOrCreate", "$toAddr", *toVaultCh) |
                          #    @RevVault!("deployerAuthKey", *deployerId, *revVaultKeyCh) |
                          #    for (@(true, vault) <- vaultCh; key <- revVaultKeyCh; @(true, toVault) <- toVaultCh) {
                          #      @vault!("transfer", "$toAddr", $amount, *key, *resultCh) |
                          #      for (_ <- resultCh) { Nil }
                          #    }
                          #  }
                          #}""".stripMargin('#')
      for {
        kvm             <- Resources.mkTestRNodeStoreManager[Task](node.dataDir)
        rspaceStore     <- kvm.rSpaceStores
        reportingCasper = ReportingCasper.rhoReporter[Task](rspaceStore)
        reportingStore  <- ReportStore.store[Task](kvm)
        blockReportAPI  = BlockReportAPI[Task](reportingCasper, reportingStore)
        term            = transferRho(fromAddr, toAddr, amount)
        deploy <- ConstructDeploy.sourceDeployNowF(
                   term,
                   sec = fromSk,
                   phloLimit = phloLimit,
                   phloPrice = phloPrice
                 )
        transactionAPI = Transaction[Task](
          blockReportAPI,
          Par(unforgeables = Seq(Transaction.transferUnforgeable))
        )
        transferBlock <- node.addBlock(deploy)
        transactions <- transactionAPI
                         .getTransaction(Blake2b256Hash.fromByteString(transferBlock.blockHash))
        _ = transactions.length should be(3)
        _ = transactions.foreach { t =>
          t.transactionType match {
            case UserDeploy(_) =>
              t.transaction.fromAddr should be(fromAddr)
              t.transaction.toAddr should be(toAddr)
              t.transaction.amount should be(amount)
              t.transaction.failReason should be(None)

            case PreCharge(_) =>
              t.transaction.fromAddr should be(fromAddr)
              t.transaction.amount should be(phloLimit * phloPrice)
              t.transaction.failReason should be(None)

            case Refund(_) =>
              t.transaction.toAddr should be(fromAddr)
              t.transaction.amount should be(
                phloLimit * phloPrice - transferBlock.body.deploys.head.cost.cost
              )
              t.transaction.failReason should be(None)
            case _ => ()
          }
        }

      } yield ()
    }
  }

}
