package coop.rchain.casper

import java.nio.file.{Files, Path}

import cats.effect._
import coop.rchain.casper.helper.BlockDagStorageTestFixture
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode.{Effect, _}
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.{ConstructDeploy, ProtoUtil}
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.p2p.EffectsTestInstances.LogicalTime
import coop.rchain.shared.PathOps.RichPath
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Inspectors, Matchers}

class MultiParentCasperRestartSpec extends FlatSpec with Matchers with Inspectors {

  import BlockDagStorageTestFixture._
  import MultiParentCasperTestUtil._

  implicit val timeEff: LogicalTime[Effect] = new LogicalTime[Effect]

  private val (validatorKey, validatorPk) = Secp256k1.newKeyPair
  private val genesis = buildGenesis(
    buildGenesisParameters(1, createBonds(Seq(validatorPk)))
  )

  "MultiParentCasper" should "load block data after restart" in effectTest {
    createDirectories[Effect].use {
      case (blockStoreDir, blockDagDir) =>
        Resource
          .make[Effect, Path](
            Sync[Effect].delay {
              Files.createTempDirectory(s"casper-restart-spec-storage-")
            }
          )(dir => Sync[Effect].delay { dir.recursivelyDelete() })
          .use { storageDir =>
            def startAndCreateBlock(restart: Boolean) =
              HashSetCasperTestNode
                .standaloneEffFromDirs(
                  genesis,
                  blockStoreDir,
                  blockDagDir,
                  storageDir,
                  validatorKey,
                  createEmptyDagStorage = !restart
                )
                .use { node =>
                  for {
                    deployData    <- ConstructDeploy.basicDeployData[Effect](1)
                    block         <- node.addBlock(deployData)
                    dagRepr       <- node.blockDagStorage.getRepresentation
                    invalidBlocks <- dagRepr.invalidBlocks
                    _             = invalidBlocks shouldBe empty
                  } yield (block)
                }

            for {
              block1     <- startAndCreateBlock(restart = false)
              blockHash1 = PrettyPrinter.buildStringNoLimit(block1.blockHash)
              block2     <- startAndCreateBlock(restart = true)
              block2parentHashes = ProtoUtil
                .parentHashes(block2)
                .map(PrettyPrinter.buildStringNoLimit)
            } yield (block2parentHashes should contain only (blockHash1))
          }
    }
  }
}
