package coop.rchain.casper

import java.nio.file.{Path, Paths}

import cats.effect.Sync
import cats.implicits._
import coop.rchain.blockstorage.util.io.IOError.RaiseIOError
import coop.rchain.blockstorage.util.io.SourceIO
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.genesis.Genesis
import coop.rchain.casper.genesis.Genesis.{fromLine, getVaults}
import coop.rchain.casper.genesis.contracts.{ProofOfStake, RevGenerator, Validator, Vault}
import coop.rchain.casper.genesis.contracts.StandardDeploys.toDeploy
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.casper.util.ConstructDeploy.{defaultPub, defaultPub2}
import coop.rchain.casper.util.GenesisBuilder._
import coop.rchain.casper.util.RSpaceUtil._
import coop.rchain.casper.util.rholang.RegistrySigGen
import coop.rchain.crypto.PublicKey
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.rholang.interpreter.util.RevAddress
import coop.rchain.shared.Log
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.io.Source
import scala.util.{Failure, Success, Try}

class RholangBuildTest extends FlatSpec with Matchers {

  val genesis = buildGenesis()

  "Our build system" should "allow import of rholang sources into scala code" in effectTest {
    HashSetCasperTestNode
      .standaloneEff(genesis)
      .use { node =>
        import node._

        val code =
          """
          |new testRet, double, rl(`rho:registry:lookup`), ListOpsCh, getBlockData(`rho:block:data`),
          |    timeRtn, stdout(`rho:io:stdout`), doubleRet
          |in {
          |  contract double(@x, ret) = { ret!(2 * x) } |
          |  rl!(`rho:lang:listOps`, *ListOpsCh) |
          |  for(@(_, ListOps) <- ListOpsCh) {
          |    @ListOps!("map", [2, 3, 5, 7], *double, *doubleRet)
          |  } |
          |  getBlockData!(*timeRtn) |
          |  for (@_, @timestamp <- timeRtn; @doubles <- doubleRet) {
          |    testRet!((doubles, "The timestamp is ${timestamp}" %% {"timestamp" : timestamp}))
          |  }
          |}
          |""".stripMargin
        for {
          deploy <- ConstructDeploy.sourceDeployNowF(code)
          createBlockResult <- MultiParentCasper[Effect]
                                .deploy(deploy) >> MultiParentCasper[Effect].createBlock
          Created(signedBlock) = createBlockResult
          _                    <- MultiParentCasper[Effect].addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
          _                    = logEff.warns should be(Nil)
          _ <- getDataAtPrivateChannel[Effect](
                signedBlock,
                Base16.encode(
                  RegistrySigGen.generateUnforgeableNameId(
                    PublicKey(deploy.deployer.toByteArray),
                    deploy.timestamp
                  )
                )
              ).map(_ shouldBe Seq("""([4, 6, 10, 14], "The timestamp is 2")"""))
        } yield ()
      }
  }

  "Our build system" should "execute the genesis block" ignore effectTest {
    val validatorKeyPairs = (1 to 4).map(_ => Secp256k1.newKeyPair)
    val bonds = validatorKeyPairs
      .map(_._2)
      .zipWithIndex
      .map { case (v, i) => v -> (2L * i.toLong + 1L) }
      .toMap
    val vaults = Source
      .fromResource("wallets.txt")
      .getLines()
      .map { line =>
        line.split(",") match {
          case Array(ethAddress, initRevBalanceStr, _) =>
            Vault(RevAddress.fromEthAddress(ethAddress), initRevBalanceStr.toLong)
        }
      }
      .toSeq
    val genesisParams: GenesisParameters = (
      validatorKeyPairs,
      Genesis(
        shardId = "MultiParentCasperSpec",
        timestamp = 0L,
        proofOfStake = ProofOfStake(
          minimumBond = 0L,
          maximumBond = Long.MaxValue,
          epochLength = 1,
          validators = bonds.map(Validator.tupled).toSeq
        ),
        genesisPk = defaultPub,
        vaults = vaults,
        supply = Long.MaxValue
      )
    )
    HashSetCasperTestNode
      .standaloneEff(buildGenesis(genesisParams))
      .use { node =>
        import node._
        (logEff.warns should be(Nil)).pure[Effect]
      }
  }
}
