package coop.rchain.casper

import cats.implicits._
import coop.rchain.casper.MultiParentCasper.ignoreDoppelgangerCheck
import coop.rchain.casper.helper.HashSetCasperTestNode
import coop.rchain.casper.helper.HashSetCasperTestNode._
import coop.rchain.casper.util.ConstructDeploy
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.casper.scalatestcontrib._
import coop.rchain.rholang.interpreter.accounting
import coop.rchain.shared.Time
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}
import util.RSpaceUtil._

class RholangBuildTest extends FlatSpec with Matchers {

  val (validatorKeys, validators) = (1 to 4).map(_ => Secp256k1.newKeyPair).unzip
  val bonds                       = MultiParentCasperTestUtil.createBonds(validators)
  val genesis                     = MultiParentCasperTestUtil.createGenesis(bonds)

  "Our build system" should "allow import of rholang sources into scala code" in effectTest {
    HashSetCasperTestNode
      .standaloneEff(genesis, validatorKeys.last)
      .use { node =>
        import node._
        implicit val rm = node.runtimeManager

        val code =
          """new double, rl(`rho:registry:lookup`), ListOpsCh, getBlockData(`rho:block:data`), timeRtn, stdout(`rho:io:stdout`), doubleRet ,timestampRet in {
          |  contract double(@x, ret) = { ret!(2 * x) } |
          |  rl!(`rho:lang:listOps`, *ListOpsCh) |
          |  for(@(_, ListOps) <- ListOpsCh) {
          |    @ListOps!("map", [2, 3, 5, 7], *double, *doubleRet)
          |  } |
          |  getBlockData!(*timeRtn) |
          |  for (@_, @timestamp <- timeRtn) {
          |    timestampRet!("The timestamp is ${timestamp}" %% {"timestamp" : timestamp})
          |  } |
          |  stdout!(("timestampRet: ", *timestampRet)) |
          |  stdout!(("doubleRet: ", *doubleRet))
          |}""".stripMargin

        for {
          //FIXME why having a 'real' time here resulted in initial charge not returning a result?
          //FIXME why does the resulting chnnel change depending on previous executions? Is rand shared and mutated?
          t      <- Time[Effect].currentMillis
          deploy <- ConstructDeploy.sourceDeploy(code, t, accounting.MAX_VALUE).pure[Effect]
          createBlockResult <- MultiParentCasper[Effect]
                                .deploy(deploy) *> MultiParentCasper[Effect].createBlock
          Created(signedBlock) = createBlockResult
          _                    <- MultiParentCasper[Effect].addBlock(signedBlock, ignoreDoppelgangerCheck[Effect])
          _                    = logEff.warns should be(Nil)
          _ <- getDataAtPrivateChannel[Effect](
                signedBlock,
                "801779f051312cb1cb58bd44c9123f914bbac866b8196be064b5fb5f59b1294"
              ).map(_ shouldBe Seq("[4, 6, 10, 14]"))
          _ <- getDataAtPrivateChannel[Effect](
                signedBlock,
                "d497b0a6770f453beee96823f71127ea7d5d8597decd8e9129776a0a6ac74a17"
              ).map(_ shouldBe Seq("\"The timestamp is 2\""))
        } yield ()
      }
  }

}
