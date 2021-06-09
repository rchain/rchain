package coop.rchain.casper.batch2

import com.google.protobuf.ByteString
import coop.rchain.blockstorage.BlockStore
import coop.rchain.blockstorage.dag.IndexedBlockDagStorage
import coop.rchain.casper.SafetyOracle
import coop.rchain.casper.helper.BlockGenerator._
import coop.rchain.casper.helper.BlockUtil.generateValidator
import coop.rchain.casper.helper.{BlockDagStorageFixture, BlockGenerator}
import coop.rchain.casper.protocol.{BlockMessage, Bond}
import coop.rchain.metrics.{Metrics, NoopSpan, Span}
import coop.rchain.models.Validator.Validator
import coop.rchain.p2p.EffectsTestInstances.LogStub
import monix.eval.Task
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.HashMap
import scala.collection.mutable

class CliqueOracleTest
    extends FlatSpec
    with Matchers
    with BlockGenerator
    with BlockDagStorageFixture {

  behavior of "Clique Oracle"

  implicit val logEff               = new LogStub[Task]
  implicit val metricsEff           = new Metrics.MetricsNOP[Task]
  implicit val noopSpan: Span[Task] = NoopSpan[Task]()

  implicit def toImmutableMap(
      m: mutable.Map[Validator, BlockMessage]
  ): Map[Validator, BlockMessage] = m.toMap

  def createBlock(bonds: Seq[Bond])(genesis: BlockMessage)(creator: ByteString)(
      parent: BlockMessage,
      justifications: Map[Validator, BlockMessage]
  )(implicit store: BlockStore[Task], dagStore: IndexedBlockDagStorage[Task]): Task[BlockMessage] =
    createBlock[Task](
      Seq(parent.blockHash),
      genesis,
      creator = creator,
      bonds = bonds,
      justifications = justifications.map { case (v, bm) => (v, bm.blockHash) }
    )

  // See [[/docs/casper/images/cbc-casper_ping_pong_diagram.png]]
  /**
    *       *     b8
    *       |
    *   *   *     b6 b7
    *   | /
    *   *   *     b4 b5
    *   | /
    *   *   *     b2 b3
    *    \ /
    *     *
    *   c2 c1
    */
  it should "detect finality as appropriate" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = generateValidator("Validator One")
      val v2     = generateValidator("Validator Two")
      val v1Bond = Bond(v1, 2)
      val v2Bond = Bond(v2, 3)
      val bonds  = Seq(v1Bond, v2Bond)

      implicit val cliqueOracleEffect = SafetyOracle.cliqueOracle[Task]
      for {
        genesis              <- createGenesis[Task](bonds = bonds)
        creator1             = createBlock(bonds)(genesis)(v1) _
        creator2             = createBlock(bonds)(genesis)(v2) _
        genesisJustification = HashMap(v1 -> genesis, v2 -> genesis)
        b2                   <- creator2(genesis, genesisJustification)
        b3                   <- creator1(genesis, genesisJustification)
        b4                   <- creator2(b2, HashMap(v1 -> genesis, v2 -> b2))
        b5                   <- creator1(b2, HashMap(v1 -> b3, v2 -> b2))
        b6                   <- creator2(b4, HashMap(v1 -> b5, v2 -> b4))
        b7                   <- creator1(b4, HashMap(v1 -> b5, v2 -> b4))
        b8                   <- creator1(b7, HashMap(v1 -> b7, v2 -> b4))

        dag                   <- blockDagStorage.getRepresentation
        genesisFaultTolerance <- SafetyOracle[Task].normalizedFaultTolerance(dag, genesis.blockHash)
        _                     = assert(genesisFaultTolerance === 1f +- 0.01f)
        b2FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b2.blockHash)
        _                     = assert(b2FaultTolerance === 1f +- 0.01f)
        b3FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b3.blockHash)
        _                     = assert(b3FaultTolerance === -1f +- 0.01f)
        b4FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b4.blockHash)
        result                = assert(b4FaultTolerance === 0.2f +- 0.01f)
      } yield result
  }

  // See [[/docs/casper/images/no_finalizable_block_mistake_with_no_disagreement_check.png]]
  it should "detect possible disagreements appropriately" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v1     = generateValidator("Validator One")
      val v2     = generateValidator("Validator Two")
      val v3     = generateValidator("Validator Three")
      val v1Bond = Bond(v1, 25)
      val v2Bond = Bond(v2, 20)
      val v3Bond = Bond(v3, 15)
      val bonds  = Seq(v1Bond, v2Bond, v3Bond)

      implicit val cliqueOracleEffect = SafetyOracle.cliqueOracle[Task]
      for {
        genesis              <- createGenesis[Task](bonds = bonds)
        creator1             = createBlock(bonds)(genesis)(v1) _
        creator2             = createBlock(bonds)(genesis)(v2) _
        creator3             = createBlock(bonds)(genesis)(v3) _
        genesisJustification = HashMap(v1 -> genesis, v2 -> genesis, v3 -> genesis)
        b2                   <- creator2(genesis, genesisJustification)
        b3                   <- creator1(genesis, genesisJustification)
        b4                   <- creator3(b2, HashMap(v1 -> genesis, v2 -> b2, v3 -> b2))
        b5                   <- creator2(b3, HashMap(v1 -> b3, v2 -> b2, v3 -> genesis))
        b6                   <- creator1(b4, HashMap(v1 -> b3, v2 -> b2, v3 -> b4))
        b7                   <- creator3(b5, HashMap(v1 -> b3, v2 -> b5, v3 -> b4))
        b8                   <- creator2(b6, HashMap(v1 -> b6, v2 -> b5, v3 -> b4))

        dag <- blockDagStorage.getRepresentation

        genesisFaultTolerance <- SafetyOracle[Task].normalizedFaultTolerance(dag, genesis.blockHash)
        _                     = assert(genesisFaultTolerance === 1f +- 0.01f)
        b2FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b2.blockHash)
        _                     = assert(b2FaultTolerance === -1f / 6 +- 0.01f)
        b3FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b3.blockHash)
        _                     = assert(b3FaultTolerance === -1f +- 0.01f)
        b4FaultTolerance      <- SafetyOracle[Task].normalizedFaultTolerance(dag, b4.blockHash)
        result                = assert(b4FaultTolerance === -1f / 6 +- 0.01f)
      } yield result
  }

  // See [[/docs/casper/images/no_majority_fork_safe_after_union.png]]
  it should "identify no majority fork safe after union" in withStorage {
    implicit blockStore => implicit blockDagStorage =>
      val v0    = generateValidator("Validator Zero")
      val v1    = generateValidator("Validator One")
      val v2    = generateValidator("Validator Two")
      val v3    = generateValidator("Validator Three")
      val v4    = generateValidator("Validator Four")
      val bonds = Seq(Bond(v0, 500), Bond(v1, 450), Bond(v2, 600), Bond(v3, 400), Bond(v4, 525))

      implicit val cliqueOracleEffect = SafetyOracle.cliqueOracle[Task]
      /*
        # create right hand side of fork and check for no safety
        'M-2-A SJ-1-A M-1-L0 SJ-0-L0 M-0-L1 SJ-1-L1 M-1-L2 SJ-0-L2 '
        'M-0-L3 SJ-1-L3 M-1-L4 SJ-0-L4 '
        # now, left hand side as well. should still have no safety
        'SJ-3-A M-3-R0 SJ-4-R0 M-4-R1 SJ-3-R1 M-3-R2 SJ-4-R2 M-4-R3 '
        'SJ-3-R3 M-3-R4 SJ-4-R4'
       */

      for {
        ge       <- createGenesis[Task](bonds = bonds)
        creator0 = createBlock(bonds)(ge)(v0) _
        creator1 = createBlock(bonds)(ge)(v1) _
        creator2 = createBlock(bonds)(ge)(v2) _
        creator3 = createBlock(bonds)(ge)(v3) _
        creator4 = createBlock(bonds)(ge)(v4) _
        gjL      = mutable.HashMap(v0 -> ge, v1 -> ge, v2 -> ge, v3 -> ge, v4 -> ge)

        /*
         create left hand side of fork and check for no safety
        'M-2-A SJ-1-A M-1-L0 SJ-0-L0 M-0-L1 SJ-1-L1 M-1-L2 SJ-0-L2 '
        'M-0-L3 SJ-1-L3 M-1-L4 SJ-0-L4 '
         */
        a <- creator2(ge, gjL)

        l0 <- creator1(a, gjL += (v2  -> a))
        l1 <- creator0(l0, gjL += (v1 -> l0))
        l2 <- creator1(l1, gjL += (v0 -> l1))
        l3 <- creator0(l2, gjL += (v1 -> l2))
        l4 <- creator1(l3, gjL += (v0 -> l3))

        gjR = mutable.HashMap(v0 -> ge, v1 -> ge, v2 -> ge, v3 -> ge, v4 -> ge)
        /*
         now, right hand side as well. should still have no safety
        'SJ-3-A M-3-R0 SJ-4-R0 M-4-R1 SJ-3-R1 M-3-R2 SJ-4-R2 M-4-R3 '
        'SJ-3-R3 M-3-R4 SJ-4-R4'
         */
        r0 <- creator3(a, gjR += (v2  -> a))
        r1 <- creator4(r0, gjR += (v3 -> r0))
        r2 <- creator3(r1, gjR += (v4 -> r1))
        r3 <- creator4(r2, gjR += (v3 -> r2))
        r4 <- creator3(r3, gjR += (v4 -> r3))

        dag <- blockDagStorage.getRepresentation

        l0FaultTolerance <- SafetyOracle[Task].normalizedFaultTolerance(dag, l0.blockHash)
        _                = assert(l0FaultTolerance === -1f +- 0.01f)
        r0FaultTolerance <- SafetyOracle[Task].normalizedFaultTolerance(dag, r0.blockHash)
        _                = assert(r0FaultTolerance === -1f +- 0.01f)

        /*
         show all validators all messages
        'SJ-0-R4 SJ-1-R4 SJ-2-R4 SJ-2-L4 SJ-3-L4 SJ-4-L4 '
         */
        aj = mutable.HashMap(v0 -> l3, v1 -> l4, v2 -> a, v3 -> r4, v4 -> r3)

        /*
         two rounds of round robin, check have safety on the correct fork
        'M-0-J0 SJ-1-J0 M-1-J1 SJ-2-J1 M-2-J2 SJ-3-J2 M-3-J3 SJ-4-J3 M-4-J4 SJ-0-J4 '
        'M-0-J01 SJ-1-J01 M-1-J11 SJ-2-J11 M-2-J21 SJ-3-J21 M-3-J31 SJ-4-J31 M-4-J41 SJ-0-J41'
         */

        j0 <- creator0(l4, aj)
        j1 <- creator1(j0, aj += (v0 -> j0))
        j2 <- creator2(j1, aj += (v1 -> j1))
        j3 <- creator3(j2, aj += (v2 -> j2))
        j4 <- creator4(j3, aj += (v3 -> j3))

        j01 <- creator0(j4, aj += (v4  -> j4))
        j11 <- creator1(j01, aj += (v0 -> j01))
        j21 <- creator2(j11, aj += (v1 -> j11))
        j31 <- creator3(j21, aj += (v2 -> j21))
        j41 <- creator4(j31, aj += (v3 -> j31))

        dag2 <- blockDagStorage.getRepresentation

        faultTolerance <- SafetyOracle[Task].normalizedFaultTolerance(dag2, l0.blockHash)
        _              = assert(faultTolerance === 1f +- 0.01f)
      } yield ()
  }
}
