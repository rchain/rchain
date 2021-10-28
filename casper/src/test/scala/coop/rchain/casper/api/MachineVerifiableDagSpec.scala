package coop.rchain.casper.api

import coop.rchain.casper._
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.models.BlockHash.BlockHash

import coop.rchain.models.syntax._
import cats._, cats.implicits._
import org.scalatest.{FunSpec, Matchers}

class MachineVerifiableDagSpec extends FunSpec with Matchers {

  import VerifiableEdge._

  describe("MachineVerifiableDag") {

    it("should create dag for simple two blocks with one merge block") {
      // given
      val genesis = Blake2b256.hash("genesis".getBytes).toByteString
      val block1  = Blake2b256.hash("block1".getBytes).toByteString
      val block2  = Blake2b256.hash("block2".getBytes).toByteString
      val block3  = Blake2b256.hash("block3".getBytes).toByteString

      val toposort: TopoSort = Vector(
        Vector(block1, block2),
        Vector(block3)
      )

      val fetch: BlockHash => Id[List[BlockHash]] = {
        case b if b == block1 => List(genesis)
        case b if b == block2 => List(genesis)
        case b if b == block3 => List(block1, block2)
      }
      // when
      val result: List[VerifiableEdge] = MachineVerifiableDag[Id](toposort, fetch)
      // then
      result(0) should be(VerifiableEdge(block3.show, block1.show))
      result(1) should be(VerifiableEdge(block3.show, block2.show))
      result(2) should be(VerifiableEdge(block1.show, genesis.show))
      result(3) should be(VerifiableEdge(block2.show, genesis.show))
    }
  }
}
