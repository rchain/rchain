package coop.rchain.storage.models
import coop.rchain.storage.{Serialize, SerializeError}
import org.scalatest.{FlatSpec, Matchers}

class RhoTypesTest extends FlatSpec with Matchers with SerializeInstances {
  def rtt[T](msg: T, serializeInstance: Serialize[T]): Unit = {
    val bytes = serializeInstance.encode(msg)
    val msg2  = serializeInstance.decode(bytes)
    assert(msg2.isRight && msg == msg2.right.get)

    val errBytes = Array[Byte](1, 2, 3, 4)
    val msg3     = serializeInstance.decode(errBytes)
    assert(msg3.isLeft && msg3.left.get.isInstanceOf[SerializeError])
  }

  "Block" should "pass SerializeInstances rtt" in {
    val blk = Block()
    rtt(blk, blockInstance)
  }

  "BoundVar" should "pass SerializeInstances rtt" in {
    val bv = BoundVar(20)
    rtt(bv, boundVarInstance)
  }

  "Var" should "pass SerializeInstances rtt" in {
    val bv: Var = Var().withBoundVar(BoundVar(20))
    rtt(bv, varInstance)

    val fv: Var = Var().withFreeVar(FreeVar(30))
    rtt(fv, varInstance)
  }
}
