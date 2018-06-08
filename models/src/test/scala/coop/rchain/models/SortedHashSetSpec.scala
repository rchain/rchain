package coop.rchain.models

import java.io.ByteArrayOutputStream
import java.util

import com.google.protobuf.CodedInputStream
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.models.Expr.ExprInstance.GInt
import coop.rchain.models.Var.VarInstance.BoundVar
import coop.rchain.models.rholang.implicits._
import coop.rchain.models.rholang.sort.ordering._
import org.scalatest.{Assertion, FlatSpec, Matchers}

import scala.collection.immutable.BitSet

class SortedHashSetSpec extends FlatSpec with Matchers {

  // toByteArray method is using the same method calls as real protobuf's serialization
  private[this] def serializeESet(parTreeSet: SortedHashSet[Par]): Array[Byte] =
    ESet(ps = parTreeSet.sortedPars).toByteArray

  private[this] def roundtripTest(parTreeSet: SortedHashSet[Par]): Assertion =
    ESet.parseFrom(serializeESet(parTreeSet)) should ===(ESet(ps = parTreeSet.sortedPars))

  val pars: Seq[Par] = {
    val parGround =
      Par().withExprs(Seq(GInt(2), GInt(1), GInt(-1), GInt(-2), GInt(0)))
    val parExpr: Par =
      EPlus(EPlus(GInt(1), GInt(3)), GInt(2))
    val parMethods =
      Par().withExprs(
        List(
          EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(1)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(2), GInt(3)), locallyFree = BitSet(2)),
          EMethod("nth", EVar(BoundVar(2)), List(GInt(2)), locallyFree = BitSet(2))
        ))
    Seq(parGround, parExpr, parMethods)
  }

  def sample = SortedHashSet[Par](pars)

  "ParTreeSet" should "preserve structure during round trip protobuf serialization" in {
    roundtripTest(sample)
  }

  it should "preserve ordering during serialization (required for deterministic serialization)" in {
    val referenceBytes = serializeESet(sample)

    // just to make sure that it's not a fluke, serialize it 1000 times
    (1 to 1000).forall { i =>
      withClue(s"Run #$i serialization: ") {
        val serialized = serializeESet(sample)
        val res        = util.Arrays.equals(serialized, referenceBytes)
        assert(res == true, ". Same set of Pars should serialize deterministically")
        res
      }
    } should be(true)
  }
}
