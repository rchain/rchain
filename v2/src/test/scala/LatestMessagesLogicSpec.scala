import coop.rchain.v2.casper.data.LatestMessages
import org.scalatest.{FlatSpec, Matchers}

class LatestMessagesLogicSpec extends FlatSpec with Matchers {

  it should "Update LM record if current LM is a self justification for a new message" in {
    val curV = LatestMessages[Int, Int](Map(1 -> Set(0)))
    val newV = LatestMessages.update(curV, 1)(_ => 1, _ => 0)
    newV shouldBe LatestMessages[Int, Int](Map(1 -> Set(1)))
  }

  it should "Record two equivocating LMs if current LM is not self justification for a new message" in {
    val curV = LatestMessages[Int, Int](Map(1 -> Set(0)))
    val newV = LatestMessages.update(curV, 1)(_ => 1, _ => -1)
    newV shouldBe LatestMessages[Int, Int](Map(1 -> Set(0, 1)))
  }
}
