package coop.rchain.models
import monix.eval.Coeval
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MemoSpec extends AnyFlatSpec with Matchers {

  behavior of "Memo"

  it should "memoize the result" in {
    var timesExecuted = 0
    val random = new Memo[Int](Coeval.delay {
      timesExecuted += 1
      9
    })
    assert(timesExecuted == 0)
    assert(random.get.value() == 9)
    assert(timesExecuted == 1)
    assert(random.get.value() == 9)
    assert(timesExecuted == 1)
  }

}
