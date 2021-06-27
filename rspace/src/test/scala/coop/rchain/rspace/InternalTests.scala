package coop.rchain.rspace

import coop.rchain.rspace.serializers.ScodecSerialize.RichAttempt
import org.scalatest.{FlatSpec, Matchers}
import scodec.{Attempt, Err}

class InternalTests extends FlatSpec with Matchers {

  "RichAttempt" should "tell user that data in RSpace was corrupted on faulty get" in {
    val a = Attempt.failure(Err("I failed miserably"))
    the[Exception] thrownBy { a.getUnsafe } should have message "Data in RSpace is corrupted. I failed miserably"
  }

  it should "get a value" in {
    val a = Attempt.successful(42)
    a.getUnsafe shouldBe 42
  }
}
