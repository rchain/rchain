package coop.rchain.rspace

import org.scalatest.{FlatSpec, Matchers}
import scodec.{Attempt, Err}

import internal._

class InternalTests extends FlatSpec with Matchers {

  "RichAttempt" should "tell user that data in RSpace was corrupted on faulty get" in {
    val a = Attempt.failure(Err("I failed miserably"))
    the[Exception] thrownBy { a.get } should have message "Data in RSpace is corrupted. I failed miserably"
  }

  it should "get a value" in {
    val a = Attempt.successful(42)
    a.get shouldBe 42
  }
}
