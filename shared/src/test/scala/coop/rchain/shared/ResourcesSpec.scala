package coop.rchain.shared
import coop.rchain.shared.Resources.withResource
import org.scalatest.{FlatSpec, Matchers}

class ResourcesSpec extends FlatSpec with Matchers {

  val resource = new AutoCloseable {
    override def close(): Unit = throw new Exception("close")
  }

  "Try-finally" should "swallow exception in try-block when exception in finally is thrown" in {
    val caught = intercept[Exception](
      try {
        throw new Exception("try-block")
      } finally {
        throw new Exception("finally")
      }
    )

    caught.getMessage shouldBe "finally"
    caught.getSuppressed shouldBe empty
  }

  "withResource" should "not swallow exception in try-block when exception in finally is thrown" in {
    val caught = intercept[Exception](
      withResource(resource)(_ => throw new Exception("try-block"))
    )

    caught.getMessage shouldBe "try-block"
  }

  it should "add the exception thrown in finally as suppressed" in {
    val caught = intercept[Exception](
      withResource(resource)(_ => throw new Exception("try-block"))
    )

    caught.getSuppressed.apply(0).getMessage shouldBe "close"
  }
}
