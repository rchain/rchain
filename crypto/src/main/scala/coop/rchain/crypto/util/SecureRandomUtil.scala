package coop.rchain.crypto.util

import java.security.SecureRandom
import scala.util.{Success, Try}

object SecureRandomUtil {
  private val instances = List(
    "NativePRNGNonBlocking",
    "Windows-PRNG",
    "SHA1PRNG"
  )

  def getSecureRandomInstance: SecureRandom =
    instances.iterator //use iterator for lazy evaluation
      .map(name => Try(SecureRandom.getInstance(name)))
      .collectFirst {
        case Success(sr) => sr
      }
      .getOrElse(throw new Exception("Could not get any SecureRandom instance!"))
}
