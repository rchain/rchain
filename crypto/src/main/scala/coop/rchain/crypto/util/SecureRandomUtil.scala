package coop.rchain.crypto.util

import java.security.SecureRandom
import scala.util.{Success, Try}

object SecureRandomUtil {
  // NativePRNGNonBlocking uses /dev/urandom for nextBytes() and generateSeed()
  // See https://docs.oracle.com/javase/8/docs/technotes/guides/security/SunProviders.html#SUNProvider
  private val instancesNonBlocking = List(
    "NativePRNGNonBlocking",
    "Windows-PRNG",
    "SHA1PRNG"
  )

  lazy val secureRandomNonBlocking: SecureRandom =
    instancesNonBlocking.iterator //use iterator to try instances lazily
      .map(name => Try(SecureRandom.getInstance(name)))
      .collectFirst {
        case Success(sr) => sr
      }
      .getOrElse(throw new Exception("Could not get any SecureRandom instance!"))
}
