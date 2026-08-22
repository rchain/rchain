package coop.rchain.crypto.hash

import java.security.MessageDigest

/**
  * Sha256 hashing algorithm
  */
object Sha256 {

  def hash(input: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(input)

}
