package coop.rchain.trie

import java.security.MessageDigest
import scala.util.Random

object Digest {
  
  def sha256(s: String): String = bytesToHex(digest(s))
    
  def sha256: String = sha256(Random.alphanumeric.take(100).mkString)
    
  private def bytesToHex(b: Array[Byte]): String =
    b.map("%02x".format(_)).mkString
    
  private def digest(s: String)(implicit provider: String = "SHA-256"): Array[Byte] =
    MessageDigest.getInstance(provider).digest(s.getBytes)
}