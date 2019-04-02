package coop.rchain.casper.util
import coop.rchain.casper.protocol.BlockMessage
import coop.rchain.crypto.PrivateKey
import coop.rchain.crypto.signatures.SignaturesAlg

object implicits {
  implicit class RichBlockMessage(b: BlockMessage) {
    def signFunction: (Array[Byte], PrivateKey) => Array[Byte] =
    SignaturesAlg(b.sigAlgorithm).get.sign
  }
}
