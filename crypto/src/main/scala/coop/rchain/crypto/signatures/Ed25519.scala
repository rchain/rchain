package coop.rchain.crypto.signatures

import org.abstractj.kalium.keys._

object Ed25519 {

  //TODO: Make use of strongly typed keys
  def newKeyPair: (Array[Byte], Array[Byte]) = {
    val key = new SigningKey()
    val sec = key.toBytes()
    val pub = key.getVerifyKey().toBytes()
    (sec, pub)
  }

  /**
    * Ed25519 Compute Pubkey - computes public key from secret key
    *
    * {{{
    * >>> import coop.rchain.crypto.codec._
    * >>> val sec = Base16.decode("b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd")
    * >>> Base16.encode(Ed25519.toPublic(sec))
    * 77f48b59caeda77751ed138b0ec667ff50f8768c25d48309a8f386a2bad187fb
    * }}}
    *
    * @param seckey Ed25519 Secret key, 32 bytes
    *
    * Return values
    * @param pubkey Ed25519 Public key, 32 bytes
    */
  def toPublic(sec: Array[Byte]): Array[Byte] = {
    val key = new SigningKey(sec)
    key.getVerifyKey().toBytes()
  }

  /**
    * Verifies the given Ed25519 signature.
    *
    * {{{
    * >>> import coop.rchain.crypto.codec._
    * >>> val data = Base16.decode("916c7d1d268fc0e77c1bef238432573c39be577bbea0998936add2b50a653171ce18a542b0b7f96c1691a3be6031522894a8634183eda38798a0c5d5d79fbd01dd04a8646d71873b77b221998a81922d8105f892316369d5224c9983372d2313c6b1f4556ea26ba49d46e8b561e0fc76633ac9766e68e21fba7edca93c4c7460376d7f3ac22ff372c18f613f2ae2e856af40")
    * >>> val sig = Base16.decode("6bd710a368c1249923fc7a1610747403040f0cc30815a00f9ff548a896bbda0b4eb2ca19ebcf917f0f34200a9edbad3901b64ab09cc5ef7b9bcc3c40c0ff7509")
    * >>> val pub = Base16.decode("77f48b59caeda77751ed138b0ec667ff50f8768c25d48309a8f386a2bad187fb")
    * >>> Ed25519.verify(data, sig, pub)
    * true
    * }}}
    *
    * Input values
    * @param data The data which was signed, must be exactly 32 bytes
    * @param signature The signature
    * @param pub The public key which did the signing
    *
    * Return value
    * boolean value of verification
    *
    */
  def verify(
      data: Array[Byte],
      signature: Array[Byte],
      pub: Array[Byte]
  ): Boolean =
    try {
      new VerifyKey(pub).verify(data, signature)
    } catch {
      case ex: RuntimeException =>
        if (ex.getMessage contains "signature was forged or corrupted") {
          false
        } else {
          throw ex
        }
    }

  /**
    * Create an Ed25519 signature.
    *
    * {{{
    * >>> import coop.rchain.crypto.codec._
    * >>> val data = Base16.decode("916c7d1d268fc0e77c1bef238432573c39be577bbea0998936add2b50a653171ce18a542b0b7f96c1691a3be6031522894a8634183eda38798a0c5d5d79fbd01dd04a8646d71873b77b221998a81922d8105f892316369d5224c9983372d2313c6b1f4556ea26ba49d46e8b561e0fc76633ac9766e68e21fba7edca93c4c7460376d7f3ac22ff372c18f613f2ae2e856af40")
    * >>> val sig = Base16.decode("6bd710a368c1249923fc7a1610747403040f0cc30815a00f9ff548a896bbda0b4eb2ca19ebcf917f0f34200a9edbad3901b64ab09cc5ef7b9bcc3c40c0ff7509")
    * >>> val sec = Base16.decode("b18e1d0045995ec3d010c387ccfeb984d783af8fbb0f40fa7db126d889f6dadd")
    * >>> Ed25519.sign(data, sec).deep == sig.deep
    * true
    * }}}
    *
    * Input values
    * @param data Message hash, 32 bytes
    * @param sec Secret key, 32 bytes
    *
    * Return value
    * byte array of signature
    *
  **/
  def sign(
      data: Array[Byte],
      sec: Array[Byte]
  ): Array[Byte] =
    new SigningKey(sec).sign(data)

}
