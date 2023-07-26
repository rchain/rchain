package coop.rchain.models.rholangN
import scodec.bits.ByteVector

final class UPrivateN(private val input: ByteVector) extends UnforgeableN {
  override val v: ByteVector = input
}
object UPrivateN {
  def apply(v: ByteVector): UPrivateN      = new UPrivateN(v)
  def apply(bytes: Array[Byte]): UPrivateN = new UPrivateN(ByteVector(bytes))
}

final class UDeployIdN(private val input: ByteVector) extends UnforgeableN {
  override val v: ByteVector = input
}
object UDeployIdN {
  def apply(v: ByteVector): UDeployIdN      = new UDeployIdN(v)
  def apply(bytes: Array[Byte]): UDeployIdN = new UDeployIdN(ByteVector(bytes))
}

final class UDeployerIdN(private val input: ByteVector) extends UnforgeableN {
  override val v: ByteVector = input
}
object UDeployerIdN {
  def apply(v: ByteVector): UDeployerIdN      = new UDeployerIdN(v)
  def apply(bytes: Array[Byte]): UDeployerIdN = new UDeployerIdN(ByteVector(bytes))
}

final class USysAuthTokenN(private val input: ByteVector) extends UnforgeableN {
  override val v: ByteVector = input
}
object USysAuthTokenN { // TODO: Temporary solution for easier conversion from old types - change type in the future
  def apply(): USysAuthTokenN = new USysAuthTokenN(ByteVector(Array[Byte]()))
}
