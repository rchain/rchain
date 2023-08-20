package coop.rchain.models.rholangn.parmanager.primitive

trait PrimitiveWriter[F[_]] {
  def write(x: Byte): F[Unit]

  /** Writes raw bytes without size prefix */
  def writeRaw(x: Array[Byte]): F[Unit]

  /** Writes bytes with size prefix */
  def write(x: Array[Byte]): F[Unit]
  def write(x: Boolean): F[Unit]
  def write(x: Int): F[Unit]
  def write(x: Long): F[Unit]
  def write(x: String): F[Unit]
}
