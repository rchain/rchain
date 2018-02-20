package coop.rchain.catscontrib

object IOUtil {
  def currentMilis[F[_]: Capture]: F[Long]       = Capture[F].capture(System.currentTimeMillis)
  def sleep[F[_]: Capture](milis: Long): F[Unit] = Capture[F].capture(Thread.sleep(milis))
}
