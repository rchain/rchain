package coop.rchain.catscontrib

object IOUtil {
  def sleep[F[_]: Capture](milis: Long): F[Unit] = Capture[F].capture(Thread.sleep(milis))
}
