package coop.rchain.pcasper.finalization

trait Finalizer[F[_], M, S, FT <: Final[M, S]] {
  def computeFinal(parentViews: Iterable[FT]): F[FT]
  def genesisFinal(genesis: M): FT
}
