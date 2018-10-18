package coop.rchain.comm

import cats.effect.Sync
import coop.rchain.shared.Log

import scala.language.higherKinds

package object transport {

  def generateCertificateIfAbsent[F[_]: Log: Sync] =
    new GenerateCertificateIfAbsent[F]
}
