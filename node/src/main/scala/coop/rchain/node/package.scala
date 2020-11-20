package coop.rchain

import cats.data.EitherT
import coop.rchain.comm.CommError

package object node {
  type CommErrT[F[_], A] = EitherT[F, CommError, A]
}
