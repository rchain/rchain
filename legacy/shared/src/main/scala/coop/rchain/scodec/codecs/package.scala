package coop.rchain.scodec

import scodec.{Attempt, Codec, Err}

import cats._, cats.data._, cats.syntax.all._

package object codecs {

  /**
    * Codec that encodes/decodes a `Seq[A]` of `N` elements using a `Codec[A]`.
    */
  def seqOfN[A](countCodec: Codec[Int], valueCodec: Codec[A]): Codec[Seq[A]] =
    countCodec
      .flatZip(count => new SeqCodec(valueCodec, Some(count)))
      .narrow[Seq[A]](
        {
          case (cnt, xs) =>
            if (xs.size === cnt)
              Attempt.successful(xs)
            else
              Attempt.failure(
                Err(
                  s"Insufficient number of elements: decoded ${xs.size} but should have decoded $cnt"
                )
              )
        },
        xs => (xs.size, xs)
      )
      .withToString(s"seqOfN($countCodec, $valueCodec)")
}
