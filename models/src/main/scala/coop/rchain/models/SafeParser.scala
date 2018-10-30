package coop.rchain.models
import cats.effect.Sync
import com.google.protobuf.CodedInputStream

object SafeParser {

  def readMessage[F[_]: Sync, A](input: CodedInputStream, message: StacksafeMessage[A]): F[A] = {
    import cats.implicits._

    val length   = input.readRawVarint32()
    val oldLimit = input.pushLimit(length)

    message
      .mergeFromM[F](input)
      .map(result => {
        input.checkLastTagWas(0)
        input.popLimit(oldLimit)
        result
      })
  }

}
