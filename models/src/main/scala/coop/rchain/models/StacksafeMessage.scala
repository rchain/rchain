package coop.rchain.models
import cats.effect.Sync
import com.google.protobuf.CodedInputStream
import scalapb.Message

trait StacksafeMessage[A] extends scalapb.GeneratedMessage with Message[A] {

  val serializedSizeM: Memo[Int]

  def mergeFromM[F[_]: Sync](input: CodedInputStream): F[A]

}
