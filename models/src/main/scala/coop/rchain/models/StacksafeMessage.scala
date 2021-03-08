package coop.rchain.models

import cats.effect.Sync
import com.google.protobuf.CodedInputStream

trait StacksafeMessage[A] extends scalapb.GeneratedMessage {

//  def serializedSizeM: Memo[Int]
//
//  def mergeFromM[F[_]: Sync](input: CodedInputStream): F[A]

}
