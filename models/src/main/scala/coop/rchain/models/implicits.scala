package coop.rchain.models

import cats.implicits._
import coop.rchain.models.Channel.ChannelInstance.Quote
import coop.rchain.rspace.Serialize._
import coop.rchain.rspace.{Serialize, Match => StorageMatch}

object implicits {

  implicit val serializePar: Serialize[Par]         = mkProtobufInstance(Par)
  implicit val serializeChannel: Serialize[Channel] = mkProtobufInstance(Channel)
  implicit val serializeVar: Serialize[Var]         = mkProtobufInstance(Var)
  implicit val serializeSend: Serialize[Send]       = mkProtobufInstance(Send)
  implicit val serializeReceive: Serialize[Receive] = mkProtobufInstance(Receive)
  implicit val serializeEval: Serialize[Eval]       = mkProtobufInstance(Eval)
  implicit val serializeNew: Serialize[New]         = mkProtobufInstance(New)
  implicit val serializeExpr: Serialize[Expr]       = mkProtobufInstance(Expr)
  implicit val serializeMatch: Serialize[Match]     = mkProtobufInstance(Match)

}
