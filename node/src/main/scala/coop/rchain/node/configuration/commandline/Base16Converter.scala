package coop.rchain.node.configuration.commandline

import coop.rchain.models.syntax._
import org.rogach.scallop.{ArgType, ValueConverter}

object Base16Converter extends ValueConverter[Array[Byte]] {
  override def parse(args: List[(String, List[String])]): Either[String, Option[Array[Byte]]] =
    args.flatMap {
      case (name, strings) => strings.map((name, _))
    } match {
      case List((name, v)) =>
        v.decodeHex
          .toRight(s"Error parsing $name. Invalid base16 encoding.")
          .map(Some(_))
      case List() => Right(None)
      case _      => Left("Expecting a single argument encoded as base16.")
    }
  override val argType: ArgType.V = ArgType.SINGLE
}
