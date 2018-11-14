package coop.rchain.node.configuration.toml

import java.io.File
import java.nio.file.{Path, Paths}

import scala.io.Source
import scala.util.Try
import cats.syntax.either._
import coop.rchain.comm.PeerNode
import coop.rchain.shared.Resources._
import toml._
import toml.Codecs._

import scala.concurrent.duration.{Duration, FiniteDuration}

object TomlConfiguration {
  import error._

  private implicit val bootstrapAddressCodec: Codec[PeerNode] =
    Codec {
      case (Value.Str(uri), _) =>
        PeerNode
          .fromAddress(uri)
          .map(u => Right(u))
          .getOrElse(Left((Nil, "can't parse the rnode bootstrap address")))
      case _ => Left((Nil, "the rnode bootstrap address should be a string"))
    }
  private implicit val pathCodec: Codec[Path] =
    Codec {
      case (Value.Str(uri), _) =>
        Try(Paths.get(uri)).toEither.leftMap(_ => (Nil, s"Can't parse the path $uri"))
      case _ => Left((Nil, "A path must be a string"))
    }
  private implicit val boolCodec: Codec[Boolean] = Codec {
    case (Value.Bool(value), _) => Right(value)
    case (value, _) =>
      Left((List.empty, s"Bool expected, $value provided"))
  }

  private implicit val finiteDurationCodec: Codec[FiniteDuration] = Codec {
    case (Value.Str(value), _) =>
      Duration(value) match {
        case fd: FiniteDuration => Either.right(fd)
        case _                  => Either.left((Nil, s"Failed to parse $value as FiniteDuration."))
      }
    case (value, _) =>
      Either.left((Nil, s"Failed to parse $value as FiniteDuration."))
  }

  def from(toml: String): Either[TomlConfigurationError, Configuration] =
    Toml.parse(toml) match {
      case Left(error) => Either.left(ConfigurationParseError(error))
      case Right(ast)  => from(ast)
    }

  def from(ast: Value.Tbl): Either[TomlConfigurationError, Configuration] =
    Toml.parseAs[Configuration](rewriteKeysToCamelCase(ast)) match {
      case Left((_, error)) => Either.left(ConfigurationAstError(error))
      case Right(root)      => Either.right(root)
    }

  def from(file: File): Either[TomlConfigurationError, Configuration] =
    if (file.exists())
      withResource(Source.fromFile(file))(f => from(f.mkString))
    else Either.left(ConfigurationFileNotFound(file.getAbsolutePath))

  private def rewriteKeysToCamelCase(tbl: Value.Tbl): Value.Tbl = {

    def rewriteTbl(t: Value.Tbl): Value.Tbl =
      Value.Tbl(
        t.values.map {
          case (key, t1 @ Value.Tbl(_)) => (camelify(key), rewriteTbl(t1))
          case (key, a @ Value.Arr(_))  => (camelify(key), rewriteArr(a))
          case (key, value)             => (camelify(key), value)
        }
      )

    def rewriteArr(a: Value.Arr): Value.Arr =
      Value.Arr(
        a.values.map {
          case t1 @ Value.Tbl(_) => rewriteTbl(t1)
          case a @ Value.Arr(_)  => rewriteArr(a)
          case value             => value
        }
      )

    rewriteTbl(tbl)
  }

  private def camelify(name: String): String = {
    def loop(x: List[Char]): List[Char] = (x: @unchecked) match {
      case '-' :: '-' :: rest => loop('_' :: rest)
      case '-' :: c :: rest   => Character.toUpperCase(c) :: loop(rest)
      case '-' :: Nil         => Nil
      case c :: rest          => c :: loop(rest)
      case Nil                => Nil
    }

    loop(name.toList).mkString
  }

}

object error {
  sealed trait TomlConfigurationError
  final case class ConfigurationParseError(error: String)  extends TomlConfigurationError
  final case class ConfigurationAstError(error: String)    extends TomlConfigurationError
  final case class ConfigurationFileNotFound(path: String) extends TomlConfigurationError
}
