package coop.rchain.node

import coop.rchain.shared.StringOps._
import cats._
import cats.data._
import cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._
import ski.kp
import coop.rchain.node.effects.{ConsoleIO, ReplClient}

class ReplRuntime() {

  private val logo: String =
    """
  ╦═╗┌─┐┬ ┬┌─┐┬┌┐┌  ╔╗╔┌─┐┌┬┐┌─┐  ╦═╗╔═╗╔═╗╦  
  ╠╦╝│  ├─┤├─┤││││  ║║║│ │ ││├┤   ╠╦╝║╣ ╠═╝║  
  ╩╚═└─┘┴ ┴┴ ┴┴┘└┘  ╝╚╝└─┘─┴┘└─┘  ╩╚═╚═╝╩  ╩═╝
    """

  def replProgram[F[_]: Capture: Monad: ConsoleIO: ReplClient: TerminalMode]: F[Boolean] = {
    implicit val isTty: Boolean = TerminalMode[F].interactive.exists(Function.const(true))

    def run(program: String): F[Boolean] =
      for {
        result <- ReplClient[F].run(program)
        _      <- printResult(result)
      } yield result.isRight

    def printResult(result: Either[Throwable, String]): F[Unit] =
      result match {
        case Left(ex)   => ConsoleIO[F].println(s"Error: ${ex.getMessage}".red)
        case Right(res) => ConsoleIO[F].println(res.blue)
      }

    val rep: F[Boolean] = for {
      line <- ConsoleIO[F].readLine.map(Option.apply)
      res <- line.map(_.trim) match {
              case Some("")      => ConsoleIO[F].println("").as(true)
              case Some(":q")    => false.pure[F]
              case Some(program) => run(program)
              case _             => false.pure[F]
            }
    } yield res

    def repl: F[Boolean] = rep >>= {
      case true  => repl
      case false => ConsoleIO[F].close.as(false)
    }

    TerminalMode[F].interactive match {
      case Some(_) => ConsoleIO[F].println(logo.red) >>= kp(repl)
      case None => repl
    }
  }

  def evalProgram[F[_]: Monad: ReplClient: ConsoleIO: TerminalMode](fileNames: List[String]): F[Unit] = {

    implicit val isTty: Boolean = TerminalMode[F].interactive.exists(Function.const(true))

    def printResult(result: Either[Throwable, String]): F[Unit] =
      result match {
        case Left(ex)   => ConsoleIO[F].println(s"Error: ${ex.getMessage}".red)
        case Right(res) => ConsoleIO[F].println(res)
      }

    def printResults(results: List[(String, Either[Throwable, String])]): F[Unit] =
      results.traverse {
        case (fileName, result) =>
          for {
            _ <- ConsoleIO[F].println("")
            _ <- ConsoleIO[F].println(s"Result for $fileName:".blue)
            _ <- printResult(result)
          } yield ()
      }.void

    for {
      _   <- ConsoleIO[F].println(s"Evaluating from ${fileNames.mkString(", ")}")
      res <- ReplClient[F].eval(fileNames)
      _   <- printResults(fileNames.zip(res))
      _   <- ConsoleIO[F].close
    } yield ()
  }
}

object ReplRuntime {
  def keywords: List[String] = List(
    "stdout",
    "stdoutack",
    "stderr",
    "stderrack",
    "for",
    "!!"
  )
}
