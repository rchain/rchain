package coop.rchain.node.runtime

import cats.Monad
import cats.syntax.all._
import coop.rchain.node.effects.{ConsoleIO, ReplClient}
import coop.rchain.shared.StringOps._
import coop.rchain.shared.TerminalMode

class ReplRuntime() {

  private val logo: String =
    """
  ╦═╗┌─┐┬ ┬┌─┐┬┌┐┌  ╔╗╔┌─┐┌┬┐┌─┐  ╦═╗╔═╗╔═╗╦
  ╠╦╝│  ├─┤├─┤││││  ║║║│ │ ││├┤   ╠╦╝║╣ ╠═╝║
  ╩╚═└─┘┴ ┴┴ ┴┴┘└┘  ╝╚╝└─┘─┴┘└─┘  ╩╚═╚═╝╩  ╩═╝
    """

  def replProgram[F[_]: Monad: ConsoleIO: ReplClient]: F[Boolean] = {
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
      case false => false.pure[F]
    }
    if (TerminalMode.readMode) {
      for {
        _   <- ConsoleIO[F].println(logo.red)
        res <- repl
      } yield res
    } else {
      repl
    }
  }

  def evalProgram[F[_]: Monad: ReplClient: ConsoleIO](
      fileNames: List[String],
      printUnmatchedSendsOnly: Boolean
  ): F[Unit] = {
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
      res <- ReplClient[F].eval(fileNames, printUnmatchedSendsOnly)
      _   <- printResults(fileNames.zip(res))
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
