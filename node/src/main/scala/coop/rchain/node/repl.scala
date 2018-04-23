package coop.rchain.node

import coop.rchain.shared.StringOps._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski.kp

class ReplRuntime(conf: Conf) {

  private val logo: String =
    """
  ╦═╗┌─┐┬ ┬┌─┐┬┌┐┌  ╔╗╔┌─┐┌┬┐┌─┐  ╦═╗╔═╗╔═╗╦  
  ╠╦╝│  ├─┤├─┤││││  ║║║│ │ ││├┤   ╠╦╝║╣ ╠═╝║  
  ╩╚═└─┘┴ ┴┴ ┴┴┘└┘  ╝╚╝└─┘─┴┘└─┘  ╩╚═╚═╝╩  ╩═╝
    """.red

  def replProgram[F[_]: Monad: ConsoleIO: ReplService]: F[Unit] = {
    val rep: F[Unit] = for {
      line <- ConsoleIO[F].readLine
      _ <- line.trim match {
            case ""   => ConsoleIO[F].println("")
            case line => ReplService[F].run(line) >>= (s => ConsoleIO[F].println(s.blue))
          }
    } yield ()

    val repl: F[Unit] = rep.forever

    ConsoleIO[F].println(logo) >>= kp(repl)
  }

  def evalProgram[F[_]: Monad: ReplService: ConsoleIO](fileName: String): F[Unit] =
    for {
      _   <- ConsoleIO[F].println(s"Evaluating from $fileName")
      res <- ReplService[F].eval(fileName)
      _   <- ConsoleIO[F].println(res)
    } yield ()

}

object ReplRuntime {
  def keywords: List[String] = List(
    "stdOut",
    "stdOutAck",
    "stdErr",
    "stdErrAck",
    "for",
    "!!"
  )
}
