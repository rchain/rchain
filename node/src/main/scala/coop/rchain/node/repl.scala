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

  def replProgram[F[_]: Capture: Monad: ConsoleIO: ReplService]: F[Boolean] = {
    val rep: F[Boolean] = for {
      line <- ConsoleIO[F].readLine.map(Option.apply)
      res <- line.map(_.trim) match {
              case Some("")   => ConsoleIO[F].println("").as(true)
              case Some(":q") => false.pure[F]
              case Some(program) =>
                (ReplService[F].run(program) >>= (s => ConsoleIO[F].println(s.blue))).as(true)
              case _ => false.pure[F]
            }
    } yield res

    def repl: F[Boolean] = rep >>= {
      case true  => repl
      case false => ConsoleIO[F].close.as(false)
    }

    ConsoleIO[F].println(logo) >>= kp(repl)
  }

  def evalProgram[F[_]: Monad: ReplService: ConsoleIO](fileNames: List[String]): F[Unit] =
    for {
      _   <- ConsoleIO[F].println(s"Evaluating from $fileNames")
      res <- ReplService[F].eval(fileNames)
      _   <- ConsoleIO[F].println(res)
      _   <- ConsoleIO[F].close
    } yield ()
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
