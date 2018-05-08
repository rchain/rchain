package coop.rchain.node

import java.util.concurrent.TimeUnit
import io.grpc.{ManagedChannel, ManagedChannelBuilder, StatusRuntimeException}
import coop.rchain.shared.StringOps._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski.kp
import monix.eval.Task

class ReplRuntime(conf: Conf) {

  private val logo: String =
    """
  ╦═╗┌─┐┬ ┬┌─┐┬┌┐┌  ╔╗╔┌─┐┌┬┐┌─┐  ╦═╗╔═╗╔═╗╦  
  ╠╦╝│  ├─┤├─┤││││  ║║║│ │ ││├┤   ╠╦╝║╣ ╠═╝║  
  ╩╚═└─┘┴ ┴┴ ┴┴┘└┘  ╝╚╝└─┘─┴┘└─┘  ╩╚═╚═╝╩  ╩═╝
    """.red

  def replProgram[F[_]: Capture: Monad: ConsoleIO: ReplService]: F[Boolean] = {
    val rep: F[Boolean] = for {
      line <- ConsoleIO[F].readLine
      res <- line.trim match {
              case ""   => ConsoleIO[F].println("").as(true)
              case ":q" => false.pure[F]
              case line =>
                (ReplService[F].run(line) >>= (s => ConsoleIO[F].println(s.blue))).as(true)
            }
    } yield res

    def repl: F[Boolean] = rep >>= {
      case true  => repl
      case false => ConsoleIO[F].close.as(false)
    }

    ConsoleIO[F].println(logo) >>= kp(repl)
  }

  def evalProgram[F[_]: Monad: ReplService: ConsoleIO](fileName: String): F[Unit] =
    for {
      _   <- ConsoleIO[F].println(s"Evaluating from $fileName")
      res <- ReplService[F].eval(fileName)
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
