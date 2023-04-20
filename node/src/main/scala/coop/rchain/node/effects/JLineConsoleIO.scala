package coop.rchain.node.effects

import scala.tools.jline._
import scala.tools.jline.console._
import completer.StringsCompleter
import scala.jdk.CollectionConverters._
import cats._
import cats.data._
import cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._
import ski._
import TaskContrib._
import cats.effect.Sync
import coop.rchain.shared.StringOps.ColoredString
import coop.rchain.shared.TerminalMode

@SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
class JLineConsoleIO[F[_]: Sync](console: ConsoleReader) extends ConsoleIO[F] {
  private val mode = TerminalMode.readMode

  def readLine: F[String] = Sync[F].delay {
    console.readLine
  }

  def readPassword(prompt: String): F[String] = Sync[F].delay {
    console.readLine(prompt, '*')
  }

  def println(str: String): F[Unit] = Sync[F].delay {
    console.println(str)
    console.flush()
  }

  def updateCompletion(history: Set[String]): F[Unit] = Sync[F].delay {
    console.getCompleters.asScala.foreach(c => console.removeCompleter(c))
    console.addCompleter(new StringsCompleter(history.asJava))
  }

  def close: F[Unit] = Sync[F].delay {
    TerminalFactory.get().restore()
  }

  def println(str: ColoredString): F[Unit] =
    Sync[F]
      .delay {
        if (mode) {
          str.colorize
        } else {
          str.str
        }
      }
      .flatMap(println)
}
