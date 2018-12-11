package coop.rchain.node.effects

import scala.tools.jline._
import scala.tools.jline.console._
import completer.StringsCompleter
import scala.collection.JavaConverters._
import cats._
import cats.data._
import cats.implicits._
import coop.rchain.catscontrib._
import Catscontrib._
import ski._
import TaskContrib._
import coop.rchain.shared.StringOps.ColoredString
import coop.rchain.shared.TerminalMode
import monix.eval.Task

class JLineConsoleIO(console: ConsoleReader) extends ConsoleIO[Task] {
  private val mode = TerminalMode.readMode

  def readLine: Task[String] = Task.delay {
    console.readLine
  }
  def println(str: String): Task[Unit] = Task.delay {
    console.println(str)
    console.flush()
  }
  def updateCompletion(history: Set[String]): Task[Unit] = Task.delay {
    console.getCompleters.asScala.foreach(c => console.removeCompleter(c))
    console.addCompleter(new StringsCompleter(history.asJava))
  }

  def close: Task[Unit] = Task.delay {
    TerminalFactory.get().restore()
  }
  def println(str: ColoredString): Task[Unit] =
    Task
      .delay {
        if (mode) {
          str.colorize
        } else {
          str.str
        }
      }
      .flatMap(println)
}
