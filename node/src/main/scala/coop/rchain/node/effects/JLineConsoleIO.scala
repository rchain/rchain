package coop.rchain.node.effects

import scala.tools.jline._
import scala.tools.jline.console._, completer.StringsCompleter
import scala.collection.JavaConverters._
import cats._, cats.data._, cats.implicits._
import coop.rchain.catscontrib._, Catscontrib._, ski._, TaskContrib._
import monix.eval.Task

class JLineConsoleIO(console: ConsoleReader) extends ConsoleIO[Task] {
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

}
