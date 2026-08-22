package coop.rchain.shared

import cats.effect.Sync

/**
  * A util class for effortless, systematic and useful debug messages.
  * Best explained with example usage:
  *
  * {{{
  *   def someMethod(x: Int, y: String, z: ByteString): F[Unit] = for {
  *     _ <- Debug.print(x, y, Base16.encode(z.toByteArray))
  *     // do someMethod stuff
  *   }
  * }}}
  *
  * when someMethod is called as below:
  *
  * {{{
  *   someMethod(42, "Hi!", ByteString.fromHex("0xC0FFEE"))
  * }}}
  *
  *  the following will be written into the stdout:
  *
  * {{{
  *   1234.567 someMethod (SomeClass.scala:123)
  *              x = 42
  *              y = Hi!
  *              Base16.encode(z.toByteArray) = "0xCOFFEE"
  * }}}
  *
  * Works best with IntelliJ's AwesomeConsole plugin, which makes the source location a hyperlink (!).
  */
object Debug {

  // It's actually 'this class' loading time', but is good enough for having a relative measure
  private val startupTime = System.currentTimeMillis()

  def print[F[_]: Sync](values: sourcecode.Text[Any]*)(
      implicit enclosing: sourcecode.Enclosing,
      file: sourcecode.File,
      line: sourcecode.Line
  ): F[Unit] =
    Sync[F].delay {
      printUnsafe(values: _*)(enclosing, file, line)
    }

  def printUnsafe(values: sourcecode.Text[Any]*)(
      implicit enclosing: sourcecode.Enclosing,
      file: sourcecode.File,
      line: sourcecode.Line
  ): Unit =
    println(string(values: _*)(enclosing, file, line))

  def string(values: sourcecode.Text[Any]*)(
      implicit enclosing: sourcecode.Enclosing,
      file: sourcecode.File,
      line: sourcecode.Line
  ): String = {

    val name     = suffixAfterLast(".", enclosing.value)
    val filename = suffixAfterLast("/", file.value)

    val valueIndent = f"${""}%11s" //11 spaces. 8 for timestamp field, one for space past it, 2 for indent from method
    val valuesText =
      if (values.isEmpty) ""
      else "\n" + values.map(v => s"$valueIndent${v.source} = ${v.value}").mkString("\n")
    val timestamp = (System.currentTimeMillis() - startupTime) / 1e3d

    f"$timestamp% 8.3f $name($filename:${line.value})$valuesText"
  }

  private def suffixAfterLast(pattern: String, string: String): String = {
    val pos   = string.lastIndexOf(pattern)
    val start = Math.max(-1, Math.min(pos + pattern.length, string.length))
    string.substring(start)
  }
}
