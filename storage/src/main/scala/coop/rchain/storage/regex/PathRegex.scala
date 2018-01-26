package main.scala.coop.rchain.storage.regex

import java.nio.charset.StandardCharsets
import scala.annotation.tailrec

private[regex] trait ParseOptions {
  val delimiter: Char
  val delimiters: Set[Char]
}

private[regex] trait RegexOptions extends ParseOptions {
  val caseSensitive: Boolean
  val strict: Boolean
  val end: Boolean
  val endsWith: Either[String, List[String]]
}

object PathRegexOptions {
  val default       = PathRegexOptions()
  val caseSensitive = PathRegexOptions(caseSensitive = true)
  val strict        = PathRegexOptions(strict = true)
  val nonEnd        = PathRegexOptions(end = false)
}

case class PathRegexOptions(
    caseSensitive: Boolean = false,
    strict: Boolean = false,
    end: Boolean = true,
    delimiter: Char = '/',
    delimiters: Set[Char] = "./".toSet,
    endsWith: Either[String, List[String]] = Right(Nil)
) extends RegexOptions {}

object PathRegex {
  private[this] val defaultDelimiter = '/'

  private[this] val defaultDelimiters = "./".toSet

  private[this] val rxEscapeString = """([.+*?=^!:${}()\[\]|/\\])""".r

  /**
    * Escape a regular expression string.
    */
  def escapeString(str: String): String =
    rxEscapeString.replaceAllIn(str, """\\$1""")

  private[this] val utf8charSet = StandardCharsets.UTF_8

  private[this] val uriAllowedChars =
    (('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9') ++ "-_.!~*'()".toList).toSet

  /**
    * Accordingly to
    * https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/encodeURIComponent
    * encodeURIComponent escapes all characters except: A-Z a-z 0-9 - _ . ! ~ * ' ( )
    */
  private[regex] def encodeUriComponent(str: String): String =
    if (str.forall(uriAllowedChars.contains)) {
      //fastpath - namespaces and storage keys will have only allowed chars in most cases, nothing to change
      str
    } else {
      //we need to transform our string to UTF-8 (internally, Java/Scala uses UTF-16)
      str
        .map(c => {
          if (uriAllowedChars.contains(c)) {
            c.toString
          } else {
            c.toString
              .getBytes(utf8charSet)
              .map(b => "%02X".format(b))
              .mkString("")
          }
        })
        .mkString("")
    }

  private[this] val rxEscapeGroup = """([=!:$/()])""".r

  /**
    * Escape the capturing group by escaping special characters and meaning.
    */
  private[regex] def escapeGroup(group: String): String =
    rxEscapeGroup.replaceAllIn(group, """\\$1""")

  private[this] val rxPath =
    """(\\.)|(?:\:(\w+)(?:\(((?:\\.|[^\\()])+)\))?|\(((?:\\.|[^\\()])+)\))([+*?])?""".r

  /**
    * Parse a string for the raw tokens.
    */
  def parse(str: String, options: ParseOptions): List[PathToken] = {
    val defaultDelimiter = options.delimiter
    val delimiters       = options.delimiters

    case class ParseState(subStr: String,
                          rawPathPart: String,
                          tokens: List[PathToken],
                          pathEscaped: Boolean)

    @tailrec
    def toTokens(parseState: ParseState): ParseState = {
      val mcFound = rxPath.findFirstMatchIn(parseState.subStr)
      mcFound match {
        case Some(mc) =>
          val rawPathPart = if (mc.start > 0) {
            parseState.rawPathPart + parseState.subStr.substring(0, mc.start)
          } else {
            parseState.rawPathPart
          }

          val grpEscaped = Option(mc.group(1))
          if (grpEscaped.isDefined) {
            //we found escape sequence, add it to our collectedPath
            toTokens(
              ParseState(rawPathPart + grpEscaped.map(_(1)),
                         parseState.subStr.substring(mc.end),
                         parseState.tokens,
                         true))
          } else {
            val grpName     = Option(mc.group(2))
            val grpCapture  = Option(mc.group(3))
            val grpGroup    = Option(mc.group(4))
            val grpModifier = Option(mc.group(5))

            val (prev, actualPath) =
              if (!parseState.pathEscaped && rawPathPart.nonEmpty) {
                val k            = rawPathPart.length - 1
                val lastPathChar = rawPathPart(k)
                if (delimiters.contains(lastPathChar)) {
                  (Some(lastPathChar), rawPathPart.substring(0, k))
                } else {
                  (None, rawPathPart)
                }
              } else {
                (None, rawPathPart)
              }

            val tokens = if (actualPath.nonEmpty) {
              PathToken(actualPath) :: parseState.tokens
            } else {
              parseState.tokens
            }

            val next =
              if (mc.end < parseState.subStr.length)
                Some(parseState.subStr(mc.end))
              else None

            val partial   = prev.isDefined && next.isDefined && (next != prev)
            val repeat    = grpModifier.contains("+") || grpModifier.contains("*")
            val optional  = grpModifier.contains("?") || grpModifier.contains("*")
            val delimiter = prev.getOrElse(defaultDelimiter)
            val key       = tokens.count(_.isToken)
            val pattern   = if (grpCapture.nonEmpty) grpCapture else grpGroup

            val userToken = PathToken(
              grpName,
              key,
              prev,
              Some(delimiter),
              optional,
              repeat,
              partial,
              pattern
                .map(escapeGroup)
                .getOrElse("[^%s]+?".format(escapeString(delimiter.toString))))

            toTokens(
              ParseState(parseState.subStr.substring(mc.end), "", userToken :: tokens, false))
          }
        case None =>
          val finalCollectedPath = parseState.rawPathPart + parseState.subStr
          if (finalCollectedPath.isEmpty)
            parseState
          else
            //some chars were left in path, but they are not a token
            ParseState("", "", PathToken(finalCollectedPath) :: parseState.tokens, false)
      }
    }
    toTokens(ParseState(str, "", Nil, false)).tokens.reverse
  }

  /**
    * Compile a string to a template function for the path.
    */
  def compile(str: String): Either[Throwable, PathRegex] =
    compile(str, PathRegexOptions.default)

  /**
    * Compile a string to a template function for the path.
    */
  def compile(str: String, options: PathRegexOptions): Either[Throwable, PathRegex] =
    tokensToFunction(parse(str, options))

  /**
    * Expose a method for transforming tokens into the path function.
    */
  def tokensToFunction(tokens: List[PathToken]): Either[Throwable, PathRegex] =
    throw new NotImplementedError("TODO")

  def apply(str: String): Either[Throwable, PathRegex] = compile(str)
  def apply(str: String, options: PathRegexOptions): Either[Throwable, PathRegex] =
    compile(str, options)
}

private object PathToken {
  def apply(name: Option[String],
            key: Int,
            prefix: Option[Char],
            delimiter: Option[Char],
            optional: Boolean,
            repeat: Boolean,
            partial: Boolean,
            pattern: String) =
    new PathToken(name, key, prefix, delimiter, optional, repeat, partial, Some(pattern), None)
  def apply(substring: String) =
    new PathToken(None, -1, None, None, false, false, false, None, Some(substring))
}

private[regex] case class PathToken(name: Option[String],
                                    key: Int,
                                    prefix: Option[Char],
                                    delimiter: Option[Char],
                                    optional: Boolean,
                                    repeat: Boolean,
                                    partial: Boolean,
                                    pattern: Option[String],
                                    rawPathPart: Option[String]) {
  def isRawPathPart: Boolean = rawPathPart.isDefined

  def isToken: Boolean = rawPathPart.isEmpty
}

class PathRegex {
  def toPath(args: List[(String, Any)] = Nil): Either[Throwable, String] =
    throw new NotImplementedError("TODO")

  def toPath(arg: (String, Any)): Either[Throwable, String] = toPath(arg :: Nil)
}
