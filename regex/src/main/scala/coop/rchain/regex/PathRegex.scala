package coop.rchain.regex

import scala.util.matching.Regex
import java.nio.charset.StandardCharsets

import scala.annotation.tailrec
import scala.util.Try

private[regex] trait ParseOptions {
  val delimiter: Char
  val delimiters: Set[Char]
}

private[regex] trait RegexOptions extends ParseOptions {
  val caseSensitive: Boolean
  val strict: Boolean
  val end: Boolean
  val endsWith: List[String]
}

case class PathRegexOptions(
    caseSensitive: Boolean = false,
    strict: Boolean = false,
    end: Boolean = true,
    delimiter: Char = PathRegexOptions.defaultDelimiter,
    delimiters: Set[Char] = PathRegexOptions.defaultDelimiters,
    endsWith: List[String] = Nil
) extends RegexOptions

object PathRegexOptions {
  private[regex] val defaultDelimiter  = '/'
  private[regex] val defaultDelimiters = "./".toSet

  val default       = PathRegexOptions()
  val caseSensitive = PathRegexOptions(caseSensitive = true)
  val strict        = PathRegexOptions(strict = true)
  val nonEnd        = PathRegexOptions(end = false)
}

private[regex] case class PathToken(
    name: Option[String],
    key: Int,
    prefix: Option[Char],
    delimiter: Option[Char],
    optional: Boolean,
    repeat: Boolean,
    partial: Boolean,
    pattern: Option[String],
    rawPathPart: Option[String]
) {
  def isRawPathPart: Boolean = rawPathPart.isDefined

  def isToken: Boolean = rawPathPart.isEmpty

  lazy val matchRegex: Option[Regex] = pattern.map(p => new Regex("^(?:" + p + ")$"))

  private[regex] def rawPartChar: Option[Char] =
    rawPathPart.filter(_.length > 0).map(part => part.charAt(part.length - 1))

  /**
    * This function converts token to path segment
    * We do not expect that this function will be called outside of PathRegex.toPath
    * method.
    *
    * @throws IllegalArgumentException if token couldn't be formatted
    */
  private[regex] def formatSegment(
      args: Map[String, Iterable[String]],
      encode: String => String
  ): String =
    rawPathPart.getOrElse {
      val argName = name.getOrElse(key.toString)
      val sumSegments = args.get(argName) match {
        case None | Some(Nil) =>
          if (optional) {
            prefix.filter(_ => partial).map(_.toString).getOrElse("")
          } else {
            throw new IllegalArgumentException(s"Expected value for token $argName")
          }
        case Some(singleValue) if singleValue.size == 1 =>
          val encValue = encode(singleValue.head)
          if (matchRegex.get.pattern.matcher(encValue).matches) {
            prefix.getOrElse("") + encValue
          } else {
            throw new IllegalArgumentException(
              s"Expected $argName to match pattern ${matchRegex.get.pattern}, but got value $encValue"
            )
          }
        case Some(lstValue: Iterable[String]) =>
          val allValues =
            for ((encValue, idx) <- lstValue.map(encode).zipWithIndex)
              yield
                if (matchRegex.get.pattern.matcher(encValue).matches) {
                  if (idx == 0) {
                    prefix.getOrElse("") + encValue
                  } else {
                    delimiter.getOrElse("") + encValue
                  }
                } else {
                  throw new IllegalArgumentException(
                    s"Expected $argName[$idx] to match pattern ${matchRegex.get.pattern}, but got value $encValue"
                  )
                }
          allValues.mkString
      }
      sumSegments
    }
}

private[regex] object PathToken {
  def apply(
      name: Option[String],
      key: Int,
      prefix: Option[Char],
      delimiter: Option[Char],
      optional: Boolean,
      repeat: Boolean,
      partial: Boolean,
      pattern: String
  ) =
    new PathToken(name, key, prefix, delimiter, optional, repeat, partial, Some(pattern), None)
  def apply(substring: String) =
    new PathToken(None, -1, None, None, false, false, false, None, Some(substring))
}

case class PathRegex(tokens: List[PathToken], options: RegexOptions) {
  def withOptions(newOptions: RegexOptions): PathRegex = new PathRegex(tokens, newOptions)

  /**
    * Takes some arguments (argument can be a value or a sequence of values),
    * and builds an Uri-path
    */
  def toPath(
      args: Map[String, Iterable[String]],
      encode: String => String = PathRegex.encodeUriComponent
  ): Either[Throwable, String] =
    Try(tokens.map(_.formatSegment(args, encode)).mkString).toEither

  val keys: List[PathToken] = tokens.filter(_.isToken)

  /**
    * Returns Regex that is able to parse our tokens with defined options
    */
  lazy val regex: Either[Throwable, Regex] = {
    val endsWith = ("$" :: options.endsWith.map(PathRegex.escapeString)).mkString("|")

    val route: List[Option[String]] = tokens.flatMap { token =>
      if (token.isRawPathPart) {
        token.rawPathPart.map(tokenRawPart => Some(PathRegex.escapeString(tokenRawPart)))
      } else {
        token.pattern.map(tokenPattern => {
          val prefix = token.prefix.map(PathRegex.escapeString).getOrElse("")
          val capture = if (token.repeat) {
            s"(?:$tokenPattern)(?:$prefix(?:$tokenPattern))*"
          } else {
            tokenPattern
          }

          if (token.optional) {
            if (token.partial) {
              Some(s"$prefix($capture)?")
            } else {
              Some(s"(?:$prefix($capture))?")
            }
          } else {
            Some(s"$prefix($capture)")
          }
        })
      }
    }

    val routeFinish: List[String] = if (options.end) {
      val strictFinish = if (options.strict) {
        List.empty
      } else {
        List(s"(?:${options.delimiter})?")
      }
      val routeFinish = List(if (endsWith == "$") "$" else s"(?=$endsWith)")
      strictFinish ++ routeFinish
    } else {
      val strictFinish = if (options.strict) {
        List.empty
      } else {
        List(s"(?:${options.delimiter}(?=$endsWith))?")
      }

      val isEndDelimited = tokens.nonEmpty && tokens.last.rawPartChar
        .exists(options.delimiters.contains)
      val routeFinish = if (!isEndDelimited) {
        List("(?=" + options.delimiter + "|" + endsWith + ")")
      } else {
        List.empty
      }
      strictFinish ++ routeFinish
    }

    Try {
      val patternFlags = if (options.caseSensitive) "" else "(?i)"
      new Regex(patternFlags + "^" + (route.flatten ++ routeFinish).mkString)
    }.toEither
  }
}

object PathRegex {
  private[this] val rxEscapeString = """([.+*?=^!:${}()\[\]|/\\])""".r

  /**
    * Escape a regular expression string.
    */
  def escapeString(str: String): String =
    rxEscapeString.replaceAllIn(str, """\\$1""")

  private[regex] def escapeString(c: Char): String = escapeString(c.toString)

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
        .map(
          c =>
            if (uriAllowedChars.contains(c)) {
              c.toString
            } else {
              c.toString
                .getBytes(StandardCharsets.UTF_8)
                .map(b => "%%%02X".format(b))
                .mkString("")
            }
        )
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

  private[this] case class ParseState(
      subStr: String,
      rawPathPart: String,
      tokens: List[PathToken],
      pathEscaped: Boolean
  )

  /**
    * Parse a string for the raw tokens.
    */
  def parse(str: String, options: ParseOptions): List[PathToken] = {
    @tailrec
    def toTokens(parseState: ParseState): ParseState =
      rxPath.findFirstMatchIn(parseState.subStr) match {
        case Some(mc) =>
          val rawPathPart = if (mc.start > 0) {
            parseState.rawPathPart + parseState.subStr.substring(0, mc.start)
          } else {
            parseState.rawPathPart
          }

          Option(mc.group(1)) match {
            case Some(escapeStr) =>
              //we found escape sequence, add it to our collectedPath
              toTokens(
                ParseState(
                  parseState.subStr.substring(mc.end),
                  rawPathPart + escapeStr.charAt(1),
                  parseState.tokens,
                  true
                )
              )
            case None =>
              val (prev, actualPath) =
                if (!parseState.pathEscaped && rawPathPart.nonEmpty) {
                  val k            = rawPathPart.length - 1
                  val lastPathChar = rawPathPart(k)
                  if (options.delimiters.contains(lastPathChar)) {
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

              val grpCapture  = Option(mc.group(3))
              val grpGroup    = Option(mc.group(4))
              val grpModifier = Option(mc.group(5))

              val delimiter    = prev.getOrElse(options.delimiter)
              val patternGroup = if (grpCapture.nonEmpty) grpCapture else grpGroup

              val userToken = PathToken(
                name = Option(mc.group(2)),
                key = tokens.count(_.isToken),
                prev,
                Some(delimiter),
                optional = grpModifier.contains("?") || grpModifier.contains("*"),
                repeat = grpModifier.contains("+") || grpModifier.contains("*"),
                partial = prev.isDefined && next.isDefined && (next != prev),
                pattern = patternGroup
                  .map(escapeGroup)
                  .getOrElse("[^%s]+?".format(escapeString(delimiter.toString)))
              )

              toTokens(
                ParseState(parseState.subStr.substring(mc.end), "", userToken :: tokens, false)
              )
          }
        case None =>
          val finalCollectedPath = parseState.rawPathPart + parseState.subStr
          if (finalCollectedPath.isEmpty)
            parseState
          else
            //some chars were left in path, but they are not a token
            ParseState("", "", PathToken(finalCollectedPath) :: parseState.tokens, false)
      }

    toTokens(ParseState(str, "", List.empty, false)).tokens.reverse
  }

  /**
    * Compile a string to a template function for the path.
    */
  def apply(str: String, options: PathRegexOptions = PathRegexOptions.default): PathRegex =
    PathRegex(parse(str, options), options)
}
