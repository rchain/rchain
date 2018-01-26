package main.scala.coop.rchain.storage.regex

import org.scalatest._

class PathRegexUnitTests extends FlatSpec with Matchers {

  def doCheck(path: String,
              tokens: Option[List[PathToken]],
              //matchCases : Seq[(String, Seq[String])],
              //compileCases : Seq[String],
              options: PathRegexOptions = PathRegexOptions.default): Unit = {
    val parsed = PathRegex.parse(path, options)
    tokens.foreach(t => assert(parsed == t))
  }

  def check(path: String,
            tokens: List[Any],
            options: PathRegexOptions = PathRegexOptions.default): Unit =
    doCheck(path, Some(tokens.map {
      case t: PathToken => t
      case s: String    => PathToken(s)
    }))

  "Simple paths" should "parse" in {
    check("/", "/" :: Nil)
    check("/test", "/test" :: Nil)
    check("/test/", "/test/" :: Nil)
  }

  "Case-sensitive" should "work" in {
    check("/test", "/test" :: Nil, PathRegexOptions.caseSensitive)
    check("/TEST", "/TEST" :: Nil, PathRegexOptions.caseSensitive)
  }

  "Strict mode" should "work" in {
    check("/test", "/test" :: Nil, PathRegexOptions.strict)
    check("/test/", "/test/" :: Nil, PathRegexOptions.strict)
    check(
      "/:test/",
      PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: PathToken(
        "/") :: Nil,
      PathRegexOptions.strict)
  }

  "Non-ending mode" should "work" in {
    check("/test", "/test" :: Nil, PathRegexOptions.nonEnd)
    check("/test/", "/test/" :: Nil, PathRegexOptions.nonEnd)
    check(
      "/:test",
      PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: Nil,
      PathRegexOptions.nonEnd)
    check(
      "/:test/",
      PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: PathToken(
        "/") :: Nil,
      PathRegexOptions.nonEnd)
  }

  "Combine modes" should "work" in {
    val options = PathRegexOptions(end = false, strict = true)
    check("/test", "/test" :: Nil, options)
    check("/test/", "/test/" :: Nil, options)
    check("/test.json", "/test.json" :: Nil, options)
    check(
      "/:test",
      PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: Nil,
      options)
    check(
      "/:test/",
      PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: PathToken(
        "/") :: Nil,
      options)
  }

  "Optional named parameter" should "work" in {
    check("/:test?",
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = true,
                    repeat = false,
                    partial = false,
                    """[^\/]+?""") :: Nil,
          PathRegexOptions.nonEnd)
  }
}
