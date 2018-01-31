package coop.rchain.storage.regex
import org.scalatest._
import scala.language.implicitConversions

class PathRegexUnitTests extends FlatSpec with Matchers {

  implicit class ExPathRegex(value: PathRegex) {
    def accept(matchCases: (String, Seq[String])*): ExPathRegex =
      this

    def build(buildCases: (Seq[String], String)*): ExPathRegex =
      this

    def struct(testTokens: List[PathToken]): ExPathRegex = {
      assert(testTokens == value.tokens)
      this
    }
  }

  implicit def stringToPathToken(str: String): PathToken = PathToken(str)

  def pt(str: String): PathToken = PathToken(str)

  def parse(path: String, options: PathRegexOptions = PathRegexOptions.default): ExPathRegex =
    PathRegex.compile(path, options)

  "Simple paths" should "parse" in {
    parse("/")
      .struct("/" :: Nil)
      .accept(("/", "/" :: Nil), ("/route", Nil))

    parse("/test")
      .struct("/test" :: Nil)
      .accept(("/test", "/test" :: Nil),
              ("/route", Nil),
              ("/test/route", Nil),
              ("/test/", "/test/" :: Nil))

    parse("/test/")
      .struct("/test/" :: Nil)
      .accept(("/test", Nil), ("/test/", "/test/" :: Nil), ("/test//", "/test//" :: Nil))
  }

  "Simple path" should "support escaped groups" in {
    parse("/te\\~st", PathRegexOptions.caseSensitive)
      .struct("/te~st" :: Nil)
      .accept(("/te~st", "/te~st" :: Nil), ("/TE~ST", Nil))
  }

  "Case-sensitive" should "work" in {
    parse("/test", PathRegexOptions.caseSensitive)
      .struct("/test" :: Nil)
      .accept(("/test", "/test" :: Nil), ("/TEST", Nil))

    parse("/TEST", PathRegexOptions.caseSensitive)
      .struct("/TEST" :: Nil)
      .accept(("/test", Nil), ("/TEST", "/TEST" :: Nil))
  }

  "Strict mode" should "work" in {
    parse("/test", PathRegexOptions.strict)
      .struct("/test" :: Nil)
      .accept(("/test", "/test" :: Nil), ("/test/", Nil), ("/TEST", "/TEST" :: Nil))

    parse("/test/", PathRegexOptions.strict)
      .struct("/test/" :: Nil)
      .accept(("/test", Nil), ("/test/", "/test/" :: Nil), ("/test//", Nil))
  }

  "Non-ending mode" should "work" in {
    parse("/test", PathRegexOptions.nonEnd)
      .struct("/test" :: Nil)
      .accept(("/test/route", "/test" :: Nil),
              ("/test", "/test" :: Nil),
              ("/test/", "/test/" :: Nil),
              ("/route", Nil))

    parse("/test/", PathRegexOptions.nonEnd)
      .struct("/test/" :: Nil)
      .accept(("/test/route", "/test/" :: Nil),
              ("/test//route", "/test/" :: Nil),
              ("/test", Nil),
              ("/test//", "/test//" :: Nil))

    parse("/:test", PathRegexOptions.nonEnd)
      .struct(
        PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: Nil)
      .accept(("/route", "/route" :: "route" :: Nil))

    parse("/:test/", PathRegexOptions.nonEnd)
      .struct(
        PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: pt(
          "/") :: Nil)
      .accept(("/route", Nil), ("/route/", "/route/" :: "route" :: Nil))
  }

  val noEndStrictOptions = PathRegexOptions(end = false, strict = true)

  "Combine modes" should "accept simple paths" in {
    parse("/test", noEndStrictOptions)
      .struct("/test" :: Nil)
      .accept(("/test", "/test" :: Nil),
              ("/test/", "/test" :: Nil),
              ("/test/route", "/test" :: Nil))

    parse("/test/", noEndStrictOptions)
      .struct("/test/" :: Nil)
      .accept(("/test", Nil),
              ("/test/", "/test/" :: Nil),
              ("/test//", "/test/" :: Nil),
              ("/test/route", "/test/" :: Nil))
  }

  "Combine modes" should "accept file path" in {
    parse("/test.json", noEndStrictOptions)
      .struct("/test.json" :: Nil)
      .accept(("/test.json", "/test.json" :: Nil),
              ("/test.json.hbs", Nil),
              ("/test.json/route", "/test.json" :: Nil))
  }

  "Combine modes" should "work with named argument" in {
    parse("/:test", noEndStrictOptions)
      .struct(
        PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: Nil)
      .accept(("/route", "/route" :: "route" :: Nil), ("/route/", "/route" :: "route" :: Nil))

    parse("/:test/", noEndStrictOptions)
      .struct(
        PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: pt(
          "/") :: Nil)
      .accept(("/route", Nil), ("/route/", "/route/" :: "route" :: Nil))
  }

  "Optional named parameter" should "work" in {
    parse("/:test?", PathRegexOptions.default)
      .struct(
        PathToken(Some("test"),
                  0,
                  Some('/'),
                  Some('/'),
                  optional = true,
                  repeat = false,
                  partial = false,
                  """[^\/]+?""") :: Nil)
      .accept(("/route", "/route" :: "route" :: Nil),
              ("/route/nested", Nil),
              ("/", "/" :: null :: Nil),
              ("//", Nil))
  }

  "Optional named parameter" should "work with strict mode" in {
    parse("/:test?", PathRegexOptions.strict)
      .struct(
        PathToken(Some("test"),
                  0,
                  Some('/'),
                  Some('/'),
                  optional = true,
                  repeat = false,
                  partial = false,
                  """[^\/]+?""") :: Nil)
      .accept(("/route", "/route" :: "route" :: Nil), ("/", Nil), ("//", Nil))

    parse("/:test?/", PathRegexOptions.strict)
      .struct(
        PathToken(Some("test"),
                  0,
                  Some('/'),
                  Some('/'),
                  optional = true,
                  repeat = false,
                  partial = false,
                  """[^\/]+?""") :: pt("/") :: Nil)
      .accept(("/route", Nil),
              ("/route/", "/route/" :: "route" :: Nil),
              ("/", "/" :: null :: Nil),
              ("//", Nil))
  }
}
