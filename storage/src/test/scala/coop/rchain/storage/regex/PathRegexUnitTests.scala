package coop.rchain.storage.regex
import org.scalatest._
import scala.language.implicitConversions

class PathRegexUnitTests extends FlatSpec with Matchers {

  class ExPathRegex(value: PathRegex) {
    def accept(matchCases: (String, Seq[String])*): ExPathRegex =
      this

    def build(args: (Map[String, Any], Option[String])*): ExPathRegex =
      this

    def struct(testTokens: List[PathToken]): ExPathRegex = {
      assert(testTokens == value.tokens)
      this
    }
  }

  def parse(path: String, options: PathRegexOptions = PathRegexOptions.default): ExPathRegex =
    new ExPathRegex(PathRegex.compile(path, options))

  "Simple paths" should "parse" in {
    parse("/")
      .struct(List(PathToken("/")))
      .accept(("/", List("/")), ("/route", Nil))
      .build((Map(), Some("/")), (Map("id" -> 123), Some("/")))

    parse("/test")
      .struct(List(PathToken("/test")))
      .accept(("/test", List("/test")),
              ("/route", Nil),
              ("/test/route", Nil),
              ("/test/", List("/test/")))
      .build((Map(), Some("/")), (Map("id" -> 123), Some("/")))

    parse("/test/")
      .struct(List(PathToken("/test/")))
      .accept(("/test", Nil), ("/test/", List("/test/")), ("/test//", List("/test//")))
      .build((Map(), Some("/test/")))
  }

  "Simple path" should "support escaped groups" in {
    parse("/te\\~st", PathRegexOptions.caseSensitive)
      .struct(List(PathToken("/te~st")))
      .accept(("/te~st", List("/te~st")), ("/TE~ST", Nil))
      .build((Map(), Some("/te\\~st")))
  }

  "Case-sensitive" should "work" in {
    parse("/test", PathRegexOptions.caseSensitive)
      .struct(List(PathToken("/test")))
      .accept(("/test", List("/test")), ("/TEST", Nil))
      .build((Map(), Some("/test")))

    parse("/TEST", PathRegexOptions.caseSensitive)
      .struct(List(PathToken("/TEST")))
      .accept(("/test", Nil), ("/TEST", List("/TEST")))
      .build((Map(), Some("/TEST")))
  }

  "Strict mode" should "work" in {
    parse("/test", PathRegexOptions.strict)
      .struct(List(PathToken("/test")))
      .accept(("/test", List("/test")), ("/test/", Nil), ("/TEST", List("/TEST")))
      .build((Map(), Some("/test")))

    parse("/test/", PathRegexOptions.strict)
      .struct(List(PathToken("/test/")))
      .accept(("/test", Nil), ("/test/", List("/test/")), ("/test//", Nil))
      .build((Map(), Some("/test/")))
  }

  "Non-ending mode" should "work" in {
    parse("/test", PathRegexOptions.nonEnd)
      .struct(List(PathToken("/test")))
      .accept(("/test/route", List("/test")),
              ("/test", List("/test")),
              ("/test/", List("/test/")),
              ("/route", Nil))
      .build((Map(), Some("/test")))

    parse("/test/", PathRegexOptions.nonEnd)
      .struct(List(PathToken("/test/")))
      .accept(("/test/route", List("/test/")),
              ("/test//route", List("/test/")),
              ("/test", Nil),
              ("/test//", List("/test//")))
      .build((Map(), Some("/test/")))

    parse("/:test", PathRegexOptions.nonEnd)
      .struct(
        List(PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""")))
      .accept(("/route", List("/route", "route")))
      .build((Map(), None),
             (Map("test" -> "abc"), Some("/abc")),
             (Map("test" -> "a+b"), Some("/a%2Bb")))

    parse("/:test/", PathRegexOptions.nonEnd)
      .struct(
        List(PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?"""),
             PathToken("/")))
      .accept(("/route", Nil), ("/route/", List("/route/", "route")))
      .build((Map(), None), (Map("test" -> "abc"), Some("/abc/")))
  }

  val noEndStrictOptions = PathRegexOptions(end = false, strict = true)

  "Combine modes" should "accept simple paths" in {
    parse("/test", noEndStrictOptions)
      .struct(List(PathToken("/test")))
      .accept(("/test", List("/test")), ("/test/", List("/test")), ("/test/route", List("/test")))
      .build((Map(), Some("/test")))

    parse("/test/", noEndStrictOptions)
      .struct(List(PathToken("/test/")))
      .accept(("/test", Nil),
              ("/test/", List("/test/")),
              ("/test//", List("/test/")),
              ("/test/route", List("/test/")))
      .build((Map(), Some("/test/")))
  }

  "Combine modes" should "accept file path" in {
    parse("/test.json", noEndStrictOptions)
      .struct(List(PathToken("/test.json")))
      .accept(("/test.json", List("/test.json")),
              ("/test.json.hbs", Nil),
              ("/test.json/route", List("/test.json")))
      .build((Map(), Some("/test.json")))
  }

  "Combine modes" should "work with named argument" in {
    parse("/:test", noEndStrictOptions)
      .struct(
        List(PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""")))
      .accept(("/route", List("/route", "route")), ("/route/", List("/route", "route")))
      .build((Map(), None), (Map("test" -> "abc"), Some("/abc")))

    parse("/:test/", noEndStrictOptions)
      .struct(
        List(PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?"""),
             PathToken("/")))
      .accept(("/route", Nil), ("/route/", List("/route/", "route")))
      .build((Map(), None), (Map("test" -> "foobar"), Some("/foobar/")))
  }

  "Single named parameter" should "pass test set" in {
    parse("/:test")
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = false,
                    repeat = false,
                    partial = false,
                    """[^\/]+?""")))
      .accept(
        ("/route", List("/route", "route")),
        ("/another", List("/another", "another")),
        ("/something/else", Nil),
        ("/route.json", List("/route.json", "route.json")),
        ("/something%2Felse", List("/something%2Felse", "/something%2Felse")),
        ("/something%2Felse%2Fmore", List("/something%2Felse%2Fmore", "something%2Felse%2Fmore")),
        ("/;,:@&=+$-_.!~*()", List("/;,:@&=+$-_.!~*()", ";,:@&=+$-_.!~*()"))
      )
      .build(
        (Map("test" -> "route"), Some("/route")),
        (Map("test" -> "something/else"), Some("/something%2Felse")),
        (Map("test" -> "something/else/more"), Some("/something%2Felse%2Fmore"))
      )
  }

  "Named parameter" should "support strict mode" in {
    parse("/:test", PathRegexOptions.strict)
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = false,
                    repeat = false,
                    partial = false,
                    """[^\/]+?""")))
      .accept(("/route", List("/route", "route")), ("/route", Nil))
      .build((Map("test" -> "route"), Some("/route")))

    parse("/:test/", PathRegexOptions.strict)
      .struct(
        List(PathToken(Some("test"),
                       0,
                       Some('/'),
                       Some('/'),
                       optional = false,
                       repeat = false,
                       partial = false,
                       """[^\/]+?"""),
             PathToken("/")))
      .accept(("/route/", List("/route/", "route")), ("/route//", Nil))
      .build((Map("test" -> "route"), Some("/route/")))
  }

  "Named parameter" should "support non-ending mode" in {
    parse("/:test", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = false,
                    repeat = false,
                    partial = false,
                    """[^\/]+?""")))
      .accept(("/route.json", List("/route.json", "route.json")),
              ("/route//", List("/route", "route")))
      .build((Map("test" -> "route"), Some("/route")))
  }

  "Optional named parameter" should "work" in {
    parse("/:test?")
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = true,
                    repeat = false,
                    partial = false,
                    """[^\/]+?""")))
      .accept(("/route", List("/route", "route")),
              ("/route/nested", Nil),
              ("/", List("/", null)),
              ("//", Nil))
  }

  "Optional named parameter" should "work with strict mode" in {
    parse("/:test?", PathRegexOptions.strict)
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = true,
                    repeat = false,
                    partial = false,
                    """[^\/]+?""")))
      .accept(("/route", List("/route", "route")), ("/", Nil), ("//", Nil))
      .build((Map(), Some("")), (Map("test" -> "foobar"), Some("/foobar")))

    parse("/:test?/", PathRegexOptions.strict)
      .struct(
        List(PathToken(Some("test"),
                       0,
                       Some('/'),
                       Some('/'),
                       optional = true,
                       repeat = false,
                       partial = false,
                       """[^\/]+?"""),
             PathToken("/")))
      .accept(("/route", Nil),
              ("/route/", List("/route/", "route")),
              ("/", List("/", null)),
              ("//", Nil))
      .build((Map(), Some("/")), (Map("test" -> "foobar"), Some("/foobar/")))
  }

  "Optional named parameter" should "work well in the middle of path" in {
    parse("/:test?/bar")
      .struct(
        List(PathToken(Some("test"),
                       0,
                       Some('/'),
                       Some('/'),
                       optional = true,
                       repeat = false,
                       partial = false,
                       """[^\/]+?"""),
             PathToken("/bar")))
      .accept(("/foo/bar", List("/foo/bar", "foo")))
      .build((Map("test" -> "foo"), Some("/foo/bar")))

    parse("/:test?-bar")
      .struct(
        List(PathToken(Some("test"),
                       0,
                       Some('/'),
                       Some('/'),
                       optional = true,
                       repeat = false,
                       partial = true,
                       """[^\/]+?"""),
             PathToken("-bar")))
      .accept(("/-bar", List("/-bar", null)), ("/foo-bar", List("/foo-bar", "foo")))
      .build((Map("test" -> "aaa"), Some("/aaa-bar")))

    parse("/:test*-bar")
      .struct(
        List(PathToken(Some("test"),
                       0,
                       Some('/'),
                       Some('/'),
                       optional = true,
                       repeat = true,
                       partial = true,
                       """[^\/]+?"""),
             PathToken("-bar")))
      .accept(("/-bar", List("/-bar", null)),
              ("/foo-bar", List("/foo-bar", "foo")),
              ("/foo/baz-bar", List("/foo/baz-bar", "foo/baz")))
      .build((Map("test" -> "aaa"), Some("/aaa-bar")),
             (Map("test" -> List("aaa", "bbb")), Some("/aaa-bbb-bar")))
  }

  "Repeated one or more times parameters." should "work" in {
    parse("/:test+")
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = false,
                    repeat = true,
                    partial = false,
                    """[^\/]+?""")))
      .accept(("/", Nil),
              ("//", Nil),
              ("/route", List("/route", "route")),
              ("/some/basic/route", List("/some/basic/route", "some/basic/route")))
      .build((Map(), None),
             (Map("test" -> "foobar"), Some("foobar")),
             (Map("test" -> List("a", "b", "c")), Some("/a/b/c")))
  }

  "Repeated parameter" should "support inline regex" in {
    parse("/:test(\\d+)+")
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = false,
                    repeat = true,
                    partial = false,
                    """\d+""")))
      .accept(("/abc/456/789", Nil), ("/123/456/789", List("/123/456/789", "123/456/789")))
      .build((Map("test" -> "abc"), None),
             (Map("test" -> 123), Some("/123")),
             (Map("test" -> List(1, 2, 3)), Some("/1/2/3")))

    parse("/route.:ext(json|xml)+")
      .struct(
        List(PathToken("/route"),
             PathToken(Some("ext"),
                       0,
                       Some('.'),
                       Some('.'),
                       optional = false,
                       repeat = true,
                       partial = false,
                       """json|xml""")))
      .accept(("/route", Nil),
              ("/route.json", List("/route.json", "json")),
              ("/route.xml.json", List("/route.xml.json", "xml.json")),
              ("/route.html", Nil))
      .build((Map("ext" -> "foobar"), None),
             (Map("ext" -> "xml"), Some("/route.xml")),
             (Map("ext" -> List("xml", "json")), Some("/route.xml.json")))
  }

  "Repeated zero or more times parameters" should "be supported" in {
    parse("/:test*")
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = true,
                    repeat = true,
                    partial = false,
                    """[^\/]+?""")))
      .accept(("/", List("/", null)),
              ("//", Nil),
              ("/route", List("/route", "route")),
              ("/some/basic/route", List("/some/basic/route", "some/basic/route")))
      .build((Map(), None),
             (Map("test" -> "foobar"), Some("/foobar")),
             (Map("test" -> List("foo", "bar")), Some("/foo/bar")))

    parse("/route.:ext([a-z]+)*")
      .struct(
        List(PathToken("/route"),
             PathToken(Some("ext"),
                       0,
                       Some('.'),
                       Some('.'),
                       optional = true,
                       repeat = true,
                       partial = false,
                       """[a-z]+""")))
      .accept(("/route", List("/route", null)),
              ("/route.json", List("/route.json", "json")),
              ("/route.json.xml", List("/route.json.xml", "json.xml")),
              ("/route.123", Nil))
      .build(
        (Map(), Some("/route")),
        (Map("ext" -> Nil), Some("/route")),
        (Map("ext" -> "123"), None),
        (Map("ext" -> "foobar"), Some("/route.foobar")),
        (Map("ext" -> List("foo", "bar")), Some("/foute.foo.bar"))
      )
  }

  "Custom named parameters (no repeat)" should "work" in {
    parse("/:test(\\d+)")
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = false,
                    repeat = false,
                    partial = false,
                    """\d+""")))
      .accept(("/abc", Nil), ("/123", List("/123", "123")))
      .build((Map("test" -> "abc"), None), (Map("test" -> 123), Some("/123")))

    parse("/:test(\\d+)", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = false,
                    repeat = false,
                    partial = false,
                    """\d+""")))
      .accept(("/abc", Nil), ("/123", List("/123", "123")), ("/123/abc", List("/123", "123")))
      .build((Map("test" -> "abc"), None), (Map("test" -> 123), Some("/123")))
  }

  "Custom named parameter" should "support wildcard" in {
    parse("/:test(.*)")
      .struct(
        List(
          PathToken(Some("test"),
                    0,
                    Some('/'),
                    Some('/'),
                    optional = false,
                    repeat = false,
                    partial = false,
                    """.*""")))
      .accept(("/anything/goes/here", List("/anything/goes/here", "anything/goes/here")),
              ("/;,:@&=/+$-_.!/~*()", List("/;,:@&=/+$-_.!/~*()", ";,:@&=/+$-_.!/~*()")))
      .build(
        (Map("test" -> ""), Some("/")),
        (Map("test" -> "abc"), Some("/abc")),
        (Map("test" -> "abc/123"), Some("/abc%2F123")),
        (Map("test" -> "abc/123/456"), Some("/abc%2F123%2F456"))
      )
  }
}
