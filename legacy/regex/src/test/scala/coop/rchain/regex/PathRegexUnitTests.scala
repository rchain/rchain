package coop.rchain.regex
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.language.implicitConversions

class PathRegexUnitTests extends AnyFlatSpec with Matchers {

  class ExPathRegex(value: PathRegex) {
    def accept(matchCases: (String, Seq[String])*): ExPathRegex = {
      value.regex match {
        case Right(regex) =>
          for ((sample, sampleGroups) <- matchCases) {
            val matches = regex.findAllMatchIn(sample).toList
            //we expect only one match in source path, otherwise something is wrong
            assert(matches.length <= 1)
            if (matches.nonEmpty) {
              val foundMatch   = matches.head
              val plainMatched = foundMatch.matched :: foundMatch.subgroups

              assert(
                plainMatched == sampleGroups,
                s"::: $sample => $sampleGroups != ${foundMatch.subgroups}"
              )
            } else {
              assert(Nil == sampleGroups, s"::: $sample => $sampleGroups != Nil")
            }
          }
        case Left(err) =>
          if (matchCases.toSeq.nonEmpty) {
            fail(err)
          }
      }
      this
    }

    def build(samples: (Map[String, Iterable[String]], Either[Throwable, String])*): ExPathRegex = {
      for ((args, expectedResult) <- samples) {
        (value.toPath(args), expectedResult) match {
          case (Right(path), Right(expectedPath)) =>
            assert(expectedPath.contains(path))
          case (Right(path), Left(_)) =>
            fail(s"Expected error instead of path: '$path'")
          case (Left(err), Left(expectedErr)) =>
            assert(err.getClass == expectedErr.getClass)
          case (Left(err), Right(expectedPath)) =>
            fail(s"Expected Path: '$expectedPath' instead of error: '$err'")
        }
      }
      this
    }

    def struct(testTokens: List[PathToken]): ExPathRegex = {
      assert(testTokens == value.tokens)
      this
    }
  }

  def parse(path: String, options: PathRegexOptions = PathRegexOptions.default): ExPathRegex =
    new ExPathRegex(PathRegex(path, options))

  "Empty paths" should "parse" in {
    parse("/")
      .struct(List(PathToken("/")))
      .accept(("/", List("/")), ("/route", Nil))
      .build((Map(), Right("/")), (Map("id" -> List("123")), Right("/")))
  }

  "Simple paths" should "parse" in {
    parse("/test")
      .struct(List(PathToken("/test")))
      .accept(
        ("/test", List("/test")),
        ("/route", Nil),
        ("/test/route", Nil),
        ("/test/", List("/test/"))
      )
      .build((Map(), Right("/test")), (Map("id" -> List("123")), Right("/test")))

    parse("/test/")
      .struct(List(PathToken("/test/")))
      .accept(("/test", Nil), ("/test/", List("/test/")), ("/test//", List("/test//")))
      .build((Map(), Right("/test/")))
  }

  "Simple path" should "support escaped groups" in {
    parse("/te\\~st", PathRegexOptions.caseSensitive)
      .struct(List(PathToken("/te~st")))
      .accept(("/te~st", List("/te~st")), ("/TE~ST", Nil))
      .build((Map(), Right("/te~st")))
  }

  "Case-sensitive" should "work" in {
    parse("/test", PathRegexOptions.caseSensitive)
      .struct(List(PathToken("/test")))
      .accept(("/test", List("/test")), ("/TEST", Nil))
      .build((Map(), Right("/test")))

    parse("/TEST", PathRegexOptions.caseSensitive)
      .struct(List(PathToken("/TEST")))
      .accept(("/test", Nil), ("/TEST", List("/TEST")))
      .build((Map(), Right("/TEST")))
  }

  "Strict mode" should "work" in {
    parse("/test", PathRegexOptions.strict)
      .struct(List(PathToken("/test")))
      .accept(("/test", List("/test")), ("/test/", Nil), ("/TEST", List("/TEST")))
      .build((Map(), Right("/test")))

    parse("/test/", PathRegexOptions.strict)
      .struct(List(PathToken("/test/")))
      .accept(("/test", Nil), ("/test/", List("/test/")), ("/test//", Nil))
      .build((Map(), Right("/test/")))
  }

  "Non-ending mode" should "work" in {
    parse("/test", PathRegexOptions.nonEnd)
      .struct(List(PathToken("/test")))
      .accept(
        ("/test/route", List("/test")),
        ("/test", List("/test")),
        ("/test/", List("/test/")),
        ("/route", Nil)
      )
      .build((Map(), Right("/test")))

    parse("/test/", PathRegexOptions.nonEnd)
      .struct(List(PathToken("/test/")))
      .accept(
        ("/test/route", List("/test/")),
        ("/test//route", List("/test/")),
        ("/test", Nil),
        ("/test//", List("/test//"))
      )
      .build((Map(), Right("/test/")))

    parse("/:test", PathRegexOptions.nonEnd)
      .struct(
        List(PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?"""))
      )
      .accept(("/route", List("/route", "route")))
      .build(
        (Map(), Left(new IllegalArgumentException)),
        (Map("test" -> List("a+b")), Right("/a%2Bb")),
        (Map("test" -> List("abc")), Right("/abc"))
      )

    parse("/:test/", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?"""),
          PathToken("/")
        )
      )
      .accept(("/route", Nil), ("/route/", List("/route/", "route")))
      .build(
        (Map(), Left(new IllegalArgumentException)),
        (Map("test" -> List("abc")), Right("/abc/"))
      )
  }

  val noEndStrictOptions = PathRegexOptions(end = false, strict = true)

  "Combine modes" should "accept simple paths" in {
    parse("/test", noEndStrictOptions)
      .struct(List(PathToken("/test")))
      .accept(("/test", List("/test")), ("/test/", List("/test")), ("/test/route", List("/test")))
      .build((Map(), Right("/test")))

    parse("/test/", noEndStrictOptions)
      .struct(List(PathToken("/test/")))
      .accept(
        ("/test", Nil),
        ("/test/", List("/test/")),
        ("/test//", List("/test/")),
        ("/test/route", List("/test/"))
      )
      .build((Map(), Right("/test/")))
  }

  "Combine modes" should "accept file path" in {
    parse("/test.json", noEndStrictOptions)
      .struct(List(PathToken("/test.json")))
      .accept(
        ("/test.json", List("/test.json")),
        ("/test.json.hbs", Nil),
        ("/test.json/route", List("/test.json"))
      )
      .build((Map(), Right("/test.json")))
  }

  "Combine modes" should "work with named argument" in {
    parse("/:test", noEndStrictOptions)
      .struct(
        List(PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?"""))
      )
      .accept(("/route", List("/route", "route")), ("/route/", List("/route", "route")))
      .build(
        (Map(), Left(new IllegalArgumentException)),
        (Map("test" -> List("abc")), Right("/abc"))
      )

    parse("/:test/", noEndStrictOptions)
      .struct(
        List(
          PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?"""),
          PathToken("/")
        )
      )
      .accept(("/route", Nil), ("/route/", List("/route/", "route")))
      .build(
        (Map(), Left(new IllegalArgumentException)),
        (Map("test" -> List("foobar")), Right("/foobar/"))
      )
  }

  "Single named parameter" should "pass test set" in {
    parse("/:test")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(
        ("/route", List("/route", "route")),
        ("/another", List("/another", "another")),
        ("/something/else", Nil),
        ("/route.json", List("/route.json", "route.json")),
        ("/something%2Felse", List("/something%2Felse", "something%2Felse")),
        ("/something%2Felse%2Fmore", List("/something%2Felse%2Fmore", "something%2Felse%2Fmore")),
        ("/;,:@&=+$-_.!~*()", List("/;,:@&=+$-_.!~*()", ";,:@&=+$-_.!~*()"))
      )
      .build(
        (Map("test" -> List("route")), Right("/route")),
        (Map("test" -> List("something/else")), Right("/something%2Felse")),
        (Map("test" -> List("something/else/more")), Right("/something%2Felse%2Fmore"))
      )
  }

  "Named parameter" should "support strict mode" in {
    parse("/:test", PathRegexOptions.strict)
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(("/route", List("/route", "route")), ("/route/", Nil))
      .build((Map("test" -> List("route")), Right("/route")))

    parse("/:test/", PathRegexOptions.strict)
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          ),
          PathToken("/")
        )
      )
      .accept(("/route/", List("/route/", "route")), ("/route//", Nil))
      .build((Map("test" -> List("route")), Right("/route/")))
  }

  "Named parameter" should "support non-ending mode" in {
    parse("/:test", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(
        ("/route.json", List("/route.json", "route.json")),
        ("/route//", List("/route", "route"))
      )
      .build((Map("test" -> List("route")), Right("/route")))
  }

  "Optional named parameter" should "work" in {
    parse("/:test?")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(
        ("/route", List("/route", "route")),
        ("/route/nested", Nil),
        ("/", List("/", null)),
        ("//", Nil)
      )
  }

  "Optional named parameter" should "work with strict mode" in {
    parse("/:test?", PathRegexOptions.strict)
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(("/route", List("/route", "route")), ("/", Nil), ("//", Nil))
      .build((Map(), Right("")), (Map("test" -> List("foobar")), Right("/foobar")))

    parse("/:test?/", PathRegexOptions.strict)
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          ),
          PathToken("/")
        )
      )
      .accept(
        ("/route", Nil),
        ("/route/", List("/route/", "route")),
        ("/", List("/", null)),
        ("//", Nil)
      )
      .build((Map(), Right("/")), (Map("test" -> List("foobar")), Right("/foobar/")))
  }

  "Optional named parameter" should "work well in the middle of path" in {
    parse("/:test?/bar")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          ),
          PathToken("/bar")
        )
      )
      .accept(("/foo/bar", List("/foo/bar", "foo")))
      .build((Map("test" -> List("foo")), Right("/foo/bar")))

    parse("/:test?-bar")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = false,
            partial = true,
            """[^\/]+?"""
          ),
          PathToken("-bar")
        )
      )
      .accept(("/-bar", List("/-bar", null)), ("/foo-bar", List("/foo-bar", "foo")))
      .build((Map("test" -> List("aaa")), Right("/aaa-bar")))

    parse("/:test*-bar")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = true,
            partial = true,
            """[^\/]+?"""
          ),
          PathToken("-bar")
        )
      )
      .accept(
        ("/-bar", List("/-bar", null)),
        ("/foo-bar", List("/foo-bar", "foo")),
        ("/foo/baz-bar", List("/foo/baz-bar", "foo/baz"))
      )
      .build(
        (Map("test" -> List("aaa")), Right("/aaa-bar")),
        (Map("test" -> List("aaa", "bbb")), Right("/aaa/bbb-bar"))
      )
  }

  "Repeated one or more times parameters." should "work" in {
    parse("/:test+")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = true,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(
        ("/", Nil),
        ("//", Nil),
        ("/route", List("/route", "route")),
        ("/some/basic/route", List("/some/basic/route", "some/basic/route"))
      )
      .build(
        (Map(), Left(new IllegalArgumentException)),
        (Map("test" -> List("foobar")), Right("/foobar")),
        (Map("test" -> List("a", "b", "c")), Right("/a/b/c"))
      )
  }

  "Repeated parameter" should "support inline regex" in {
    parse("/:test(\\d+)+")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = true,
            partial = false,
            """\d+"""
          )
        )
      )
      .accept(("/abc/456/789", Nil), ("/123/456/789", List("/123/456/789", "123/456/789")))
      .build(
        (Map("test" -> List("abc")), Left(new IllegalArgumentException)),
        (Map("test" -> List("123")), Right("/123")),
        (Map("test" -> List("1", "2", "3")), Right("/1/2/3"))
      )

    parse("/route.:ext(json|xml)+")
      .struct(
        List(
          PathToken("/route"),
          PathToken(
            Some("ext"),
            0,
            Some('.'),
            Some('.'),
            optional = false,
            repeat = true,
            partial = false,
            """json|xml"""
          )
        )
      )
      .accept(
        ("/route", Nil),
        ("/route.json", List("/route.json", "json")),
        ("/route.xml.json", List("/route.xml.json", "xml.json")),
        ("/route.html", Nil)
      )
      .build(
        (Map("ext" -> List("foobar")), Left(new IllegalArgumentException)),
        (Map("ext" -> List("xml")), Right("/route.xml")),
        (Map("ext" -> List("xml", "json")), Right("/route.xml.json"))
      )
  }

  "Repeated zero or more times parameters" should "be supported" in {
    parse("/:test*")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = true,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(
        ("/", List("/", null)),
        ("//", Nil),
        ("/route", List("/route", "route")),
        ("/some/basic/route", List("/some/basic/route", "some/basic/route"))
      )
      .build(
        (Map(), Right("")),
        (Map("test" -> List("foobar")), Right("/foobar")),
        (Map("test" -> List("foo", "bar")), Right("/foo/bar"))
      )

    parse("/route.:ext([a-z]+)*")
      .struct(
        List(
          PathToken("/route"),
          PathToken(
            Some("ext"),
            0,
            Some('.'),
            Some('.'),
            optional = true,
            repeat = true,
            partial = false,
            """[a-z]+"""
          )
        )
      )
      .accept(
        ("/route", List("/route", null)),
        ("/route.json", List("/route.json", "json")),
        ("/route.json.xml", List("/route.json.xml", "json.xml")),
        ("/route.123", Nil)
      )
      .build(
        (Map(), Right("/route")),
        (Map("ext" -> Nil), Right("/route")),
        (Map("ext" -> List("123")), Left(new IllegalArgumentException)),
        (Map("ext" -> List("abc", "123")), Left(new IllegalArgumentException)),
        (Map("ext" -> List("foobar")), Right("/route.foobar")),
        (Map("ext" -> List("foo", "bar")), Right("/route.foo.bar"))
      )
  }

  "Custom named parameters (no repeat)" should "work" in {
    parse("/:test(\\d+)")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """\d+"""
          )
        )
      )
      .accept(("/abc", Nil), ("/123", List("/123", "123")))
      .build(
        (Map("test" -> List("abc")), Left(new IllegalArgumentException)),
        (Map("test" -> List("123")), Right("/123"))
      )

    parse("/:test(\\d+)", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """\d+"""
          )
        )
      )
      .accept(("/abc", Nil), ("/123", List("/123", "123")), ("/123/abc", List("/123", "123")))
      .build(
        (Map("test" -> List("abc")), Left(new IllegalArgumentException)),
        (Map("test" -> List("123")), Right("/123"))
      )
  }

  "Custom named parameter" should "support wildcard" in {
    parse("/:test(.*)")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """.*"""
          )
        )
      )
      .accept(
        ("/anything/goes/here", List("/anything/goes/here", "anything/goes/here")),
        ("/;,:@&=/+$-_.!/~*()", List("/;,:@&=/+$-_.!/~*()", ";,:@&=/+$-_.!/~*()"))
      )
      .build(
        (Map("test" -> List("")), Right("/")),
        (Map("test" -> List("abc")), Right("/abc")),
        (Map("test" -> List("abc/123")), Right("/abc%2F123")),
        (Map("test" -> List("abc/123/456")), Right("/abc%2F123%2F456"))
      )
  }

  "Custom named parameter" should "support charsets" in {
    parse("/:route([a-z]+)")
      .struct(
        List(
          PathToken(
            Some("route"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """[a-z]+"""
          )
        )
      )
      .accept(("/abcde", List("/abcde", "abcde")), ("/12345", Nil))
      .build(
        (Map("route" -> List("")), Left(new IllegalArgumentException)),
        (Map("route" -> List("123")), Left(new IllegalArgumentException)),
        (Map("route" -> List("abc")), Right("/abc"))
      )
  }

  "Custom named parameter" should "support alternation" in {
    parse("/:route(this|that)")
      .struct(
        List(
          PathToken(
            Some("route"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """this|that"""
          )
        )
      )
      .accept(("/this", List("/this", "this")), ("/that", List("/that", "that")), ("/foo", Nil))
      .build(
        (Map("route" -> List("this")), Right("/this")),
        (Map("route" -> List("that")), Right("/that")),
        (Map("route" -> List("abc")), Left(new IllegalArgumentException))
      )

    parse("/:path(abc|xyz)*")
      .struct(
        List(
          PathToken(
            Some("path"),
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = true,
            partial = false,
            """abc|xyz"""
          )
        )
      )
      .accept(
        ("/abc", List("/abc", "abc")),
        ("/abc/abc", List("/abc/abc", "abc/abc")),
        ("/xyz/xyz", List("/xyz/xyz", "xyz/xyz")),
        ("/abc/xyz/abc/xyz", List("/abc/xyz/abc/xyz", "abc/xyz/abc/xyz")),
        ("/xyzxyz", Nil)
      )
      .build(
        (Map("path" -> List("abc")), Right("/abc")),
        (Map("path" -> List("abc", "xyz")), Right("/abc/xyz")),
        (Map("path" -> List("xyz", "abc", "xyz")), Right("/xyz/abc/xyz")),
        (Map("path" -> List("abc123")), Left(new IllegalArgumentException)),
        (Map("path" -> List("abcxyz")), Left(new IllegalArgumentException))
      )
  }

  "Prefixed slashes" should "be supported" in {
    parse("test")
      .struct(List(PathToken("test")))
      .accept(("/test", Nil), ("test", List("test")))
      .build((Map(), Right("test")))

    parse(":test")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            None,
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(
        ("/route", Nil),
        ("route", List("route", "route")),
        ("/route", Nil),
        ("route/", List("route/", "route"))
      )
      .build(
        (Map("test" -> List("")), Left(new IllegalArgumentException)),
        (Map(), Left(new IllegalArgumentException)),
        (Map("test" -> Nil), Left(new IllegalArgumentException)),
        (Map("test" -> List("route")), Right("route"))
      )

    parse(":test", PathRegexOptions.strict)
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            None,
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(("/route", Nil), ("route", List("route", "route")), ("route/", Nil))
      .build((Map("test" -> List("route")), Right("route")))

    parse(":test", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            None,
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(
        ("/route", Nil),
        ("route", List("route", "route")),
        ("route/", List("route/", "route")),
        ("route/foobar", List("route", "route"))
      )
      .build((Map("test" -> List("route")), Right("route")))

    parse(":test?")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            None,
            Some('/'),
            optional = true,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(
        ("/route", Nil),
        ("route", List("route", "route")),
        ("", List("", null)),
        ("route/foobar", Nil)
      )
      .build(
        (Map(), Right("")),
        (Map("test" -> List("")), Left(new IllegalArgumentException())),
        (Map("test" -> List("route")), Right("route"))
      )
  }

  "Formats" should "work" in {
    parse("/test.json")
      .struct(List(PathToken("/test.json")))
      .accept(("/route.json", Nil), ("/test.json", List("/test.json")))
      .build((Map(), Right("/test.json")))

    parse("/:test.json")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = true,
            """[^\/]+?"""
          ),
          PathToken(".json")
        )
      )
      .accept(
        ("/.json", Nil),
        ("/test.json", List("/test.json", "test")),
        ("/route.json", List("/route.json", "route")),
        ("/route.json.json", List("/route.json.json", "route.json"))
      )
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("test" -> List("")), Left(new IllegalArgumentException())),
        (Map("test" -> List("foo")), Right("/foo.json"))
      )
  }

  "Format params" should "work" in {
    parse("/test.:format")
      .struct(
        List(
          PathToken("/test"),
          PathToken(
            Some("format"),
            0,
            Some('.'),
            Some('.'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\.]+?"""
          )
        )
      )
      .accept(("/test.html", List("/test.html", "html")), ("/test.txt.html", Nil))
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("format" -> List("")), Left(new IllegalArgumentException())),
        (Map("format" -> List("foo")), Right("/test.foo"))
      )

    parse("/test.:format.:format")
      .struct(
        List(
          PathToken("/test"),
          PathToken(
            Some("format"),
            0,
            Some('.'),
            Some('.'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\.]+?"""
          ),
          PathToken(
            Some("format"),
            1,
            Some('.'),
            Some('.'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\.]+?"""
          )
        )
      )
      .accept(("/test.html", Nil), ("/test.txt.html", List("/test.txt.html", "txt", "html")))
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("format" -> List("foo.bar")), Left(new IllegalArgumentException())),
        (Map("format" -> List("foo")), Right("/test.foo.foo"))
      )
  }

  "Format params" should "support multipliers" in {
    parse("/test.:format+")
      .struct(
        List(
          PathToken("/test"),
          PathToken(
            Some("format"),
            0,
            Some('.'),
            Some('.'),
            optional = false,
            repeat = true,
            partial = false,
            """[^\.]+?"""
          )
        )
      )
      .accept(
        ("/test.html", List("/test.html", "html")),
        ("/test.txt.html", List("/test.txt.html", "txt.html"))
      )
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("format" -> List("foo")), Right("/test.foo")),
        (Map("format" -> List("foo", "bar")), Right("/test.foo.bar"))
      )
  }

  "Format params" should "support nonEnd mode" in {
    parse("/test.:format", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken("/test"),
          PathToken(
            Some("format"),
            0,
            Some('.'),
            Some('.'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\.]+?"""
          )
        )
      )
      .accept(("/test.html", List("/test.html", "html")), ("/test.txt.html", Nil))
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("format" -> List("")), Left(new IllegalArgumentException())),
        (Map("format" -> List("foo")), Right("/test.foo"))
      )
  }

  "Format params" should "support partial paths" in {
    parse("/test.:format.")
      .struct(
        List(
          PathToken("/test"),
          PathToken(
            Some("format"),
            0,
            Some('.'),
            Some('.'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\.]+?"""
          ),
          PathToken(".")
        )
      )
      .accept(("/test.html.", List("/test.html.", "html")), ("/test.txt.html", Nil))
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("format" -> List("")), Left(new IllegalArgumentException())),
        (Map("format" -> List("foo")), Right("/test.foo."))
      )
  }

  "Format and path params" should "work" in {
    parse("/:test.:format")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = true,
            """[^\/]+?"""
          ),
          PathToken(
            Some("format"),
            1,
            Some('.'),
            Some('.'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\.]+?"""
          )
        )
      )
      .accept(
        ("/route.html", List("/route.html", "route", "html")),
        ("/route", Nil),
        ("/route.html.json", List("/route.html.json", "route.html", "json"))
      )
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("format" -> List("foo"), "test" -> List("route")), Right("/route.foo"))
      )
  }

  "Format and path params" should "support optional" in {
    parse("/:test.:format?")
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = true,
            """[^\/]+?"""
          ),
          PathToken(
            Some("format"),
            1,
            Some('.'),
            Some('.'),
            optional = true,
            repeat = false,
            partial = false,
            """[^\.]+?"""
          )
        )
      )
      .accept(
        ("/route", List("/route", "route", null)),
        ("/route.html", List("/route.html", "route", "html")),
        ("/route.html.json", List("/route.html.json", "route.html", "json"))
      )
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("test"   -> List("route")), Right("/route")),
        (Map("test"   -> List("route"), "format" -> List("")), Left(new IllegalArgumentException())),
        (Map("format" -> List("foo"), "test" -> List("route")), Right("/route.foo"))
      )

    //and non-end mode
    parse("/:test.:format?", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken(
            Some("test"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = true,
            """[^\/]+?"""
          ),
          PathToken(
            Some("format"),
            1,
            Some('.'),
            Some('.'),
            optional = true,
            repeat = false,
            partial = false,
            """[^\.]+?"""
          )
        )
      )
      .accept(
        ("/route", List("/route", "route", null)),
        ("/route.html", List("/route.html", "route", "html")),
        ("/route.html.json", List("/route.html.json", "route.html", "json"))
      )
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("test"   -> List("route")), Right("/route")),
        (Map("test"   -> List("route"), "format" -> Nil), Right("/route")),
        (Map("test"   -> List("route"), "format" -> List("")), Left(new IllegalArgumentException())),
        (Map("format" -> List("foo"), "test" -> List("route")), Right("/route.foo"))
      )
  }

  "Format and path params" should "support custom regex-es" in {
    parse("/test.:format(.*)z", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken("/test"),
          PathToken(
            Some("format"),
            0,
            Some('.'),
            Some('.'),
            optional = false,
            repeat = false,
            partial = true,
            """.*"""
          ),
          PathToken("z")
        )
      )
      .accept(
        ("/test.abc", Nil),
        ("/test.z", List("/test.z", "")),
        ("/test.abcz", List("/test.abcz", "abc"))
      )
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("format" -> List("")), Right("/test.z")),
        (Map("format" -> List("foo")), Right("/test.fooz"))
      )
  }

  "Unnamed params" should "work" in {
    parse("/(\\d+)")
      .struct(
        List(
          PathToken(
            None,
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """\d+"""
          )
        )
      )
      .accept(("/123", List("/123", "123")), ("/abc", Nil), ("/123/abc", Nil))
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("0" -> List("123")), Right("/123"))
      )

    parse("/(\\d+)", PathRegexOptions.nonEnd)
      .struct(
        List(
          PathToken(
            None,
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """\d+"""
          )
        )
      )
      .accept(
        ("/123", List("/123", "123")),
        ("/abc", Nil),
        ("/123/abc", List("/123", "123")),
        ("/123/", List("/123/", "123"))
      )
      .build(
        (Map(), Left(new IllegalArgumentException())),
        (Map("0" -> List("123")), Right("/123"))
      )
  }

  "Optional unnamed parameter" should "work" in {
    parse("/(\\d+)?")
      .struct(
        List(
          PathToken(
            None,
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = false,
            partial = false,
            """\d+"""
          )
        )
      )
      .accept(("/", List("/", null)), ("/123", List("/123", "123")))
      .build((Map(), Right("")), (Map("0" -> List("123")), Right("/123")))
  }

  "Optional unnamed wildcard" should "work" in {
    parse("/(.*)")
      .struct(
        List(
          PathToken(
            None,
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """.*"""
          )
        )
      )
      .accept(
        ("/", List("/", "")),
        ("/123", List("/123", "123")),
        ("/route/nested", List("/route/nested", "route/nested"))
      )
      .build((Map("0" -> List("")), Right("/")), (Map("0" -> List("123")), Right("/123")))
  }

  "Escape sequences" should "work" in {
    parse("""/route\(\\(\d+\\)\)""")
      .struct(
        List(
          PathToken("/route(\\"),
          PathToken(
            None,
            0,
            None,
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """\d+\\"""
          ),
          PathToken(")")
        )
      )
      .accept(("/route(\\123\\)", List("/route(\\123\\)", "123\\")))
  }

  "Escaped characters" should "be supported" in {
    parse("/\\(testing\\)")
      .struct(List(PathToken("/(testing)")))
      .accept(("/testing", Nil), ("/(testing)", List("/(testing)")))
      .build((Map(), Right("/(testing)")))

    parse("/.+*?=^!:${}[]|")
      .struct(List(PathToken("/.+*?=^!:${}[]|")))
      .accept(("/.+*?=^!:${}[]|", List("/.+*?=^!:${}[]|")))
      .build((Map(), Right("/.+*?=^!:${}[]|")))
  }

  "Two optional parameters" should "be supported" in {
    parse("/test\\/:uid(u\\d+)?:cid(c\\d+)?")
      .struct(
        List(
          PathToken("/test/"),
          PathToken(
            Some("uid"),
            0,
            None,
            Some('/'),
            optional = true,
            repeat = false,
            partial = false,
            """u\d+"""
          ),
          PathToken(
            Some("cid"),
            1,
            None,
            Some('/'),
            optional = true,
            repeat = false,
            partial = false,
            """c\d+"""
          )
        )
      )
      .accept(
        ("/test", Nil),
        ("/test/", List("/test/", null, null)),
        ("/test/u123", List("/test/u123", "u123", null)),
        ("/test/c123", List("/test/c123", null, "c123"))
      )
      .build(
        (Map("uid" -> List("u123")), Right("/test/u123")),
        (Map("cid" -> List("c123")), Right("/test/c123")),
        (Map("cid" -> List("u123")), Left(new IllegalArgumentException()))
      )
  }

  "Unnamed group prefix" should "work" in {
    parse("/(apple-)?icon-:res(\\d+).png")
      .struct(
        List(
          PathToken(
            None,
            0,
            Some('/'),
            Some('/'),
            optional = true,
            repeat = false,
            partial = true,
            """apple-"""
          ),
          PathToken("icon-"),
          PathToken(
            Some("res"),
            1,
            None,
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """\d+"""
          ),
          PathToken(".png")
        )
      )
      .accept(
        ("/icon-240.png", List("/icon-240.png", null, "240")),
        ("/apple-icon-240.png", List("/apple-icon-240.png", "apple-", "240"))
      )
  }

  "Unicode characters" should "work" in {
    parse("/:foo")
      .struct(
        List(
          PathToken(
            Some("foo"),
            0,
            Some('/'),
            Some('/'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\/]+?"""
          )
        )
      )
      .accept(("/café", List("/café", "café")))
      .build((Map("foo" -> List("café")), Right("/caf%C3%A9")))

    parse("/café")
      .struct(List(PathToken("/café")))
      .accept(("/café", List("/café")))
      .build((Map(), Right("/café")))
  }

  "Ends with" should "work" in {
    parse("/test", PathRegexOptions(endsWith = List("?")))
      .struct(List(PathToken("/test")))
      .accept(
        ("/test", List("/test")),
        ("/test?query=string", List("/test")),
        ("/test/?query=string", List("/test/")),
        ("/testx", Nil)
      )
      .build((Map(), Right("/test")))

    parse("/test", PathRegexOptions(endsWith = List("?"), strict = true))
      .struct(List(PathToken("/test")))
      .accept(("/test?query=string", List("/test")), ("/test/?query=string", Nil))
      .build((Map(), Right("/test")))
  }

  "Custom delimiters" should "work" in {
    parse("$:foo$:bar?", PathRegexOptions(delimiters = Set('$')))
      .struct(
        List(
          PathToken(
            Some("foo"),
            0,
            Some('$'),
            Some('$'),
            optional = false,
            repeat = false,
            partial = false,
            """[^\$]+?"""
          ),
          PathToken(
            Some("bar"),
            1,
            Some('$'),
            Some('$'),
            optional = true,
            repeat = false,
            partial = false,
            """[^\$]+?"""
          )
        )
      )
      .accept(("$x", List("$x", "x", null)), ("$x$y", List("$x$y", "x", "y")))
      .build(
        (Map("foo" -> List("foo")), Right("$foo")),
        (Map("foo" -> List("foo"), "bar" -> List("bar")), Right("$foo$bar"))
      )
  }
}
