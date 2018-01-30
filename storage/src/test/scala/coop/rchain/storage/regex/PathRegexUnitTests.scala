package coop.rchain.storage.regex

import org.scalatest._

class PathRegexUnitTests extends FlatSpec with Matchers {

  def doCheck(path: String,
              tokens: Option[List[PathToken]],
              matchCases: Seq[(String, Seq[String])],
              //compileCases : Seq[String],
              options: PathRegexOptions = PathRegexOptions.default): Unit = {
    val parsed = PathRegex.compile(path, options)
    tokens.foreach(t => assert(parsed.tokens == t))
  }

  def check(path: String,
            tokens: List[Any],
            matchCases: Seq[(String, Seq[String])] = Nil,
            options: PathRegexOptions = PathRegexOptions.default): Unit =
    doCheck(path, Some(tokens.map {
      case t: PathToken => t
      case s: String    => PathToken(s)
    }), matchCases, options)

  "Simple paths" should "parse" in {
    check("/",
          "/" :: Nil,
          ("/", "/" :: Nil)
            :: ("/route", Nil) :: Nil)

    check("/test",
          "/test" :: Nil,
          ("/test", "/test" :: Nil)
            :: ("/route", Nil)
            :: ("/test/route", Nil)
            :: ("/test/", "/test/" :: Nil)
            :: Nil)

    check("/test/",
          "/test/" :: Nil,
          ("/test", Nil)
            :: ("/test/", "/test/" :: Nil)
            :: ("/test//", "/test//" :: Nil)
            :: Nil)
  }

  "Simple path" should "support escaped groups" in {
    check("/te\\~st",
          "/te~st" :: Nil,
          ("/te~st", "/te~st" :: Nil)
            :: ("/TE~ST", Nil)
            :: Nil,
          PathRegexOptions.caseSensitive)
  }

  "Case-sensitive" should "work" in {
    check("/test",
          "/test" :: Nil,
          ("/test", "/test" :: Nil)
            :: ("/TEST", Nil)
            :: Nil,
          PathRegexOptions.caseSensitive)

    check("/TEST",
          "/TEST" :: Nil,
          ("/test", Nil)
            :: ("/TEST", "/TEST" :: Nil)
            :: Nil,
          PathRegexOptions.caseSensitive)
  }

  "Strict mode" should "work" in {
    check("/test",
          "/test" :: Nil,
          ("/test", "/test" :: Nil)
            :: ("/test/", Nil)
            :: ("/TEST", "/TEST" :: Nil)
            :: Nil,
          PathRegexOptions.strict)
    check("/test/",
          "/test/" :: Nil,
          ("/test", Nil)
            :: ("/test/", "/test/" :: Nil)
            :: ("/test//", Nil)
            :: Nil,
          PathRegexOptions.strict)
  }

  "Non-ending mode" should "work" in {
    check("/test",
          "/test" :: Nil,
          ("/test/route", "/test" :: Nil)
            :: ("/test", "/test" :: Nil)
            :: ("/test/", "/test/" :: Nil)
            :: ("/route", Nil)
            :: Nil,
          PathRegexOptions.nonEnd)
    check(
      "/test/",
      "/test/" :: Nil,
      ("/test/route", "/test/" :: Nil)
        :: ("/test//route", "/test/" :: Nil)
        :: ("/test", Nil)
        :: ("/test//", "/test//" :: Nil)
        :: Nil,
      PathRegexOptions.nonEnd
    )
    check(
      "/:test",
      PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: Nil,
      ("/route", "/route" :: "route" :: Nil) :: Nil,
      PathRegexOptions.nonEnd
    )
    check(
      "/:test/",
      PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: PathToken(
        "/") :: Nil,
      ("/route", Nil)
        :: ("/route/", "/route/" :: "route" :: Nil)
        :: Nil,
      PathRegexOptions.nonEnd
    )
  }

  val noEndStrictOptions = PathRegexOptions(end = false, strict = true)

  "Combine modes" should "accept simple paths" in {
    check("/test",
          "/test" :: Nil,
          ("/test", "/test" :: Nil)
            :: ("/test/", "/test" :: Nil)
            :: ("/test/route", "/test" :: Nil)
            :: Nil,
          noEndStrictOptions)
    check(
      "/test/",
      "/test/" :: Nil,
      ("/test", Nil)
        :: ("/test/", "/test/" :: Nil)
        :: ("/test//", "/test/" :: Nil)
        :: ("/test/route", "/test/" :: Nil)
        :: Nil,
      noEndStrictOptions
    )
  }

  "Combine modes" should "accept file path" in {
    check(
      "/test.json",
      "/test.json" :: Nil,
      ("/test.json", "/test.json" :: Nil)
        :: ("/test.json.hbs", Nil)
        :: ("/test.json/route", "/test.json" :: Nil)
        :: Nil,
      noEndStrictOptions
    )
  }

  "Combine modes" should "work with named argument" in {
    check(
      "/:test",
      PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: Nil,
      ("/route", "/route" :: "route" :: Nil)
        :: ("/route/", "/route" :: "route" :: Nil)
        :: Nil,
      noEndStrictOptions
    )
    check(
      "/:test/",
      PathToken(Some("test"), 0, Some('/'), Some('/'), false, false, false, """[^\/]+?""") :: PathToken(
        "/") :: Nil,
      ("/route", Nil)
        :: ("/route/", "/route/" :: "route" :: Nil)
        :: Nil,
      noEndStrictOptions
    )
  }

  "Optional named parameter" should "work" in {
    check(
      "/:test?",
      PathToken(Some("test"),
                0,
                Some('/'),
                Some('/'),
                optional = true,
                repeat = false,
                partial = false,
                """[^\/]+?""") :: Nil,
      ("/route", "/route" :: "route" :: Nil)
        :: ("/route/nested", Nil)
        :: ("/", "/" :: null :: Nil)
        :: ("//", Nil)
        :: Nil,
      PathRegexOptions.default
    )
  }

  "Optional named parameter" should "work with strict mode" in {
    check(
      "/:test?",
      PathToken(Some("test"),
                0,
                Some('/'),
                Some('/'),
                optional = true,
                repeat = false,
                partial = false,
                """[^\/]+?""") :: Nil,
      ("/route", "/route" :: "route" :: Nil)
        :: ("/", Nil)
        :: ("//", Nil)
        :: Nil,
      PathRegexOptions.strict
    )

    check(
      "/:test?/",
      PathToken(Some("test"),
                0,
                Some('/'),
                Some('/'),
                optional = true,
                repeat = false,
                partial = false,
                """[^\/]+?""") :: "/" :: Nil,
      ("/route", Nil)
        :: ("/route/", "/route/" :: "route" :: Nil)
        :: ("/", "/" :: null :: Nil)
        :: ("//", Nil)
        :: Nil,
      PathRegexOptions.strict
    )
  }
}
