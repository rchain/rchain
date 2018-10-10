package coop.rchain.regex

import scala.annotation.tailrec
import scala.util.Try

trait ParsedPattern {

  /**
    * Tries to convert a given Regex from string to a RegexPattern object
    * Returns parsed (Alt|Mult|Conc|CharClass)Pattern and position where
    * parsing finished
    */
  def tryParse(str: CharSequence): Option[(RegexPattern, Int)]

  /**
    * Converts entire given string to a RegexPattern object
    * Returns None if part of the string is a valid regex
    */
  def parse(str: CharSequence): Option[RegexPattern] =
    tryParse(str).flatMap {
      case (pattern, len) =>
        if (len == str.length)
          Some(pattern)
        else
          None
    }
}

/**
  * A companion object for the RegexPattern class
  */
object RegexPattern extends ParsedPattern {

  /**
    * Turn the supplied finite state machine into a `RegexPattern` object.
    * This is accomplished using the Brzozowski algebraic method.
    */
  def fromFsm(fsm: Fsm): RegexPattern =
    throw new NotImplementedError("TODO")

  /**
    * Tries to convert a given Regex from string to a RegexPattern object
    * Returns parsed (Alt|Mult|Conc|CharClass)Pattern and position where
    * parsing finished
    */
  def tryParse(str: CharSequence): Option[(RegexPattern, Int)] = AltPattern.tryParse(str)

  /**
    * This RegexPattern expresses "no possibilities at all"
    * and can never match anything.
    */
  val nothing: RegexPattern = CharClassPattern(Nil)
}

/**
  * Parent abstract class for all Regex patterns.
  * All patterns have some things in common. This parent class mainly
  * hosts documentation though.
  */
sealed abstract class RegexPattern {

  /**
    * Two RegexPatterns are equivalent if they recognise the same strings. Note
    *	that in the general case this is actually quite an intensive calculation,
    *	but far from unsolvable, as we demonstrate here:
    */
  def equivalent(that: RegexPattern): Boolean

  /**
    * Concatenate a sequence of Regex patterns, regardless of differing classes.
    * Call using "a = b + c"
    */
  def concatenate(that: RegexPattern): ConcPattern

  /**
    * Alternate between any two Regex patterns, regardless of differing classes.
    */
  def union(that: RegexPattern): RegexPattern

  /**
    * Takes the current RegexPattern and simplifies it in every way possible,
    * returning a simpler RegexPattern which is quite probably not of the same class
    * as the original. Approaches are different for every descendant.
    */
  def reduced: RegexPattern

  /**
    * Returns
    */
  def negated: RegexPattern

  /**
    * Return a set of all unique characters used in this RegexPattern.
    * By convention, fsm.anythingElse is always included in this result.
    */
  lazy val alphabet: Set[Char] = Set(Fsm.anythingElse)

  /**
    * Intersection function. Return a RegexPattern that can match any string
    *	that both self and other can match. Fairly elementary results relating
    *	to regular languages and finite state machines show that this is
    * possible, but implementation is a BEAST in many cases. Here, we convert
    *	both Regex patters to FSMs (see to_fsm(), above) for the intersection, then
    *	back to RegexPatterns afterwards.
    *	Call using "a = b & c"
    */
  def intersection(that: RegexPattern): RegexPattern

  /**
    * Return a RegexPattern which will match any string which, when reversed,
    * self would match. E.g. if self matches "beer" then reversed(self) will
    * match "reeb".
    */
  def reversed: RegexPattern

  /**
    * Equivalent to repeated concatenation. Multiplier consists of a minimum
    * and a maximum; maximum may be infinite (for Kleene star closure).
    * Call using "a = b * qm"
    */
  def multiply(multiplier: Multiplier): MultPattern

  /**
    * Equivalent to repeated concatenation. Multiplier consists of a minimum
    * and a maximum; maximum may be infinite (for Kleene star closure).
    * Call using "a = b * qm"
    */
  final def multiply(multiplier: Int): MultPattern = multiply(Multiplier(multiplier))

  /**
    * Return False if there exists a string which the present RegexPattern
    * can match. Return true if no such string exists. Example of empty
    * patterns: CharClassPattern()
    */
  def isEmpty: Boolean

  /**
    * Test whether the present RegexPattern accepts the supplied string.
    * Used only for unit-testing.
    */
  private[regex] def accepts(s: String): Boolean = toFsm().accepts(s)

  def toFsm(alphabet: Option[Set[Char]] = None): Fsm

  final def toFsm(alphabet: Set[Char]): Fsm = toFsm(Some(alphabet))

  final override def equals(obj: scala.Any): Boolean = obj match {
    case pattern: RegexPattern => equivalent(pattern)
    case _                     => false
  }

  //region Operators

  /**
    * Just calls equivalent method
    */
  final def ==(that: RegexPattern): Boolean = equivalent(that)

  /**
    * Just calls !equivalent method
    */
  final def !=(that: RegexPattern): Boolean = !equivalent(that)

  /**
    * Concatenate a sequence of Regex patterns, regardless of differing classes.
    */
  final def +(that: RegexPattern): ConcPattern = concatenate(that)

  /**
    * Alternate between any two Regex patterns, regardless of differing classes.
    */
  final def |(that: RegexPattern): RegexPattern = union(that)

  /**
    * Intersection between any two Regex patterns.
    */
  final def &(that: RegexPattern): RegexPattern = intersection(that)

  /**
    * Returns repeated concatenation of this RegexPattern
    */
  final def *(multiplier: Int): MultPattern = multiply(multiplier)

  final def *(multiplier: Multiplier): MultPattern = multiply(multiplier)

  //endregion
}

//we need 3 states to handle case [a-b-z], that means Set(a,b,-,z)
private[regex] object RangeState extends Enumeration {
  val firstSymbol, notStarted, inside, justFinished = Value
}

/**
  * Companion object for the CharClassPattern, used only for easy testing
  */
object CharClassPattern extends ParsedPattern {
  def apply(charSet: String): CharClassPattern    = new CharClassPattern(charSet.toSet)
  def apply(charSet: Seq[Char]): CharClassPattern = new CharClassPattern(charSet.toSet)
  def apply(charSet: Set[Char]): CharClassPattern = new CharClassPattern(charSet)
  def apply(charSet: String, negateCharSet: Boolean): CharClassPattern =
    new CharClassPattern(charSet.toSet, negateCharSet)
  def apply(charSet: Seq[Char], negateCharSet: Boolean): CharClassPattern =
    new CharClassPattern(charSet.toSet, negateCharSet)
  def apply(charSet: Set[Char], negateCharSet: Boolean): CharClassPattern =
    new CharClassPattern(charSet, negateCharSet)

  private[regex] final val escapes =
    Map('t' -> '\t', 'r' -> '\r', 'n' -> '\n', 'f' -> '\f', 'v' -> '\u000b')

  private[regex] final val revEscapes = escapes.map(_.swap)

  private[regex] final val allSpecialChars = """\[]|().?*+{}""".toSet

  //region predefined char classes

  private[regex] final val wordCharsSet =
    "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ_abcdefghijklmnopqrstuvwxyz".toSet

  private[regex] final val digitsCharSet = "0123456789".toSet

  private[regex] final val spacesCharSet = "\t\n\u000b\f\r \u00A0".toSet

  private[regex] final val knownClassMap: Map[Char, CharClassPattern] = Map(
    'w' -> CharClassPattern(wordCharsSet),
    'W' -> CharClassPattern(wordCharsSet, negateCharSet = true),
    'd' -> CharClassPattern(digitsCharSet),
    'D' -> CharClassPattern(digitsCharSet, negateCharSet = true),
    's' -> CharClassPattern(spacesCharSet),
    'S' -> CharClassPattern(spacesCharSet, negateCharSet = true)
  )

  //endregion

  def tryParse(str: CharSequence): Option[(RegexPattern, Int)] = {

    def parseHexChar(startIndex: Int, charsCount: Int): Option[(Char, Int)] =
      if (startIndex + charsCount <= str.length) {
        val substr = str.subSequence(startIndex, startIndex + charsCount).toString
        Try(Integer.parseInt(substr, 16)).toOption
          .flatMap(codePoint => Some(codePoint.asInstanceOf[Char], startIndex + charsCount))
      } else {
        None
      }

    def parseEscapedSequence(startIndex: Int): Option[(CharClassPattern, Int)] =
      if (startIndex < str.length) {
        str.charAt(startIndex) match {
          case 'x' => {
            //unicode escape hex ascii sequence
            val hexChar = parseHexChar(startIndex + 1, 2)
            hexChar.flatMap {
              case (char, finalIndex) => Some(CharClassPattern(Set(char)), finalIndex)
            }
          }
          case 'u' => {
            //escape hex unicode sequence
            val hexChar = parseHexChar(startIndex + 1, 4)
            hexChar.flatMap {
              case (char, finalIndex) => Some(CharClassPattern(Set(char)), finalIndex)
            }
          }
          case known if knownClassMap.contains(known) =>
            Some(knownClassMap(known), startIndex + 1)
          case esc if escapes.contains(esc) =>
            Some(CharClassPattern(Set(escapes(esc))), startIndex + 1)
          case other => Some(CharClassPattern(Set(other)), startIndex + 1)
        }
      } else {
        None
      }

    def parseInternalEscapedSequence(
        startIndex: Int
    ): Option[(Either[Char, CharClassPattern], Int)] =
      if (startIndex < str.length) {
        str.charAt(startIndex) match {
          //escape hex ascii sequence
          case 'x' =>
            parseHexChar(startIndex + 1, 2).map {
              case (parsedChar, takenCount) => (Left(parsedChar), takenCount)
            }
          //escaped hex unicode sequence
          case 'u' =>
            parseHexChar(startIndex + 1, 4).map {
              case (parsedChar, takenCount) => (Left(parsedChar), takenCount)
            }
          //common escape sequences
          case esc if escapes.contains(esc) => Some(Left(escapes(esc)), startIndex + 1)
          //well-known classes, like \d
          case known if knownClassMap.contains(known) =>
            Some(Right(knownClassMap(known)), startIndex + 1)
          //any other escaped symbol - just leave untouched
          case other =>
            Some(Left(other), startIndex + 1) //any unsupported escape sequence - just char
        }
      } else {
        None
      }

    case class ParseState(
        collectedChars: List[Char],
        collectedUnionClasses: List[CharClassPattern],
        rangeState: RangeState.Value
    ) {
      def changeState(nextState: RangeState.Value) =
        ParseState(collectedChars, collectedUnionClasses, nextState)

      //def add(value: Either[Char, CharClassPattern]) : ParseState = addOverrideState(value, rangeState)

      def add(
          value: Either[Char, CharClassPattern],
          overrideRangeState: Option[RangeState.Value] = None
      ): ParseState = {
        val actualRangeState = overrideRangeState.getOrElse(rangeState)

        value match {
          case Left(addChar) => {
            if (actualRangeState == RangeState.inside) {
              //we're in range, like a-z, add range and switch to justFinished state
              ParseState(
                (collectedChars.head to addChar).toList ++ collectedChars,
                collectedUnionClasses,
                RangeState.justFinished
              )
            } else {
              //states justFinished or notStarted - just add single char
              ParseState(addChar :: collectedChars, collectedUnionClasses, RangeState.notStarted)
            }
          }
          case Right(addCharClass) => {
            if (actualRangeState == RangeState.inside) {
              //this is 'bad' case, like [0-\w], we can fail like most regex engines do,
              //but let's better handle it like javascript regex engine
              ParseState(
                '-' :: collectedChars,
                addCharClass :: collectedUnionClasses,
                RangeState.justFinished
              )
            } else {
              ParseState(
                collectedChars,
                addCharClass :: collectedUnionClasses,
                RangeState.justFinished
              )
            }
          }
        }
      }
    }

    //this function parses interior of a char set [abc-xyz], assuming that
    //entry '[^' is handled outside, and finished parsing if ']' found
    //in case of any error, including absent of ']' returns None
    def parseCharSetSequence(
        startIndex: Int,
        negateCharSet: Boolean
    ): Option[(RegexPattern, Int)] = {

      @tailrec
      def processNextChar(currentIndex: Int, parseState: ParseState): Option[(ParseState, Int)] =
        if (currentIndex < str.length) {
          str.charAt(currentIndex) match {
            case '\\' => {
              //'\?' escape sequence
              val charToAdd = parseInternalEscapedSequence(currentIndex + 1)
              if (charToAdd.isDefined) {
                //@tailrec restricts flatMap usage here
                val (nextChar, nextPos) = charToAdd.get
                processNextChar(nextPos, parseState.add(nextChar))
              } else {
                //we got an error during parseInternalEscapedSequence,
                //for example invalid hex char, or end of string
                None
              }
            }
            case ']' if parseState.rangeState != RangeState.firstSymbol => {
              //if ] is a first symbol in a sequence like []] - it will be handled by 'case anyOtherChar' below
              if (parseState.rangeState == RangeState.inside) {
                // [a-] is a valid character class
                Some(parseState.add(Left('-'), Some(RangeState.notStarted)), currentIndex + 1)
              } else {
                //we're done
                Some(parseState, currentIndex + 1)
              }
            }
            case '-' => {
              parseState.rangeState match {
                case RangeState.notStarted => //remember that range started, and continue from the next char
                  processNextChar(currentIndex + 1, parseState.changeState(RangeState.inside))
                case RangeState.inside | RangeState.justFinished |
                    RangeState.firstSymbol => //range just finished, or we're in, '-' is the symbol to add
                  processNextChar(currentIndex + 1, parseState.add(Left('-')))
              }
            }
            case anyOtherChar => {
              processNextChar(currentIndex + 1, parseState.add(Left(anyOtherChar)))
            }
          }
        } else {
          None
        }
      // start from rangeJustFinished (means 'range just ended'),
      // consequently cases like [-] will be handled
      processNextChar(startIndex, ParseState(Nil, Nil, RangeState.firstSymbol)).flatMap {
        case (parseState, endIndex) => {
          if (parseState.collectedUnionClasses.isEmpty) {
            Some(CharClassPattern(parseState.collectedChars, negateCharSet), endIndex)
          } else {
            val startCharSet
              : RegexPattern = CharClassPattern(parseState.collectedChars) //no negation!
            val unionCharSet = parseState.collectedUnionClasses.foldLeft(startCharSet)(_.union(_))
            Some(if (negateCharSet) unionCharSet.negated else unionCharSet, endIndex)
          }
        }
      }
    }

    if (str.length > 0) {
      str.charAt(0) match {
        case '.'  => Some(CharClassPattern("", negateCharSet = true), 1)
        case '\\' => parseEscapedSequence(1)
        case '[' => {
          if (str.length > 1) {
            if (str.charAt(1) == '^')
              parseCharSetSequence(2, negateCharSet = true)
            else
              parseCharSetSequence(1, negateCharSet = false)
          } else {
            None
          }
        }
        case c if allSpecialChars.contains(c) => None
        case c                                => Some(CharClassPattern(Set(c)), 1)
      }
    } else {
      None
    }
  }
}

/**
  * A CharClass is basically a Set of symbols. The reason for the
  * CharClass object instead of using Set directly is to allow us to
  * set a "negated" flag. A CharClass with the negation flag set is assumed
  * to contain every symbol that is in the alphabet of all symbols but not
  * explicitly listed inside the Set. e.g. [{@literal ^}a]. This is very handy
  * if the full alphabet is extremely large, but also requires dedicated
  * combination functions.
  */
final case class CharClassPattern(charSet: Set[Char], negateCharSet: Boolean = false)
    extends RegexPattern {
  //chars should consist only of chars
  require(!charSet.contains(Fsm.anythingElse), "Charset can't contain Fsm.AnythingElse")

  override lazy val alphabet: Set[Char] = charSet + Fsm.anythingElse

  override def hashCode(): Int = if (negateCharSet) charSet.hashCode() else -charSet.hashCode()

  override def toString: String = {
    def isPrintable(c: Char): Boolean =
      ((c >= 32) && (c <= 127)) || (!Character.isISOControl(c) && {
        val block = Character.UnicodeBlock.of(c)
        block != null && block != Character.UnicodeBlock.SPECIALS
      })

    def singleCharToString(c: Char): String =
      if (CharClassPattern.allSpecialChars.contains(c)) {
        //this character needs escape
        "\\" + c
      } else if (CharClassPattern.revEscapes.contains(c)) {
        "\\" + CharClassPattern.revEscapes(c)
      } else if ((c >= 128) && (c <= 255)) {
        "\\x%02X".format(c.toInt)
      } else if (isPrintable(c)) {
        c.toString
      } else if (!c.isSurrogate && (c <= 65535)) {
        "\\u%04X".format(c.toInt)
      } else {
        //we don't support surrogates anyway, but let's be able to print them
        val surrogatePair = Character.toChars(c.toInt)
        //one day Unicode consortium will add 3-chars surrogate, lets handle it today
        surrogatePair.foldLeft("")((str, ch) => str + "\\u%04X".format(ch.toInt))
      }

    def unknownSetToString: String = {
      def listToString(lst: List[Char]): String = lst.size match {
        case 0     => ""
        case 1     => singleCharToString(lst.head)
        case 2 | 3 =>
          //sequence like "abc", no sense to convert to "a-c"
          lst.map(singleCharToString).mkString
        case _ =>
          //sequence of 4 chars or more "abcd" -> "a-d"
          val startChar = singleCharToString(lst.last)
          val endChar   = singleCharToString(lst.head)
          s"$startChar-$endChar"
      }

      val sortedSet = charSet.toList.sorted
      val (strings, pending) = sortedSet.foldLeft((Nil: List[String], Nil: List[Char])) {
        case ((strings, pending), nextChar) =>
          if (pending.isEmpty) {
            (strings, nextChar :: pending)
          } else {
            if (nextChar == (pending.head + 1)) {
              //next char in a continious sequence, add to pending
              (strings, nextChar :: pending)
            } else {
              (listToString(pending) :: strings, nextChar :: Nil)
            }
          }
      }

      val finalStrings = if (pending.isEmpty) {
        strings
      } else {
        listToString(pending) :: strings
      }

      finalStrings.reverse.mkString
    }

    if (charSet.isEmpty) {
      if (negateCharSet) {
        //corner case - any char ~[] => '.'
        "."
      } else {
        //or empty set => nothing
        ""
      }
    } else if (charSet.size == 1) {
      if (negateCharSet) {
        //single character negated
        val cStr = singleCharToString(charSet.head)
        s"[^$cStr]"
      } else {
        //just one simple character
        singleCharToString(charSet.head)
      }
    } else {
      //check for well-known class first, render the set otherwise
      CharClassPattern.knownClassMap
        .find(_._2 == this)
        .map("\\" + _._1)
        .getOrElse {
          val inv = if (negateCharSet) "^" else ""
          s"[$inv$unknownSetToString]"
        }
    }
  }

  override def equivalent(that: RegexPattern): Boolean = that match {
    case thatCharClass: CharClassPattern =>
      (negateCharSet == thatCharClass.negateCharSet) && (charSet == thatCharClass.charSet)
    case _ => false
  }

  override def toFsm(alphabet: Option[Set[Char]] = None): Fsm = {
    val actualAlphabet = alphabet.getOrElse(this.alphabet)
    //0 is initial, 1 is final
    val map = if (negateCharSet) {
      //if negated, make a singular FSM accepting any other characters
      Map(0 -> (actualAlphabet -- charSet).map(_ -> 1).toMap)
    } else {
      //if normal, make a singular FSM accepting only these characters
      Map(0 -> charSet.map(_ -> 1).toMap)
    }
    Fsm(actualAlphabet, Set(0, 1), 0, Set(1), map)
  }

  override def reduced: CharClassPattern = this //Char classes cannot be reduced

  override def isEmpty: Boolean = charSet.isEmpty && !negateCharSet

  /**
    * Negate the current CharClass e.g. [ab] becomes [{@literal ^}ab].
    */
  override def negated: CharClassPattern = CharClassPattern(charSet, !negateCharSet)

  /**
    * Concatenate a sequence of Regex patterns, regardless of differing classes.
    * Call using "a = b + c"
    */
  override def concatenate(that: RegexPattern): ConcPattern = multiply(1) + that

  override def multiply(multiplier: Multiplier): MultPattern = MultPattern(this, multiplier)

  /**
    * Alternate between any two Regex patterns, regardless of differing classes.
    * This method MUST NOT call the toFsm method, because this method is used
    * in turn when converting an FSM back to a regex.
    *
    * ¬A OR ¬B = ¬(A AND B)
    * ¬A OR B = ¬(A - B)
    * A OR ¬B = ¬(B - A)
    * A OR B
    */
  override def union(that: RegexPattern): RegexPattern = that match {
    case thatCharClass: CharClassPattern =>
      (this.negateCharSet, thatCharClass.negateCharSet) match {
        case (true, true) =>
          CharClassPattern(this.charSet & thatCharClass.charSet, negateCharSet = true)
        case (true, false) =>
          CharClassPattern(this.charSet -- thatCharClass.charSet, negateCharSet = true)
        case (false, true) =>
          CharClassPattern(thatCharClass.charSet -- this.charSet, negateCharSet = true)
        case (false, false) => CharClassPattern(this.charSet | thatCharClass.charSet)
      }
    case _ => multiply(1) | that
  }

  /**
    * Return a RegexPattern that can match any string
    *	that both self and other can match.
    *
    * ¬A AND ¬B = ¬(A OR B)
    * ¬A AND B = B - A
    * A AND ¬B = A - B
    * A AND B
    */
  override def intersection(that: RegexPattern): RegexPattern = that match {
    case thatCharClass: CharClassPattern =>
      (this.negateCharSet, thatCharClass.negateCharSet) match {
        case (true, true) =>
          CharClassPattern(this.charSet | thatCharClass.charSet, negateCharSet = true)
        case (true, false)  => CharClassPattern(thatCharClass.charSet -- this.charSet)
        case (false, true)  => CharClassPattern(this.charSet -- thatCharClass.charSet)
        case (false, false) => CharClassPattern(this.charSet & thatCharClass.charSet)
      }
    case _ => multiply(1) & that
  }

  /**
    * CharClass doesn't change when reversed
    */
  override def reversed: CharClassPattern = this

  /**
    * Returns RegexPattern.negated
    */
  def unary_~ : CharClassPattern = negated
}

object ConcPattern extends ParsedPattern {
  def apply(pattern: MultPattern): ConcPattern = new ConcPattern(pattern :: Nil)

  def apply(patterns: RegexPattern*): ConcPattern =
    ConcPattern(patterns.map {
      case mp: MultPattern => mp
      case p               => MultPattern(p, Multiplier.presetOne)
    }.toList)

  val presetEmptyString = ConcPattern(Nil)

  def tryParse(str: CharSequence): Option[(ConcPattern, Int)] = {
    @tailrec
    def parseRecursive(startPosition: Int, parsed: List[MultPattern]): (List[MultPattern], Int) =
      MultPattern.tryParse(str.subSequence(startPosition, str.length)) match {
        case Some((mult, pos)) => {
          val nextMults = mult :: parsed
          val nextPos   = startPosition + pos
          if (nextPos < str.length) {
            //we have one more alternation option
            parseRecursive(nextPos, nextMults)
          } else {
            //no more alternation options
            (nextMults, nextPos)
          }
        }
        case _ => (parsed, startPosition)
      }

    val (seq, seqEndIndex) = parseRecursive(0, Nil)

    if (seq.nonEmpty) {
      val concPattern = ConcPattern(seq.reverse)
      Some(concPattern, seqEndIndex)
    } else {
      //nothing parsed, no characters eaten from input stream
      None
    }
  }
}

/**
  * A ConcatenationPattern is a tuple of mults i.e. an unbroken
  * string of mults occurring one after the other.
  * e.g. abcde[{@literal ^}fg]*h{4}[a-z]+(subpattern)(subpattern2)
  * To express the empty string, use an empty ConcatenationPattern().
  */
final case class ConcPattern(mults: List[MultPattern]) extends RegexPattern {

  override lazy val alphabet: Set[Char] = mults.flatMap(_.alphabet).toSet + Fsm.anythingElse

  override def hashCode: Int = mults.hashCode

  override def equivalent(that: RegexPattern): Boolean = that match {
    case concPattern: ConcPattern =>
      (mults.size == concPattern.mults.size) && mults.corresponds(concPattern.mults)(
        _.equivalent(_)
      )
    case _ => false
  }

  override def reversed: ConcPattern = ConcPattern(mults.map(_.reversed).reverse)

  override def isEmpty: Boolean = mults.forall(_.isEmpty)

  override def toFsm(alphabet: Option[Set[Char]]): Fsm = {
    val actualAlphabet = alphabet.getOrElse(this.alphabet)
    //start with a component accepting only the empty string
    mults.foldLeft(Fsm.epsilonFsm(actualAlphabet))(_ + _.toFsm(actualAlphabet))
  }

  override def concatenate(that: RegexPattern): ConcPattern = {
    val thatConc: ConcPattern = that match {
      case thatConc: ConcPattern       => thatConc
      case thatChars: CharClassPattern => ConcPattern(thatChars * 1)
      case thatMult: MultPattern       => ConcPattern(thatMult)
      case thatAlt: AltPattern         => ConcPattern(thatAlt * 1)
    }

    ConcPattern(mults ++ thatConc.mults)
  }

  override def union(that: RegexPattern): RegexPattern = AltPattern(this) | that

  override def intersection(that: RegexPattern): RegexPattern = AltPattern(this) & that

  override def multiply(multiplier: Multiplier): MultPattern =
    MultPattern(AltPattern(this), multiplier)

  override def reduced: RegexPattern = throw new NotImplementedError("TODO")

  override def negated: RegexPattern = throw new NotImplementedError("TODO")

  /**
    * Return the common prefix of these two ConcPatterns;
    * that is, the largest ConcPattern which can be safely beheaded()
    * from the front of both. The result could be emptystring.
    * "ZYAA, ZYBB" -> "ZY"
    * "CZ, CZ" -> "CZ"
    * "YC, ZC" -> ""
    * With the "suffix" flag set, works from the end. E.g.:
    * "AAZY, BBZY" -> "ZY"
    * "CZ, CZ" -> "CZ"
    * "CY, CZ" -> ""
    */
  def common(that: ConcPattern, suffix: Boolean = false): ConcPattern =
    throw new NotImplementedError("TODO")
}

object AltPattern extends ParsedPattern {
  def apply(pattern: ConcPattern): AltPattern        = new AltPattern(Set(pattern))
  def apply(patterns: List[ConcPattern]): AltPattern = new AltPattern(patterns.toSet)
  def apply(patterns: CharClassPattern*): AltPattern =
    new AltPattern(patterns.map(cp => ConcPattern(cp)).toSet)

  def tryParse(str: CharSequence): Option[(AltPattern, Int)] = {
    @tailrec
    def parseRecursive(startPosition: Int, parsed: List[ConcPattern]): (List[ConcPattern], Int) =
      ConcPattern.tryParse(str.subSequence(startPosition, str.length)) match {
        case Some((conc, pos)) => {
          val nextConcs = conc :: parsed
          val nextPos   = startPosition + pos
          if ((nextPos < str.length) && (str.charAt(nextPos) == '|')) {
            //we have one more alternation option
            parseRecursive(nextPos + 1, nextConcs)
          } else {
            //no more alternation options
            (nextConcs, nextPos)
          }
        }
        case _ => (parsed, startPosition)
      }

    val (seq, seqEndIndex) = parseRecursive(0, Nil)

    if (seq.nonEmpty) {
      val altPattern = AltPattern(seq)
      Some(altPattern, seqEndIndex)
    } else {
      //nothing parsed, no characters eaten from input stream
      None
    }
  }
}

/**
  * AltPattern (also known as an "alt", short for "alternation") is a
  * set of ConcPatterns. A pattern expresses multiple alternate possibilities.
  * When written out as a regex, these would separated by pipes. A pattern
  * containing no possibilities is possible and represents a regular expression
  * matching no strings whatsoever (there is no conventional string form for
  * this).
  * e.g. "abc|def(ghi|jkl)" is an alt containing two ConcPatterns: "abc" and
  * "def(ghi|jkl)". The latter is a ConcPattern containing four MultPatterns: "d", "e", "f"
  * and "(ghi|jkl)". The latter in turn is a MultPattern consisting of an upper bound
  * 1, a lower bound 1, and a multiplicand which is a new subpattern, "ghi|jkl".
  * This new subpattern again consists of two ConcPatterns: "ghi" and "jkl".
  */
final case class AltPattern(concs: Set[ConcPattern]) extends RegexPattern {

  override lazy val alphabet: Set[Char] = concs.flatMap(_.alphabet) + Fsm.anythingElse

  override def hashCode: Int = concs.hashCode

  override def equivalent(that: RegexPattern): Boolean = that match {
    case thatAlt: AltPattern => concs == thatAlt.concs
    case _                   => false
  }

  override def multiply(multiplier: Multiplier): MultPattern = MultPattern(this, multiplier)

  override def isEmpty: Boolean = concs.forall(_.isEmpty)

  override def concatenate(that: RegexPattern): ConcPattern =
    MultPattern(this, Multiplier.presetOne) + that

  override def intersection(that: RegexPattern): RegexPattern = {
    //A deceptively simple method for an astoundingly difficult operation
    val unionAlphabet = alphabet ++ that.alphabet
    //Which means that we can build finite state machines sharing that alphabet
    val combinedFsm = toFsm(unionAlphabet) & that.toFsm(unionAlphabet)
    RegexPattern.fromFsm(combinedFsm)
  }

  override def union(that: RegexPattern): AltPattern = {
    val thatAlt: AltPattern = that match {
      case altPattern: AltPattern => altPattern
      case charsPattern: CharClassPattern =>
        AltPattern(ConcPattern(MultPattern(charsPattern, Multiplier.presetOne)))
      case concPattern: ConcPattern => AltPattern(concPattern)
      case multPattern: MultPattern => AltPattern(ConcPattern(multPattern))
    }

    AltPattern(concs ++ thatAlt.concs)
  }

  override def toFsm(alphabet: Option[Set[Char]]): Fsm = {
    val actualAlphabet = alphabet.getOrElse(this.alphabet)
    concs.foldLeft(Fsm.nullFsm(actualAlphabet))(_ | _.toFsm(actualAlphabet))
  }

  override def reduced: RegexPattern = throw new NotImplementedError("TODO")

  override def negated: RegexPattern = throw new NotImplementedError("TODO")

  override def reversed: AltPattern = AltPattern(concs.map(_.reversed))
}

object MultPattern extends ParsedPattern {
  def tryParse(str: CharSequence): Option[(MultPattern, Int)] = {
    //matches single charclass or unnamed group (...)
    def matchMultiplicand(startIndex: Int): Option[(RegexPattern, Int)] =
      if (startIndex < str.length) {
        str.charAt(startIndex) match {
          case '(' => {
            //parse unnamed group
            val parsedAltPattern =
              AltPattern.tryParse(str.subSequence(startIndex + 1, str.length))
            parsedAltPattern.flatMap {
              case (altPattern, altInnerEndIndex) => {
                val altEndIndex = startIndex + 1 + altInnerEndIndex
                if ((altEndIndex < str.length) && (str.charAt(altEndIndex) == ')')) {
                  //we found closing bracket, group finished
                  Some(altPattern.asInstanceOf[RegexPattern], altEndIndex + 1)
                } else {
                  None
                }
              }
            }
          }
          case _ => CharClassPattern.tryParse(str.subSequence(startIndex, str.length))
        }
      } else {
        None
      }

    matchMultiplicand(0).flatMap {
      case (multiplicandPattern, multiplicandEndIndex) => {
        val (multiplier, multiplierEndIndex) =
          Multiplier.tryParse(str.subSequence(multiplicandEndIndex, str.length))
        Some(
          MultPattern(multiplicandPattern, multiplier),
          multiplicandEndIndex + multiplierEndIndex
        )
      }
    }
  }
}

/**
  * A MultPattern is a combination of a multiplicand with
  * a multiplier (a min and a max). The vast majority of characters in regular
  * expressions occur without a specific multiplier, which is implicitly
  * equivalent to a min of 1 and a max of 1, but many more have explicit
  * multipliers like "*" (min = 0, max = inf) and so on.
  * e.g. a, b{2}, c?, d*, [efg]{2,5}, f{2,}, (anysubpattern)+, .*, and so on
  */
final case class MultPattern(multiplicand: RegexPattern, multiplier: Multiplier)
    extends RegexPattern {

  override lazy val alphabet: Set[Char] = multiplicand.alphabet + Fsm.anythingElse

  override def hashCode: Int = multiplicand.hashCode ^ multiplier.hashCode

  override def equivalent(that: RegexPattern): Boolean = that match {
    case m: MultPattern => (multiplier == m.multiplier) && multiplicand.equivalent(m.multiplicand)
    case _              => false
  }

  override def multiply(nextMultiplier: Multiplier): MultPattern =
    if (nextMultiplier.isOne)
      this
    else if (multiplier.canMultiplyBy(nextMultiplier))
      MultPattern(multiplicand, multiplier * nextMultiplier)
    else
      MultPattern(AltPattern(ConcPattern(this)), multiplier)

  override def concatenate(that: RegexPattern): ConcPattern = ConcPattern(this) + that

  override def union(that: RegexPattern): RegexPattern = ConcPattern(this) | that

  override def intersection(that: RegexPattern): RegexPattern = {
    val thatMult = that match {
      case thatMult: MultPattern      => thatMult
      case otherPattern: RegexPattern => MultPattern(otherPattern, Multiplier.presetOne)
    }

    if ((thatMult.multiplicand == this.multiplicand) && this.multiplier.canIntersect(
          thatMult.multiplier
        )) {
      MultPattern(this.multiplicand, this.multiplier & thatMult.multiplier)
    } else {
      ConcPattern(this) & that
    }
  }

  override def isEmpty: Boolean = multiplicand.isEmpty || (multiplier.max.getOrElse(1) < 1)

  override def reversed: MultPattern = MultPattern(multiplicand.reversed, multiplier)

  override def toFsm(alphabet: Option[Set[Char]]): Fsm = {
    val actualAlphabet = alphabet.getOrElse(this.alphabet)

    val startFsm = multiplicand.toFsm(actualAlphabet)
    //accepts e.g. "ab"
    val mandatoryFsm = startFsm * multiplier.mandatory.getOrElse(0)
    //unlimited additional copies
    val optionalFsm = if (multiplier.optional == Multiplier.Inf) {
      startFsm.star
    } else {
      (Fsm.epsilonFsm(actualAlphabet) | startFsm) * multiplier.optional.get
    }

    mandatoryFsm + optionalFsm
  }

  override def reduced: RegexPattern = throw new NotImplementedError("TODO")

  override def negated: RegexPattern = throw new NotImplementedError("TODO")

  /**
    * Return the common part of these two MultPatterns. This is the largest MultPattern
    * which can be safely subtracted from both the originals.
    * The multiplier on this MultPattern could be zero: this is the case if,
    * for example, the multiplicands disagree.
    */
  def common(that: MultPattern): MultPattern =
    if (multiplicand == that.multiplicand) {
      MultPattern(multiplicand, multiplier.common(that.multiplier))
    } else {
      //Multiplicands disagree, no common part at all.
      MultPattern(RegexPattern.nothing, Multiplier.presetZero)
    }
}
