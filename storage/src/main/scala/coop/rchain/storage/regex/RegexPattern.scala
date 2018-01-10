package coop.rchain.storage.regex

object RegexPattern {}

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
  def concatenate(that: RegexPattern): RegexPattern

  /**
    * Alternate between any two Regex patterns, regardless of differing classes.
    * This method MUST NOT call the toFsm method, because this method is used
    * in turn when converting an FSM back to a regex.
    */
  def union(that: RegexPattern): RegexPattern

  /**
    * Takes the current RegexPattern and simplifies it in every way possible,
    * returning a simpler RegexPattern which is quite probably not of the same class
    * as the original. Approaches are different for every descendant.
    */
  def reduced: RegexPattern

  /**
    * Return a set of all unique characters used in this RegexPattern.
    * In theory this could be a static property, but in the vast majority of cases
    * this will never be queried so it's a waste of computation to
    * calculate it every time a RegexPattern is instantiated.
    * By convention, fsm.anythingElse is always included in this result.
    */
  def alphabet: Set[Char]

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
    * Negate the current Pattern
    * Call using "RegexPattern = ~RegexPattern"
    */
  def negated: RegexPattern

  /**
    * Equivalent to repeated concatenation. Multiplier consists of a minimum
    * and a maximum; maximum may be infinite (for Kleene star closure).
    * Call using "a = b * qm"
    */
  def multiply(multiplier: Multiplier): RegexPattern

  /**
    * Equivalent to repeated concatenation. Multiplier consists of a minimum
    * and a maximum; maximum may be infinite (for Kleene star closure).
    * Call using "a = b * qm"
    */
  def multiply(multiplier: Int): RegexPattern = multiply(new Multiplier(multiplier))

  /**
    * Return False if there exists a string which the present RegexPattern
    * can match. Return true if no such string exists. Example of empty
    * patterns: CharClassPattern()
    */
  def isEmpty: Boolean

  def toFsm(alphabet: Option[Set[Char]] = None): Fsm

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
  final def +(that: RegexPattern): RegexPattern = concatenate(that)

  /**
    * Alternate between any two Regex patterns, regardless of differing classes.
    */
  final def |(that: RegexPattern): RegexPattern = union(that)

  /**
    * Intersection between any two Regex patterns.
    */
  final def &(that: RegexPattern): RegexPattern = intersection(that)

  /**
    * Returns RegexPattern.negated
    */
  final def unary_~ : RegexPattern = negated

  /**
    * Returns repeated concatenation of this RegexPattern
    */
  final def *(multiplier: Int) = multiply(multiplier)
  //endregion
}

/**
  * Companion object for the CharClassPattern, used only for easy testing
  */
object CharClassPattern {
  def apply(charSet: String) = new CharClassPattern(charSet)
  def apply(charSet: Seq[Char]) = new CharClassPattern(charSet)
  def apply(charSet: Set[Char]) = new CharClassPattern(charSet)
  def apply(charSet: String, negateCharSet : Boolean) = new CharClassPattern(charSet, negateCharSet)
  def apply(charSet: Seq[Char], negateCharSet : Boolean) = new CharClassPattern(charSet, negateCharSet)
  def apply(charSet: Set[Char], negateCharSet : Boolean) = new CharClassPattern(charSet, negateCharSet)
}

/**
  * A CharClass is basically a Set of symbols. The reason for the
  * CharClass object instead of using frozenset directly is to allow us to
  * set a "negated" flag. A CharClass with the negation flag set is assumed
  * to contain every symbol that is in the alphabet of all symbols but not
  * explicitly listed inside the Set. e.g. [ \ ^ a ]. This is very handy
  * if the full alphabet is extremely large, but also requires dedicated
  * combination functions.
  */
final case class CharClassPattern(charSet: Set[Char], negateCharSet: Boolean = false)
    extends RegexPattern {
  //chars should consist only of chars
  require(!charSet.contains(Fsm.anythingElse), "Charset can't contain Fsm.AnythingElse")

  def this(charSet: Seq[Char], negateCharSet: Boolean) = this(charSet.toSet, negateCharSet)
  def this(charSet: String, negateCharSet: Boolean) = this(charSet.toSet, negateCharSet)
  def this(charSet: String) = this(charSet.toSet, false)
  def this(charSet: Seq[Char]) = this(charSet.toSet, false)

  override def equivalent(that: RegexPattern): Boolean = that match {
    case thatCharClass: CharClassPattern =>
      (negateCharSet == thatCharClass.negateCharSet) && (charSet == thatCharClass.charSet)
    case _ => false
  }

  override def toFsm(alphabet: Option[Set[Char]] = None) = {
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

  override def alphabet: Set[Char] = charSet + Fsm.anythingElse

  override def hashCode(): Int = if (negateCharSet) charSet.hashCode() else -charSet.hashCode()

  override def reduced: RegexPattern = this //Char classes cannot be reduced

  override def isEmpty: Boolean = charSet.isEmpty && !negateCharSet

  /**
    * Negate the current CharClass e.g. [ab] becomes [ \ ^ ab].
    */
  override def negated: RegexPattern = CharClassPattern(charSet, !negateCharSet)

  /**
    * Concatenate a sequence of Regex patterns, regardless of differing classes.
    * Call using "a = b + c"
    */
  override def concatenate(that: RegexPattern): RegexPattern = multiply(1) + that

  override def multiply(that: Multiplier): RegexPattern =
    if (that.isOne)
      this
    else
      throw new NotImplementedError("TODO")

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
        case (true, true)   => CharClassPattern(this.charSet & thatCharClass.charSet, true)
        case (true, false)  => CharClassPattern(this.charSet -- thatCharClass.charSet, true)
        case (false, true)  => CharClassPattern(thatCharClass.charSet -- this.charSet, true)
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
        case (true, true)   => CharClassPattern(this.charSet | thatCharClass.charSet, true)
        case (true, false)  => CharClassPattern(thatCharClass.charSet -- this.charSet)
        case (false, true)  => CharClassPattern(this.charSet -- thatCharClass.charSet)
        case (false, false) => CharClassPattern(this.charSet & thatCharClass.charSet)
      }
    case _ => multiply(1) & that
  }
}

/**
  * A min and a max. The vast majority of characters in regular
  * expressions occur without a specific multiplier, which is implicitly
  * equivalent to a min of 1 and a max of 1, but many more have explicit
  * multipliers like "*" (min = 0, max = inf) and so on.
  * Although it seems odd and can lead to some confusing edge cases, we do
  * also permit a max of 0 (iff min is 0 too). This allows the multiplier
  * "zero" to exist, which actually are quite useful in their own special way.
  */
final case class Multiplier(min: Option[Int], max: Option[Int]) {
  def this(min: Int, max: Int) = this(Some(min), Some(max))
  def this(multiplier: Int) = this(Some(multiplier), Some(multiplier))

  require(
    (min, max) match {
      case (Some(x), Some(y)) => (0 <= x) && (x <= y)
      case (Some(x), None)    => 0 <= x //min valid, max infinite
      case (None, Some(_))    => false //min is infinite
      case (None, None)       => true
    },
    "Invalid multiplier bounds"
  )

  /**
    * We need some math operation on Option[Int], assuming that None == infinite
    */
  implicit class OptionIntMath(opt: Option[Int]) {
    def +(that: Option[Int]): Option[Int] = (opt, that) match {
      case (Some(_), None)    => None
      case (None, Some(_))    => None
      case (None, None)       => None
      case (Some(x), Some(y)) => Some(x + y)
    }

    /**
      * Subtract another bound from this one.
      * Caution: this operation is not meaningful for all bounds.
      */
    def -(that: Option[Int]): Option[Int] = (opt, that) match {
      case (Some(_), None) =>
        throw new IllegalArgumentException("Can't substract infinity") //something - infinity => invalid operation
      case (None, None)       => Some(0) //Infinity minus infinity is zero.
      case (None, _)          => None //inf - anything => inf
      case (Some(x), Some(y)) => Some(x - y)
    }

    def *(that: Option[Int]): Option[Int] = (opt, that) match {
      case (None, _)          => None
      case (_, None)          => None
      case (Some(x), Some(y)) => Some(x * y)
    }

    def >=(that: Option[Int]): Boolean = (opt, that) match {
      case (None, _)          => true
      case (Some(_), None)    => false
      case (Some(x), Some(y)) => x >= y
    }

    def <=(that: Option[Int]): Boolean = (opt, that) match {
      case (_, None)          => true
      case (None, Some(_))    => false
      case (Some(x), Some(y)) => x <= y
    }
  }

  def mandatory: Option[Int] = min
  def optional: Option[Int] = max - min

  override def toString: String = (min, max) match {
    case (Some(_), Some(0)) =>
      throw new ClassFormatError("Can't serialise a multiplier with max bound 0")
    case (Some(0), Some(1))           => "?"
    case (Some(1), Some(1))           => ""
    case (Some(0), None)              => "*"
    case (Some(1), None)              => "+"
    case (Some(x), None)              => s"{$x,}"
    case (Some(x), Some(y)) if x == y => s"{$x}"
    case (Some(x), Some(y))           => s"{$x,$y}"
  }

  def isOne: Boolean = min.contains(1) && max.contains(1)

  /**
    * Multiplication is not well-defined for all pairs of multipliers because
    * the resulting possibilities do not necessarily form a continuous range.
    * For example:
    * {0,x} * {0,y} = {0,x*y}
    * {2} * {3} = {6}
    * {2} * {1,2} = ERROR
    * The proof isn't simple but suffice it to say that {p,p+q} * {r,r+s} is
    * equal to {pr, (p+q)(r+s)} only if s=0 or qr+1 >= p. If not, then at least
    * one gap appears in the range. The first inaccessible number is (p+q)r + 1.
    */
  def canMultiplyBy(that: Multiplier): Boolean =
    mandatory.contains(0) || (optional * that.mandatory + Some(1) >= mandatory)

  /**
    * Multiply this multiplier by another
    */
  def *(that: Multiplier): Multiplier =
    if (canMultiplyBy(that))
      Multiplier(min * that.min, max * that.max)
    else
      throw new IllegalArgumentException(s"Can't multiply $this and $that")

  /**
    * Add two multipliers together
    */
  def +(that: Multiplier): Multiplier = Multiplier(min + that.min, max + that.max)

  /**
    * Subtract another multiplier from this one.
    * Caution: multipliers are not totally ordered.
    * This operation is not meaningful for all pairs of multipliers.
    */
  def -(that: Multiplier): Multiplier = {
    val diffMandatory = mandatory - that.mandatory
    val diffOptional = optional - that.optional
    Multiplier(diffMandatory, diffMandatory + diffOptional)
  }
}
