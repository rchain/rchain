package coop.rchain.regex

/**
  * Companion object for the Multiplier class
  */
private[regex] object Multiplier {
  val Inf: Option[Int] = None

  val presetZero: Multiplier     = Multiplier(Some(0), Some(0))
  val presetQuestion: Multiplier = Multiplier(Some(0), Some(1))
  val presetOne: Multiplier      = Multiplier(Some(1), Some(1))
  val presetStar: Multiplier     = Multiplier(Some(0), Inf)
  val presetPlus: Multiplier     = Multiplier(Some(1), Inf)

  def apply(min: Int, max: Int): Multiplier = new Multiplier(Some(min), Some(max))

  def apply(multiplier: Int): Multiplier = new Multiplier(Some(multiplier), Some(multiplier))

  def parse(cs: CharSequence): Option[Multiplier] = {
    val (mult, count) = tryParse(cs)
    if (count == cs.length)
      Some(mult)
    else
      None
  }

  private[this] val rxRange =
    """(^\{\s*(\d+)\s*(,\s*(\d+)?)?\s*\}).*""".r("all", "min", "hasMax", "max")

  /**
    * Attempts to parse character sequence as regex repetitions bounds
    * Supported patterns are: * + ? {1,2} {3,}
    * All other sequences are ignored, and {1,1} multiplier is returned
    * Return Tuple(parsed multiplier, count of characters accepted)
    * In case of any error - returns (Multiplier.presetOne, 0) - that means no repetitions,
    * and no characters matched.
    *
    * Supported Range Quantifiers are: {#}, {#,}, {#,#}
    * Range Quantifier {,#} doesn't supported (like in PHP/JS/GO/Java engines),
    * and in contrast to Python engine which understands it like '0 to # repetitions'
    */
  def tryParse(cs: CharSequence): (Multiplier, Int) =
    if (cs.length == 0) {
      (Multiplier.presetOne, 0)
    } else {
      cs match {
        case rxRange(all, min, hasMax, max) => {
          val minBound = Some(min.toInt)
          val maxBound = if (hasMax != null) {
            //we have a ",...}" part
            if (max != null) {
              //{#,#} pattern, getting the upper bound
              Some(max.toInt)
            } else {
              //{#,} pattern - upper bound is infinite
              Multiplier.Inf
            }
          } else {
            //we found {#} pattern, exact count
            minBound
          }
          (Multiplier(minBound, maxBound), all.length)
        }
        case _ =>
          cs.charAt(0) match {
            case '*' => (Multiplier.presetStar, 1)
            case '?' => (Multiplier.presetQuestion, 1)
            case '+' => (Multiplier.presetPlus, 1)
            case _   => (Multiplier.presetOne, 0)
          }
      }
    }
}

/**
  * A min and a max bounds. The value either has a bound (Some[Int]) or not
  * (None - means unbounded/infinite). The vast majority of characters in regular
  * expressions occur without a specific multiplier, which is implicitly
  * equivalent to a min of 1 and a max of 1, but many more have explicit
  * multipliers like "*" (min = 0, max = inf) and so on.
  * Although it seems odd and can lead to some confusing edge cases, we do
  * also permit a max of 0 (iff min is 0 too). This allows the multiplier
  * "zero" to exist, which actually are quite useful in their own special way.
  */
final case class Multiplier(min: Option[Int], max: Option[Int]) {

  /**
    * Infinite multiplier bound
    */
  private val Inf = None

  require(
    (min, max) match {
      case (Some(x), Some(y)) => (0 <= x) && (x <= y)
      case (Some(x), Inf)     => 0 <= x //min valid, max infinite
      case (Inf, Some(_))     => false //min is infinite
      case (Inf, Inf)         => true
    },
    "Invalid multiplier bounds"
  )

  /**
    * Find the shared part of two multipliers. This is the largest multiplier
    * which can be safely subtracted from both the originals.
    * This may return the "zero" multiplier.
    */
  def common(that: Multiplier): Multiplier = {
    val newMandatory = minVal(mandatory, that.mandatory)
    val newOptional  = minVal(optional, that.optional)
    Multiplier(newMandatory, newMandatory + newOptional)
  }

  /**
    * We need some math operation on Option[Int], assuming that Inf == infinite
    */
  implicit class OptionIntMath(opt: Option[Int]) {

    /**
      * Add two bounds. Meaningful for all bounds.
      */
    def +(that: Option[Int]): Option[Int] =
      for {
        x <- opt
        y <- that
      } yield x + y //Inf + _ => Inf; _ + Inf => Inf

    /**
      * Subtract another bound from this one.
      * Caution: this operation is not meaningful for all bounds.
      */
    def -(that: Option[Int]): Option[Int] = (opt, that) match {
      case (Some(_), Inf) =>
        throw new IllegalArgumentException("Can't substract infinity") //something - infinity => invalid operation
      case (Inf, Inf)         => Some(0) //Infinity minus infinity is zero.
      case (Inf, _)           => Inf //inf - anything => inf
      case (Some(x), Some(y)) => Some(x - y)
    }

    def *(that: Option[Int]): Option[Int] =
      for {
        x <- opt
        y <- that
      } yield x * y //Inf * _ => Inf; _ * Inf => Inf

    def >=(that: Option[Int]): Boolean = (opt, that) match {
      case (Inf, _)           => true
      case (Some(_), Inf)     => false
      case (Some(x), Some(y)) => x >= y
    }

    def <(that: Option[Int]): Boolean = !(this >= that)

    def <=(that: Option[Int]): Boolean = (opt, that) match {
      case (_, Inf)           => true
      case (Inf, Some(_))     => false
      case (Some(x), Some(y)) => x <= y
    }

    def >(that: Option[Int]): Boolean = !(this <= that)
  }

  def minVal(first: Option[Int], second: Option[Int]): Option[Int] =
    if (first <= second) first else second
  def maxVal(first: Option[Int], second: Option[Int]): Option[Int] =
    if (first >= second) first else second

  def mandatory: Option[Int] = min
  def optional: Option[Int]  = max - min

  override def toString: String = (min, max) match {
    case (Some(x), Some(0))           => s"{$x,0}"
    case (Some(0), Some(1))           => "?"
    case (Some(1), Some(1))           => ""
    case (Some(0), Inf)               => "*"
    case (Some(1), Inf)               => "+"
    case (Some(x), Inf)               => s"{$x,}"
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
    val diffOptional  = optional - that.optional
    Multiplier(diffMandatory, diffMandatory + diffOptional)
  }

  /**
    * Intersection. Max bounds range that is shared between
    * two multipliers.
    * This operation is not meaningful for all pairs of multipliers.
    */
  def &(that: Multiplier): Multiplier = {
    if (!this.canIntersect(that))
      throw new IllegalArgumentException(s"Can't intersect $this and $that")

    val newMin = maxVal(this.min, that.min) //max of two
    val newMax = minVal(this.max, that.max) //min of two

    Multiplier(newMin, newMax)
  }

  /**
    * Intersection is not well-defined for all pairs of multipliers.
    * For example:
    * {2,3} & {3,4} = {3}
    * {2,} & {1,7} = {2,7}
    * {2} & {5} = ERROR
    */
  def canIntersect(that: Multiplier): Boolean = !((max < that.min) || (that.max < min))

  /**
    * Union. {1,3} | {2,4} => {1,4}
    */
  def |(that: Multiplier): Multiplier = {
    if (!this.canUnion(that))
      throw new IllegalArgumentException(s"Can't union $this and $that")

    val newMin = minVal(this.min, that.min) //max of two
    val newMax = maxVal(this.max, that.max) //min of two

    Multiplier(newMin, newMax)
  }

  /**
    * Union is not defined for all pairs of multipliers. e.g. {0,1} | {3,4}
    */
  def canUnion(that: Multiplier): Boolean =
    !(((max + Some(1)) < that.min) || ((that.max + Some(1)) < min))

  def ==(that: Multiplier): Boolean = (min == that.min) && (max == that.max)
}
