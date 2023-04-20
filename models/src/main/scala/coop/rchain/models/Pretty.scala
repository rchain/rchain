package coop.rchain.models
import java.io.{PrintWriter, StringWriter}

import com.google.protobuf.ByteString
import coop.rchain.crypto.hash.Blake2b512Random
import cats.Eval

import scala.annotation.switch
import scala.collection.immutable.{BitSet, HashSet}

/**
  * A typeclass for pretty-printing objects to compiling Scala code.
  *
  * A fairly complete implementation covering all rholang AST classes
  * is derived generically using Magnolia (some leaf instances were needed ofc).
  *
  * @tparam A the type of values this instance provides pretty-printing for
  */
trait Pretty[A] {
  def pretty(value: A, indentLevel: Int): String
}

object Pretty extends PrettyInstances {

  def apply[A: Pretty](implicit ev: Pretty[A]): Pretty[A] = ev

  def pretty[A](value: A)(implicit ev: Pretty[A]) = ev.pretty(value, 0)

}

trait PrettyInstances extends PrettyDerivation {
  import PrettyUtils._

  implicit val PrettyString  = singleLine(literalize(_: String))
  implicit val PrettyBoolean = fromToString[Boolean]
  implicit val PrettyByte    = fromToString[Byte]
  implicit val PrettyInt     = fromToString[Int]
  implicit val PrettyLong    = fromToString[Long]
  implicit val PrettyBitSet  = fromToString[BitSet]
  implicit val PrettyBigInt  = fromToString[BigInt]

  // obviously won't print compiling code,
  // but seeing the stacktrace is more important in this case
  implicit val PrettyThrowable: Pretty[Throwable] = (value: Throwable, indentLevel: Int) => {
    val stackTrace = new StringWriter()
    value.printStackTrace(new PrintWriter(stackTrace))
    "\n" + stackTrace.toString
  }

  implicit val PrettyByteString: Pretty[ByteString] = (value: ByteString, indentLevel: Int) =>
    "ByteString.copyFrom(Array[Byte]" + parenthesised(value.toByteArray, indentLevel) + ")"

  implicit val PrettyBlake2b512Random: Pretty[Blake2b512Random] = fromWrapped(
    Blake2b512Random.typeMapper.toBase,
    x => s"Blake2b512Random.typeMapper.toCustom($x)"
  )

  implicit def prettyArray[A: Pretty]: Pretty[Array[A]] = "Array" + parenthesised(_, _)

  implicit def prettySeq[A: Pretty]     = fromIterable[Seq[A], A]("Seq")
  implicit def prettySet[A: Pretty]     = fromIterable[Set[A], A]("Set")
  implicit def prettyHashSet[A: Pretty] = fromIterable[HashSet[A], A]("HashSet")
  implicit def prettySortedParHashSet   = fromIterable[SortedParHashSet, Par]("SortedParHashSet(Seq")
  implicit def prettySortedParMap       = fromIterable[SortedParMap, (Par, Par)]("SortedParMap(Map")

  implicit def prettyPair[A: Pretty, B: Pretty]: Pretty[(A, B)] =
    (value: (A, B), indentLevel: Int) => {
      val prettyA = Pretty[A].pretty(value._1, indentLevel)
      val prettyB = Pretty[B].pretty(value._2, indentLevel)
      s"$prettyA -> $prettyB"
    }

  implicit def prettyMap[A: Pretty, B: Pretty]: Pretty[Map[A, B]] =
    (value: Map[A, B], indentLevel: Int) => {
      val pairs = value
        .map(
          kv => {
            val key   = Pretty[A].pretty(kv._1, indentLevel + 1)
            val value = Pretty[B].pretty(kv._2, indentLevel + 1)
            s"$key -> $value"
          }
        )
      "Map" + parenthesisedStrings(pairs, indentLevel)
    }

  implicit def prettyAlwaysEqual[A: Pretty]: Pretty[AlwaysEqual[A]] =
    fromWrapped(_.item, value => s"AlwaysEqual($value)")

  implicit def prettyEval[A: Pretty]: Pretty[Eval[A]] =
    (value: Eval[A], indentLevel: Int) =>
      s"Eval.now(${Pretty[A].pretty(value.value, indentLevel)}) /* was Eval.${value.getClass.getSimpleName} */"

  implicit val PrettyPar: Pretty[Par]   = gen[Par]
  implicit val PrettyExpr               = gen[Expr]
  implicit val PrettyConnective         = gen[Connective]
  implicit val PrettyReceive            = gen[Receive]
  implicit val PrettyReceiveBind        = gen[ReceiveBind]
  implicit val PrettyTaggedContinuation = gen[TaggedContinuation]
  implicit val PrettyParWithRandom      = gen[ParWithRandom]

  def singleLine[A](print: A => String): Pretty[A] = (value: A, indentLevel: Int) => print(value)

  def fromToString[A]: Pretty[A] = (value: A, indentLevel: Int) => value.toString

  def fromIterable[F <: Iterable[A], A: Pretty](prefix: String): Pretty[F] = {
    val prefixParensCount = prefix.count(_ == '(')
    prefix + parenthesised(_, _) + (")" * prefixParensCount)
  }

  def fromWrapped[A, B: Pretty](f: A => B, wrappingCode: String => String): Pretty[A] =
    (value: A, indentLevel: Int) => wrappingCode(Pretty[B].pretty(f(value), indentLevel))
}

trait PrettyDerivation {
  import PrettyUtils._
  import magnolia._

  type Typeclass[T] = Pretty[T]

  def combine[T](ctx: CaseClass[Pretty, T]): Pretty[T] = new Pretty[T] {

    override def pretty(value: T, indentLevel: Int): String =
      if (ctx.isObject) {
        s"${ctx.typeName.short}"
      } else {
        s"${ctx.typeName.short}${nonDefaultParameters(value, indentLevel)}"
      }

    private def nonDefaultParameters(value: T, indentLevel: Int): String = {
      val nonDefaultParameters =
        ctx.parameters.filter(p => !p.default.contains(p.dereference(value)))

      val paramStrings = if (nonDefaultParameters.sizeIs == ctx.parameters.size) {
        nonDefaultParameters.map(p => printParam(value, p, indentLevel))
      } else {
        nonDefaultParameters.map(p => s"${p.label} = ${printParam(value, p, indentLevel)}")
      }

      parenthesisedStrings(paramStrings, indentLevel)
    }

    private def printParam(value: T, p: Param[Typeclass, T], indentLevel: Int) =
      s"${p.typeclass.pretty(p.dereference(value), indentLevel + 1)}"
  }

  def dispatch[T](ctx: SealedTrait[Pretty, T]): Pretty[T] =
    new Pretty[T] {
      override def pretty(value: T, indentLevel: Int): String =
        // format: off
        ctx.dispatch(value) {
          sub => sub.typeclass.pretty(sub.cast(value), indentLevel)
        }
        // format: on
    }

  implicit def gen[T]: Pretty[T] = macro Magnolia.gen[T]
}

object PrettyUtils {

  def parenthesised[A: Pretty](values: Iterable[A], indentLevel: Int): String =
    parenthesisedStrings(values.map(Pretty[A].pretty(_, indentLevel + 1)), indentLevel)

  def parenthesisedStrings(paramStrings: Iterable[String], indentLevel: Int): String =
    if (paramStrings.isEmpty) {
      "()"
    } else {
      paramStrings.mkString(
        "(\n" + indent(indentLevel + 1),
        ",\n" + indent(indentLevel + 1),
        "\n" + indent(indentLevel) + ")"
      )
    }

  private def indent(indentLevel: Int) =
    ("  " * (indentLevel))

  /**
    * Convert a string to a C&P-able literal. Basically
    * copied verbatim from the uPickle source code.
    */
  @SuppressWarnings(Array("org.wartremover.warts.Var", "org.wartremover.warts.NonUnitStatements"))
  // TODO remove this var
  def literalize(s: IndexedSeq[Char], unicode: Boolean = true): String = {
    val sb = new StringBuilder
    sb.append('"')
    var i   = 0
    val len = s.length
    while (i < len) {
      escapeChar(s(i), sb)
      i += 1
    }
    sb.append('"')

    sb.result()
  }

  private def escapeChar(c: Char, sb: StringBuilder, unicode: Boolean = true): StringBuilder =
    (c: @switch) match {
      case '"'  => sb.append("\\\"")
      case '\\' => sb.append("\\\\")
      case '\b' => sb.append("\\b")
      case '\f' => sb.append("\\f")
      case '\n' => sb.append("\\n")
      case '\r' => sb.append("\\r")
      case '\t' => sb.append("\\t")
      case c =>
        if (c < ' ' || (c > '~' && unicode)) sb.append("\\u%04x" format c.toInt)
        else sb.append(c)
    }
}
