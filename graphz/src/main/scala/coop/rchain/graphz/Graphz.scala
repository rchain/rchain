package coop.rchain.graphz

import cats._
import cats.syntax.all._
import cats.effect.Ref

trait GraphSerializer[F[_]] {
  def push(str: String, suffix: String = "\n"): F[Unit]
}

class StringSerializer[F[_]](ref: Ref[F, StringBuffer]) extends GraphSerializer[F] {
  override def push(str: String, suffix: String): F[Unit] = ref.update(_.append(str + suffix))
}

class ListSerializer[F[_]](ref: Ref[F, Vector[String]]) extends GraphSerializer[F] {
  override def push(str: String, suffix: String): F[Unit] = ref.update(_ :+ (str + suffix))
}

sealed trait GraphType
case object Graph   extends GraphType
case object DiGraph extends GraphType

sealed trait GraphShape
case object Circle        extends GraphShape
case object DoubleCircle  extends GraphShape
case object DoubleOctagon extends GraphShape
case object Box           extends GraphShape
case object PlainText     extends GraphShape
case object Msquare       extends GraphShape
case object Record        extends GraphShape

sealed trait GraphRank
case object Same   extends GraphRank
case object Min    extends GraphRank
case object Source extends GraphRank
case object Max    extends GraphRank
case object Sink   extends GraphRank

sealed trait GraphRankDir

case object TB extends GraphRankDir
case object BT extends GraphRankDir
case object LR extends GraphRankDir
case object RL extends GraphRankDir

sealed trait GraphStyle

case object Solid  extends GraphStyle
case object Bold   extends GraphStyle
case object Filled extends GraphStyle
case object Invis  extends GraphStyle
case object Dotted extends GraphStyle
case object Dashed extends GraphStyle

sealed trait GraphArrowType

case object NormalArrow extends GraphArrowType
case object InvArrow    extends GraphArrowType
case object NoneArrow   extends GraphArrowType

object Graphz {

  implicit val showShape: Show[GraphShape] = Show.show {
    case Circle        => "circle"
    case DoubleCircle  => "doublecircle"
    case DoubleOctagon => "doubleoctagon"
    case Box           => "box"
    case PlainText     => "plaintext"
    case Msquare       => "Msquare"
    case Record        => "record"
  }

  def smallToString[A]: Show[A] = Show.show(_.toString.toLowerCase)

  implicit val showStyle: Show[GraphStyle]     = smallToString[GraphStyle]
  implicit val showRank: Show[GraphRank]       = smallToString[GraphRank]
  implicit val showRankDir: Show[GraphRankDir] = Show.fromToString[GraphRankDir]
  implicit val showArrowType: Show[GraphArrowType] = Show.show {
    case NormalArrow => "normal"
    case InvArrow    => "inv"
    case NoneArrow   => "none"
  }

  val DefaultShape: GraphShape = Circle

  def apply[F[_]: Monad](
      name: String,
      gtype: GraphType,
      ser: GraphSerializer[F],
      subgraph: Boolean = false,
      comment: Option[String] = None,
      label: Option[String] = None,
      splines: Option[String] = None,
      rank: Option[GraphRank] = None,
      rankdir: Option[GraphRankDir] = None,
      style: Option[String] = None,
      color: Option[String] = None,
      graph: Map[String, String] = Map.empty,
      node: Map[String, String] = Map.empty,
      edge: Map[String, String] = Map.empty
  ): F[Graphz[F]] = {

    def insert(str: Option[String], v: String => String): F[Unit] = {
      val indent = if (subgraph) tab + tab else tab
      str.fold(().pure[F])(s => ser.push(indent + v(s)))
    }

    for {
      _ <- comment.fold(().pure[F])(c => ser.push(s"// $c"))
      t = if (subgraph) s"$tab$tab" else tab
      _ <- ser.push(head(gtype, subgraph, name))
      _ <- insert(label, l => s"label = ${quote(l)}")
      _ <- insert(style, s => s"style=$s")
      _ <- insert(color, s => s"color=$s")
      _ <- insert(rank.map(_.show), r => s"rank=$r")
      _ <- insert(rankdir.map(_.show), r => s"rankdir=$r")
      _ <- insert(attrMkStr(graph), n => s"graph $n")
      _ <- insert(attrMkStr(node), n => s"node $n")
      _ <- insert(attrMkStr(edge), n => s"edge $n")
      _ <- insert(splines.map(_.show), s => s"splines=$s")
    } yield new Graphz[F](gtype, t, ser)
  }

  def subgraph[F[_]: Monad](
      name: String,
      gtype: GraphType,
      ser: GraphSerializer[F],
      label: Option[String] = None,
      rank: Option[GraphRank] = None,
      rankdir: Option[GraphRankDir] = None,
      style: Option[String] = None,
      color: Option[String] = None
  ): F[Graphz[F]] =
    apply[F](
      name,
      gtype,
      subgraph = true,
      label = label,
      rank = rank,
      rankdir = rankdir,
      style = style,
      color = color,
      ser = ser
    )

  private def head(gtype: GraphType, subgraph: Boolean, name: String): String = {
    val prefix = (gtype, subgraph) match {
      case (_, true)    => s"${tab}subgraph"
      case (Graph, _)   => s"graph"
      case (DiGraph, _) => s"digraph"
    }
    if (name == "") s"$prefix {" else s"""$prefix "$name" {"""
  }

  def quote(str: String): String = str match {
    case _ if str.startsWith("\"") => str
    case _                         => s""""$str""""
  }

  def attrMkStr(attr: Map[String, String]): Option[String] =
    if (attr.isEmpty) None
    else
      Some("[" + attr.map(t => t._1 + "=" + t._2).mkString(" ") + "]")

  val tab = "  "
}

class Graphz[F[_]: Monad](gtype: GraphType, t: String, val ser: GraphSerializer[F]) {

  def edge(edg: (String, String)): F[Unit] = edge(edg._1, edg._2)

  def edge(
      src: String,
      dst: String,
      style: Option[GraphStyle] = None,
      arrowHead: Option[GraphArrowType] = None,
      constraint: Option[Boolean] = None
  ): F[Unit] = {
    import Graphz.{showArrowType, showStyle}
    val attrStyle: Map[String, String] = style.map(s => Map("style" -> s.show)).getOrElse(Map.empty)
    val attrArrowHead: Map[String, String] =
      arrowHead.map(s => Map("arrowhead" -> s.show)).getOrElse(Map.empty)
    val attrConstraint: Map[String, String] =
      constraint.map(s => Map("constraint" -> s.show)).getOrElse(Map.empty)
    val attrs: Map[String, String] = attrStyle |+| attrConstraint |+| attrArrowHead
    ser.push(
      edgeMkStr.format(
        Graphz.quote(src),
        Graphz.quote(dst),
        Graphz.attrMkStr(attrs).map(a => " " + a).getOrElse("")
      )
    )
  }

  def node(
      name: String,
      shape: GraphShape = Circle,
      style: Option[GraphStyle] = None,
      color: Option[String] = None,
      border: Option[String] = None,
      borderWidth: Option[Int] = None,
      label: Option[String] = None
  ): F[Unit] = {
    import Graphz.{showShape, showStyle}
    val attrShape: Map[String, String] =
      if (shape == Graphz.DefaultShape) Map.empty else Map("shape" -> shape.show)
    val attrStyle: Map[String, String] = style.map(s => Map("style" -> s.show)).getOrElse(Map.empty)
    val attrColor: Map[String, String] =
      color.map(c => Map("fillcolor" -> s""""$c"""")).getOrElse(Map.empty)
    val attrBorder: Map[String, String] =
      border.map(c => Map("color" -> s""""$c"""")).getOrElse(Map.empty)
    val attrBorderWidth: Map[String, String] =
      borderWidth.map(w => Map("penwidth" -> s"$w")).getOrElse(Map.empty)
    val attrLabel: Map[String, String] = label.map(c => Map("label" -> c)).getOrElse(Map.empty)

    val attrs: Map[String, String] =
      attrShape |+| attrColor |+| attrBorder |+| attrBorderWidth |+| attrLabel |+| attrStyle
    ser.push(t + Graphz.quote(name) + Graphz.attrMkStr(attrs).map(a => " " + a).getOrElse(""))
  }

  def close: F[Unit] = {
    val content = t.substring(Graphz.tab.length)
    val suffix  = if (content.isEmpty) "" else "\n"
    ser.push(s"$content}", suffix = suffix)
  }

  private def edgeMkStr: String = gtype match {
    case Graph   => s"$t%s -- %s%s"
    case DiGraph => s"$t%s -> %s%s"
  }
}
