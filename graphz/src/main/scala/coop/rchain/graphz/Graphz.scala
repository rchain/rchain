package coop.rchain.graphz

import java.io.FileOutputStream

import cats._
import cats.effect.Sync
import cats.effect.concurrent.Ref
import cats.syntax.all._

trait GraphSerializer[F[_]] {
  def push(str: String, suffix: String = "\n"): F[Unit]
}

class StringSerializer[F[_]](val ref: Ref[F, StringBuffer]) extends GraphSerializer[F] {
  override def push(str: String, suffix: String): F[Unit] = ref.modify { current =>
    (current.append(str + suffix), ())
  }
}

class ListSerializer[F[_]](val ref: Ref[F, Vector[String]]) extends GraphSerializer[F] {
  override def push(str: String, suffix: String): F[Unit] = ref.modify { current =>
    (current :+ (str + suffix), ())
  }
}

class FileSerializer[F[_]: Sync](fos: FileOutputStream) extends GraphSerializer[F] {
  def push(str: String, suffix: String): F[Unit] = Sync[F].delay {
    fos.write(str.getBytes)
    fos.flush()
  }
}

sealed trait GraphType
final case object Graph   extends GraphType
final case object DiGraph extends GraphType

sealed trait GraphShape
final case object Circle       extends GraphShape
final case object DoubleCircle extends GraphShape
final case object Box          extends GraphShape
final case object PlainText    extends GraphShape
final case object Msquare      extends GraphShape
final case object Record       extends GraphShape

sealed trait GraphRank
final case object Same   extends GraphRank
final case object Min    extends GraphRank
final case object Source extends GraphRank
final case object Max    extends GraphRank
final case object Sink   extends GraphRank

sealed trait GraphRankDir
final case object TB extends GraphRankDir
final case object BT extends GraphRankDir
final case object LR extends GraphRankDir
final case object RL extends GraphRankDir

sealed trait GraphStyle
final case object Solid  extends GraphStyle
final case object Bold   extends GraphStyle
final case object Filled extends GraphStyle
final case object Invis  extends GraphStyle
final case object Dotted extends GraphStyle
final case object Dashed extends GraphStyle

sealed trait GraphArrowType
final case object NormalArrow extends GraphArrowType
final case object InvArrow    extends GraphArrowType
final case object NoneArrow   extends GraphArrowType

object Graphz {

  implicit val showShape: Show[GraphShape] = new Show[GraphShape] {
    def show(shape: GraphShape): String = shape match {
      case Circle       => "circle"
      case DoubleCircle => "doublecircle"
      case Box          => "box"
      case PlainText    => "plaintext"
      case Msquare      => "Msquare"
      case Record       => "record"
    }
  }

  def smallToString[A]: Show[A] = new Show[A] {
    def show(a: A): String = a.toString.toLowerCase
  }

  implicit val showStyle: Show[GraphStyle]     = smallToString[GraphStyle]
  implicit val showRank: Show[GraphRank]       = smallToString[GraphRank]
  implicit val showRankDir: Show[GraphRankDir] = Show.fromToString[GraphRankDir]
  implicit val showArrowType: Show[GraphArrowType] = new Show[GraphArrowType] {
    def show(arrowType: GraphArrowType): String = arrowType match {
      case NormalArrow => "normal"
      case InvArrow    => "inv"
      case NoneArrow   => "none"
    }
  }

  def DefaultShape = Circle

  def apply[F[_]: Monad](
      name: String,
      gtype: GraphType,
      subgraph: Boolean = false,
      comment: Option[String] = None,
      label: Option[String] = None,
      splines: Option[String] = None,
      rank: Option[GraphRank] = None,
      rankdir: Option[GraphRankDir] = None,
      style: Option[String] = None,
      color: Option[String] = None,
      node: Map[String, String] = Map.empty
  )(
      ser: GraphSerializer[F]
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
      _ <- insert(attrMkStr(node), n => s"node $n")
      _ <- insert(splines.map(_.show), s => s"splines=$s")
    } yield new Graphz[F](gtype, t)(ser)
  }

  def subgraph[F[_]: Monad](
      name: String,
      gtype: GraphType,
      label: Option[String] = None,
      rank: Option[GraphRank] = None,
      rankdir: Option[GraphRankDir] = None,
      style: Option[String] = None,
      color: Option[String] = None
  )(ser: GraphSerializer[F]): F[Graphz[F]] =
    apply[F](
      name,
      gtype,
      subgraph = true,
      label = label,
      rank = rank,
      rankdir = rankdir,
      style = style,
      color = color
    )(ser)

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

class Graphz[F[_]: Monad](gtype: GraphType, t: String)(ser: GraphSerializer[F]) {

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
      label: Option[String] = None
  ): F[Unit] = {
    import Graphz.{showShape, showStyle}
    val attrShape: Map[String, String] =
      if (shape == Graphz.DefaultShape) Map.empty else Map("shape" -> shape.show)
    val attrStyle: Map[String, String] = style.map(s => Map("style" -> s.show)).getOrElse(Map.empty)
    val attrColor: Map[String, String] = color.map(c => Map("color" -> c)).getOrElse(Map.empty)
    val attrLabel: Map[String, String] = label.map(c => Map("label" -> c)).getOrElse(Map.empty)

    val attrs: Map[String, String] = attrShape |+| attrColor |+| attrLabel |+| attrStyle
    ser.push(t + Graphz.quote(name) + Graphz.attrMkStr(attrs).map(a => " " + a).getOrElse(""))
  }

  def subgraph(sub: F[Graphz[F]]): F[Unit] = sub >>= (_ => ser.push(""))
  def close: F[Unit]                       = ser.push(s"${t.substring(Graphz.tab.length)}}", suffix = "")

  private def edgeMkStr: String = gtype match {
    case Graph   => s"$t%s -- %s%s"
    case DiGraph => s"$t%s -> %s%s"
  }
}
