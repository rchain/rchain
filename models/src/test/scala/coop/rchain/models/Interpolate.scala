package coop.rchain.models

import coop.rchain.models.rholang.implicits._

trait Interpolate[A, B] {
  def interpolate(term: A, interpolateMap: Map[String, B]): A
}

object Interpolate extends InterpolateInstances {
  def apply[A, B](implicit ev: Interpolate[A, B]): Interpolate[A, B] = ev
  def interpolate[A, B](term: A, interpolateMap: Map[String, B])(
      implicit ev: Interpolate[A, B]
  ): A = apply[A, B].interpolate(term, interpolateMap)

  def interpolateSeq[A, B](as: Seq[A], interpolateMap: Map[String, B])(
      implicit ev: Interpolate[A, B]
  ): Seq[A] =
    as.map(ev.interpolate(_, interpolateMap))
}

trait InterpolateInstances {
  private def replaceSingleExpr(par: Par, interpolateMap: Map[String, Par]): Par =
    par.singleExpr
      .flatMap {
        case Expr(Expr.ExprInstance.GString(key)) if key.startsWith("#") =>
          interpolateMap.getOrElse(
            key,
            throw new IllegalArgumentException(s"Key $key was not found in the interpolate map.")
          )
        case _ => None
      }
      .getOrElse(par)

  implicit val sendInterpolate: Interpolate[Send, Par] = new Interpolate[Send, Par] {
    override def interpolate(term: Send, interpolateMap: Map[String, Par]): Send = {
      val newChan = replaceSingleExpr(term.chan, interpolateMap)
      term.withChan(newChan)
    }
  }

  implicit val receiveBindInterpolate: Interpolate[ReceiveBind, Par] =
    new Interpolate[ReceiveBind, Par] {
      override def interpolate(
          term: ReceiveBind,
          interpolateMap: Map[String, Par]
      ): ReceiveBind = {
        val newSource = replaceSingleExpr(term.source, interpolateMap)
        term.withSource(newSource)
      }
    }

  implicit val receiveInterpolate: Interpolate[Receive, Par] =
    new Interpolate[Receive, Par] {
      override def interpolate(term: Receive, interpolateMap: Map[String, Par]): Receive = {
        val newBinds = Interpolate.interpolateSeq(term.binds, interpolateMap)
        val newBody  = Interpolate.interpolate(term.body, interpolateMap)
        term.withBinds(newBinds).withBody(newBody)
      }
    }

  implicit val newInterpolate: Interpolate[New, Par] = new Interpolate[New, Par] {
    override def interpolate(term: New, interpolateMap: Map[String, Par]): New =
      term.withP(Interpolate.interpolate(term.p, interpolateMap))
  }

  implicit val parInterpolate: Interpolate[Par, Par] = new Interpolate[Par, Par] {
    override def interpolate(term: Par, interpolateMap: Map[String, Par]): Par =
      term
        .withSends(Interpolate.interpolateSeq(term.sends, interpolateMap))
        .withReceives(Interpolate.interpolateSeq(term.receives, interpolateMap))
        .withNews(Interpolate.interpolateSeq(term.news, interpolateMap))
  }
}
