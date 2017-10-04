/**
  * Term classes specifically used in Rholang to Rosette Base Language source to source compiler. *
  */

package coop.rchain.rho2rose

import coop.rchain.lib.term._

trait RosetteSerialization[Namespace, Var, Tag] {
  def rosetteSerializeOperation: String = {
    this match {
      case leaf: StrTermPtdCtxtLf => ""
      case branch: StrTermPtdCtxtBr => branch.rosetteSerializeOperation
      case _ => throw new Exception("unexpected CCL type")
    }
  }

  def rosetteSerialize: String = {
    this match {
      case leaf: StrTermPtdCtxtLf =>
        leaf.rosetteSerialize
      case branch: StrTermPtdCtxtBr =>
        branch.rosetteSerialize
      case _ => throw new Exception("unexpected CCL type")
    }
  }
}

case class StrTermPtdCtxtLf(override val tag: Either[String, Either[String, String]])
  extends TermCtxtLeaf[String, Either[String, String], String](tag) with Factual with RosetteSerialization[String, Either[String, String], String] {
  override def rosetteSerialize: String = {
    tag match {
      case Left(t) => "" + t
      case Right(v) => {
        v match {
          case Left(language_v) => "" + language_v
          case _ => "" + v
        }
      }
    }
  }
}

case class StrTermPtdCtxtBr(override val nameSpace: String,
                            override val labels: List[TermCtxt[String, Either[String, String], String] with Factual with RosetteSerialization[String, Either[String, String], String]]
                           ) extends TermCtxtBranch[String, Either[String, String], String](nameSpace, labels) with Factual with RosetteSerialization[String, Either[String, String], String] {
  override def rosetteSerializeOperation: String = {
    val result = labels match {
      case (albl: StrTermPtdCtxtBr) :: rlbls => {
        val preSeed = albl.nameSpace
        val seed = if (nameSpace.toString.contentEquals("method")) {
          "(defOprn " + preSeed + ")\n"
        } else {
          ""
        }
        (seed /: rlbls) (
          {
            (acc, lbl) => {
              acc + lbl.rosetteSerializeOperation
            }
          }
        )
      }
      case _ :: rlbls => {
        ("" /: rlbls) (
          {
            (acc, lbl) => {
              acc + lbl.rosetteSerializeOperation
            }
          }
        )
      }
      case _ => ""
    }
    result
  }

  override def rosetteSerialize: String = {
    val lblStr =
      labels match {
        case _ :: _ => {
          var acc = ""


          def serializeExpr(lbl: TermCtxt[String, Either[String, String], String] with Factual with RosetteSerialization[String, Either[String, String], String], i: Int) = {
            if (i == 0) {
              acc = lbl.rosetteSerialize
            } else {
              acc = acc + " " + lbl.rosetteSerialize
            }
          }

          for ((lbl, i) <- labels.zipWithIndex) {
            serializeExpr(lbl, i)
          }
          acc
        }
        case Nil => ""
      }
    "(" + nameSpace + " " + lblStr + ")"
  }
}
