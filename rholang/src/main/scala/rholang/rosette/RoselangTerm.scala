/**
  * Term classes specifically used in Rholang to Rosette Base Language source to source compiler. *
  */

package coop.rchain.rho2rose

import coop.rchain.lib.term._

trait RosetteSerialization[Namespace, VarType, TagType] {
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

case class StrTermPtdCtxtLf(override val tag: TagOrVar[String, String])
  extends TermCtxtLeaf[String, String, String](tag) with RosetteSerialization[String, String, String] {
  override def rosetteSerialize: String = {
    tag match {
      case Tag(t) => "" + t
      case Var(v) => "" + v
    }
  }
}

case class StrTermPtdCtxtBr(override val nameSpace: String,
                            override val labels: List[TermCtxt[String, String, String] with RosetteSerialization[String, String, String]]
                           ) extends TermCtxtBranch[String, String, String](nameSpace, labels) with RosetteSerialization[String, String, String] {
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


          def serializeExpr(lbl: TermCtxt[String, String, String] with RosetteSerialization[String, String, String], i: Int) = {
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
    if (nameSpace.toString.contentEquals("list")) {
      "[" + lblStr + "]"
    } else if (nameSpace.toString.contentEquals("Q")) {
      "'" + lblStr
    } else {
      "(" + nameSpace + " " + lblStr + ")"
    }
  }
}
