package coop.rchain.rholang.interpreter.tree

trait Tree[P] {
  def parent: Option[Tree[P]]
  def children: List[Tree[P]]

  def pos: P
}

trait Leaf[P] extends Tree[P] {
  override def children: List[Tree[P]] = List.empty
}

trait ParTree[P] extends Tree[P]

trait NilPar[P] extends ParTree[P] with Leaf[P]

case class Send[P](pos: P, parent: Option[ParTree[P]], children: List[ParTree[P]])
    extends ParTree[P] {}
case class Receive[P](pos: P, parent: Option[ParTree[P]], children: List[ParTree[P]])
    extends ParTree[P] {}
case class New[P](pos: P, parent: Option[ParTree[P]], children: List[ParTree[P]]) extends ParTree[P]

trait Expr[P]       extends ParTree[P] {}
trait Literal[P]    extends Expr[P] with Leaf[P]
trait Operation[P]  extends Expr[P]
trait Collection[P] extends Expr[P]
trait Dot[P]        extends Expr[P]
trait Matches[P]    extends Expr[P]

case class Match[P](pos: P, parent: Option[ParTree[P]], children: List[ParTree[P]])
    extends ParTree[P] {}

trait Unforgeable[P]                                           extends ParTree[P] with Leaf[P]
case class Private[P](pos: P, parent: Option[ParTree[P]])      extends Unforgeable[P]
case class DeployId[P](pos: P, parent: Option[ParTree[P]])     extends Unforgeable[P]
case class DeployerId[P](pos: P, parent: Option[ParTree[P]])   extends Unforgeable[P]
case class SysAuthToken[P](pos: P, parent: Option[ParTree[P]]) extends Unforgeable[P]

trait Bundle[P] extends ParTree[P] {}

trait Connective[P] extends ParTree[P]
case class ConnectiveAndBody[P](pos: P, parent: Option[Tree[P]], children: List[Tree[P]])
    extends Connective[P]
case class ConnectiveOrBody[P](pos: P, parent: Option[Tree[P]], children: List[Tree[P]])
    extends Connective[P]
trait ConnectiveNotBody[P] extends Connective[P]
case class VarRefBody[P](pos: P, parent: Option[Tree[P]], children: List[Tree[P]])
    extends Connective[P]
case class ConnectiveBool[P](pos: P, parent: Option[Tree[P]], children: List[Tree[P]])
    extends Connective[P]
case class ConnectiveInt[P](pos: P, parent: Option[Tree[P]], children: List[Tree[P]])
    extends Connective[P]
case class ConnectiveString[P](pos: P, parent: Option[Tree[P]], children: List[Tree[P]])
    extends Connective[P]
case class ConnectiveURI[P](pos: P, parent: Option[Tree[P]], children: List[Tree[P]])
    extends Connective[P]
case class ConnectiveByteArray[P](pos: P, parent: Option[Tree[P]], children: List[Tree[P]])
    extends Connective[P]

object Tree {}
