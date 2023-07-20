package coop.rchain.models.rholangN

import coop.rchain.models.{Par, Var}

object Bindings {
  def toProto(p: ParN): Par                          = BindingsToProto.toProto(p)
  def toProto(ps: Seq[ParN]): Seq[Par]               = ps.map(toProto)
  def toProto(pOpt: Option[ParN]): Option[Par]       = pOpt.map(toProto)
  def toProtoVarOpt(pOpt: Option[VarN]): Option[Var] = pOpt.map(BindingsToProto.toVar)

  def fromProto(p: Par): ParN                          = BindingsFromProto.fromProto(p)
  def fromProto(ps: Seq[Par]): Seq[ParN]               = ps.map(fromProto)
  def fromProto(pOpt: Option[Par]): Option[ParN]       = pOpt.map(fromProto)
  def fromProtoVarOpt(pOpt: Option[Var]): Option[VarN] = pOpt.map(BindingsFromProto.fromVar)
}
