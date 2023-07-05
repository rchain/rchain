package coop.rchain.models.rholangN

import coop.rchain.models.Par

object Bindings {
  def toProto(p: ParN): Par   = BindingsToProto.toProto(p)
  def fromProto(p: Par): ParN = BindingsFromProto.fromProto(p)
}
