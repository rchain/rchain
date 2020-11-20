package coop.rchain.rholang.interpreter.compiler

final case class IndexContext[T](index: Int, typ: T, sourcePosition: SourcePosition)
