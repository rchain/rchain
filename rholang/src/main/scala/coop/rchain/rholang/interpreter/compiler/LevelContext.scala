package coop.rchain.rholang.interpreter.compiler

final case class LevelContext[T](level: Int, typ: T, sourcePosition: SourcePosition)
