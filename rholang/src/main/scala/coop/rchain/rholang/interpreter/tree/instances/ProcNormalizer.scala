package coop.rchain.rholang.interpreter.tree.instances

import cats.effect.Sync
import coop.rchain.rholang.ast.rholang_mercury.Absyn._
import coop.rchain.rholang.interpreter.Position
import coop.rchain.rholang.interpreter.compiler.Visit.{ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.interpreter.normalizer.Normalizer
import coop.rchain.rholang.interpreter.tree.{Build, ConnectiveNotBody, New, NilPar, ParTree, Tree}

import scala.annotation.tailrec

trait TreeBuilder {
  implicit def instance: Build[Proc, Tree[Position]] =
    new Build[Proc, Tree[Position]] {
      override def normalize(proc: Proc, parentTree: Option[Tree[Position]]): Tree[Position] =
        proc match {
          case p: PNegation =>
            new ConnectiveNotBody[Position] {
              lazy val children: List[Tree[Position]] = List(normalize(p.proc_, Some(this)))

              val parent: Option[Tree[Position]] = parentTree

              val pos: Position = Position(p.line_num, p.col_num, p.offset)
            }

          case p: PNew =>
            New[Position](Position(p.line_num, p.col_num, p.offset), parentTree,  List(normalize(p.proc_, Some(this))))

          case p: PPar => new ParTree[Position]{
            lazy val children: List[Tree[Position]] = List(normalize(p.proc_1, Some(this)), normalize(p.proc_2, Some(this)))

            val parent: Option[Tree[Position]] = parentTree

            val pos: Position = Position(p.line_num, p.col_num, p.offset)
          }

          case p: PNil => new NilPar[Position]{
            val parent: Option[Tree[Position]] = parentTree

            val pos: Position = Position(p.line_num, p.col_num, p.offset)
          }

          case _ => throw new Exception("Error")
        }
    }
}
