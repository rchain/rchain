package coop.rchain.rholang.interpreter.compiler.normalizer.processes

import cats.effect.Sync
import coop.rchain.models.Par
import coop.rchain.rholang.interpreter.compiler.ProcNormalizeMatcher.normalizeMatch
import coop.rchain.rholang.interpreter.compiler.{ProcVisitInputs, ProcVisitOutputs}
import coop.rchain.rholang.ast.rholang_mercury.Absyn._

import java.util.UUID
import scala.collection.convert.ImplicitConversionsToScala._
object PSendSynchNormalizer {
  @SuppressWarnings(Array("org.wartremover.warts.NonUnitStatements"))
  def normalize[F[_]: Sync](p: PSendSynch, input: ProcVisitInputs)(
      implicit env: Map[String, Par]
  ): F[ProcVisitOutputs] = {
    val identifier = UUID.randomUUID().toString
    val nameVar    = new NameVar(identifier)

    val send: PSend = {
      p.listproc_.prepend(new PEval(nameVar))
      new PSend(p.name_, new SendSingle(), p.listproc_)
    }

    val receive: PInput = {

      val listName = new ListName()
      listName.add(new NameWildcard)

      val listLinearBind = new ListLinearBind()
      listLinearBind.add(
        new LinearBindImpl(listName, new NameRemainderEmpty, new SimpleSource(nameVar))
      )

      val listReceipt = new ListReceipt()
      listReceipt.add(new ReceiptLinear(new LinearSimple(listLinearBind)))

      new PInput(
        listReceipt,
        p.synchsendcont_ match {
          case _: EmptyCont               => new PNil()
          case nonEmptyCont: NonEmptyCont => nonEmptyCont.proc_
        }
      )
    }

    val listName = new ListNameDecl()
    listName.add(new NameDeclSimpl(identifier))
    normalizeMatch[F](new PNew(listName, new PPar(send, receive)), input)

  }

}
