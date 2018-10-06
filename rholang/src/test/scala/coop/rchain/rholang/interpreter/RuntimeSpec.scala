package coop.rchain.rholang.interpreter
import java.nio.file.Files

import coop.rchain.models.Expr.ExprInstance.EVarBody
import coop.rchain.models.Var.VarInstance.{BoundVar, FreeVar}
import coop.rchain.models._
import coop.rchain.rholang.interpreter.Interpreter.EvaluateResult
import coop.rchain.rholang.interpreter.errors.ReduceError
import monix.execution.Scheduler.Implicits.global
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.immutable.BitSet
import scala.concurrent.duration._

class RuntimeSpec extends FlatSpec with Matchers {
  val mapSize     = 10L * 1024L * 1024L
  val tmpPrefix   = "rspace-store-"
  val maxDuration = 5.seconds

  val runtime = Runtime.create(Files.createTempDirectory(tmpPrefix), mapSize)

  val nonReadableErr = ReduceError("Trying to read from non-readable channel.")

  "rho:io:stdout" should "not get intercepted" in {
    // new s(`rho:io:stdout`) in { for (x <- s) { Nil } }
    val interceptNormalized = Par(
      List(),
      List(),
      List(
        New(
          1,
          Par(
            List(),
            List(
              Receive(
                List(
                  ReceiveBind(
                    List(
                      Par(
                        List(),
                        List(),
                        List(),
                        List(Expr(EVarBody(EVar(Var(FreeVar(0)))))),
                        List(),
                        List(),
                        List(),
                        List(),
                        AlwaysEqual(BitSet()),
                        true
                      )
                    ),
                    Par(
                      List(),
                      List(),
                      List(),
                      List(Expr(EVarBody(EVar(Var(BoundVar(0)))))),
                      List(),
                      List(),
                      List(),
                      List(),
                      AlwaysEqual(BitSet(0)),
                      false
                    ),
                    None,
                    1
                  )
                ),
                Par(
                  List(),
                  List(),
                  List(),
                  List(),
                  List(),
                  Vector(),
                  List(),
                  List(),
                  AlwaysEqual(BitSet()),
                  false
                ),
                false,
                1,
                AlwaysEqual(BitSet(0)),
                false
              )
            ),
            List(),
            List(),
            List(),
            Vector(),
            List(),
            List(),
            AlwaysEqual(BitSet(0)),
            false
          ),
          Vector("rho:io:stdout"),
          AlwaysEqual(BitSet())
        )
      ),
      List(),
      List(),
      Vector(),
      List(),
      List(),
      AlwaysEqual(BitSet()),
      false
    )

    failure(interceptNormalized) should contain(nonReadableErr)
  }

  "rho:io:stdoutAck" should "not get intercepted" in {
    // new s(`rho:io:stdoutAck`) in { for (x <- s) { Nil } }
    val interceptNormalized = Par(
      List(),
      List(),
      List(
        New(
          1,
          Par(
            List(),
            List(
              Receive(
                List(
                  ReceiveBind(
                    List(
                      Par(
                        List(),
                        List(),
                        List(),
                        List(Expr(EVarBody(EVar(Var(FreeVar(0)))))),
                        List(),
                        List(),
                        List(),
                        List(),
                        AlwaysEqual(BitSet()),
                        true
                      )
                    ),
                    Par(
                      List(),
                      List(),
                      List(),
                      List(Expr(EVarBody(EVar(Var(BoundVar(0)))))),
                      List(),
                      List(),
                      List(),
                      List(),
                      AlwaysEqual(BitSet(0)),
                      false
                    ),
                    None,
                    1
                  )
                ),
                Par(
                  List(),
                  List(),
                  List(),
                  List(),
                  List(),
                  Vector(),
                  List(),
                  List(),
                  AlwaysEqual(BitSet()),
                  false
                ),
                false,
                1,
                AlwaysEqual(BitSet(0)),
                false
              )
            ),
            List(),
            List(),
            List(),
            Vector(),
            List(),
            List(),
            AlwaysEqual(BitSet(0)),
            false
          ),
          Vector("rho:io:stdoutAck"),
          AlwaysEqual(BitSet())
        )
      ),
      List(),
      List(),
      Vector(),
      List(),
      List(),
      AlwaysEqual(BitSet()),
      false
    )

    failure(interceptNormalized) should contain(nonReadableErr)
  }

  "rho:io:stderr" should "not get intercepted" in {
    // new s(`rho:io:stderr`) in { for(x <- s) { Nil } }
    val interceptNormalized = Par(
      List(),
      List(),
      List(
        New(
          1,
          Par(
            List(),
            List(
              Receive(
                List(
                  ReceiveBind(
                    List(
                      Par(
                        List(),
                        List(),
                        List(),
                        List(Expr(EVarBody(EVar(Var(FreeVar(0)))))),
                        List(),
                        List(),
                        List(),
                        List(),
                        AlwaysEqual(BitSet()),
                        true
                      )
                    ),
                    Par(
                      List(),
                      List(),
                      List(),
                      List(Expr(EVarBody(EVar(Var(BoundVar(0)))))),
                      List(),
                      List(),
                      List(),
                      List(),
                      AlwaysEqual(BitSet(0)),
                      false
                    ),
                    None,
                    1
                  )
                ),
                Par(
                  List(),
                  List(),
                  List(),
                  List(),
                  List(),
                  Vector(),
                  List(),
                  List(),
                  AlwaysEqual(BitSet()),
                  false
                ),
                false,
                1,
                AlwaysEqual(BitSet(0)),
                false
              )
            ),
            List(),
            List(),
            List(),
            Vector(),
            List(),
            List(),
            AlwaysEqual(BitSet(0)),
            false
          ),
          Vector("rho:io:stderr"),
          AlwaysEqual(BitSet())
        )
      ),
      List(),
      List(),
      Vector(),
      List(),
      List(),
      AlwaysEqual(BitSet()),
      false
    )

    failure(interceptNormalized) should contain(nonReadableErr)
  }

  "rho:io:stderrAck" should "not get intercepted" in {
    // new s(`rho:io:stderrAck`) in { for (x <- s) { Nil } }
    val interceptNormalized = Par(
      List(),
      List(),
      List(
        New(
          1,
          Par(
            List(),
            List(
              Receive(
                List(
                  ReceiveBind(
                    List(
                      Par(
                        List(),
                        List(),
                        List(),
                        List(Expr(EVarBody(EVar(Var(FreeVar(0)))))),
                        List(),
                        List(),
                        List(),
                        List(),
                        AlwaysEqual(BitSet()),
                        true
                      )
                    ),
                    Par(
                      List(),
                      List(),
                      List(),
                      List(Expr(EVarBody(EVar(Var(BoundVar(0)))))),
                      List(),
                      List(),
                      List(),
                      List(),
                      AlwaysEqual(BitSet(0)),
                      false
                    ),
                    None,
                    1
                  )
                ),
                Par(
                  List(),
                  List(),
                  List(),
                  List(),
                  List(),
                  Vector(),
                  List(),
                  List(),
                  AlwaysEqual(BitSet()),
                  false
                ),
                false,
                1,
                AlwaysEqual(BitSet(0)),
                false
              )
            ),
            List(),
            List(),
            List(),
            Vector(),
            List(),
            List(),
            AlwaysEqual(BitSet(0)),
            false
          ),
          Vector("rho:io:stderrAck"),
          AlwaysEqual(BitSet())
        )
      ),
      List(),
      List(),
      Vector(),
      List(),
      List(),
      AlwaysEqual(BitSet()),
      false
    )

    failure(interceptNormalized) should contain(nonReadableErr)
  }

  private def failure(normalized: Par): Vector[Throwable] =
    evaluate(normalized).right.get.errors

  private def evaluate(normalized: Par): Either[Throwable, EvaluateResult] =
    Interpreter
      .evaluate(runtime, normalized)
      .attempt
      .runSyncUnsafe(maxDuration)
}
