package coop.rchain.casper.util.rholang.costacc

import coop.rchain.casper.util.rholang.{FailedSystemDeploy, SystemDeploy}
import coop.rchain.rholang.interpreter.RhoType._

object PreCharge extends SystemDeploy("prechargeDeploy") {
  object Success

  type Output = (RhoBoolean, RhoString)
  type Result = Success.type

  override val code: String = """
                                |new rl(`rho:registry:lookup`),
                                |poSCh,
                                |initialDeployerId(`sys:casper:initialDeployerId`),
                                |chargeAmount(`sys:casper:chargeAmount`),
                                |return(`sys:casper:return`) in
                                |{
                                |  rl!(`rho:rchain:pos`, *poSCh) |
                                |  for(@(_, PoS) <- poSCh) {
                                |    @PoS!("chargeDeploy", *initialDeployerId, *chargeAmount, *return)
                                |  }
                                |
                                |}""".stripMargin

  protected override val extractor = Extractor.derive
  protected def result(value: (Boolean, String)): Either[FailedSystemDeploy, Result] =
    value match {
      case (true, _)     => Right(Success)
      case (_, errorMsg) => Left(deployError(errorMsg))
    }

}
