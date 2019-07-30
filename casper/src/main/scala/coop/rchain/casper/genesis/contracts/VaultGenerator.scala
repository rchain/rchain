package coop.rchain.casper.genesis.contracts

import cats.implicits._

import scala.io.Source

final case class VaultGenerator(genesisVaults: Seq[Vault])
    extends SourceCode(Source.fromResource("RevVault.rhox").mkString) {

  private val vaultNames: Seq[String] = (1 to genesisVaults.size).map(i => s"revVault$i")

  private val (revAddresses, initialBalances): (Seq[String], Seq[String]) = genesisVaults.map {
    case Vault(revAddress, initialBalance) =>
      (s""""${revAddress.toBase58}"""", initialBalance.toString)
  }.unzip

  private val vaultMap: String = revAddresses
    .zip(vaultNames)
    .map {
      case (revAddress, vaultName) => s"$revAddress:*$vaultName"
    }
    .mkString("{", ", ", "}")

  private val vaultProcs: Seq[String] = revAddresses
    .zip(vaultNames)
    .zip(initialBalances)
    .map {
      case ((revAddress, vaultName), initialBalance) =>
        s""" new purseCh in {
         #     mint!("makePurse", $initialBalance, *purseCh) |
         #     for(purse <- purseCh){
         #
         #       contract $vaultName(@"balance", ret) = {
         #         purse!("getBalance", *ret)
         #       } |
         #
         #       contract $vaultName(@"transfer", @revAddress, @amount, authKey, ret) = {
         #         new revAddressValid,
         #             revAddressValidEither,
         #             amountNonNegative,
         #             authKeyValidCh,
         #             authKeyValidEitherCh,
         #             parametersOkCh,
         #             parametersAndAuthOkCh,
         #             split,
         #             eitherPurseCh,
         #             doDeposit in {
         #
         #         RevAddress!("validate", revAddress, *revAddressValid) |
         #         @Either!("fromNillableError <-", *revAddressValid, *revAddressValidEither) |
         #         @Either!("fromBoolean", amount >= 0, "Amount must be non-negative", *amountNonNegative) |
         #         @AuthKey!("check", *authKey, (*_revVault, $revAddress), *authKeyValidCh) |
         #         @Either!("fromBoolean <-", *authKeyValidCh, "Invalid AuthKey", *authKeyValidEitherCh) |
         #         @Either!("productR <-", *revAddressValidEither, *amountNonNegative, *parametersOkCh) |
         #         @Either!("productR <-", *parametersOkCh, *authKeyValidEitherCh, *parametersAndAuthOkCh) |
         #         @Either!("flatMap <-", *parametersAndAuthOkCh, *split, *eitherPurseCh) |
         #         for (_, retCh <- split) {
         #           new amountPurseCh in {
         #             purse!("split", amount, *amountPurseCh) |
         #             @Either!("fromSingletonList <-", *amountPurseCh, "Insufficient funds", *retCh)
         #           }
         #         } |
         #
         #         @Either!("flatMap <-", *eitherPurseCh, *doDeposit, *ret) |
         #         for (@p, retCh <- doDeposit) {
         #           @{revAddress | bundle0{*_revVault}}!("_deposit", p, *retCh)
         #         }
         #       }
         #     } |
         #
         #       contract @{$revAddress | bundle0{*_revVault}}(@"_deposit", depositPurse, retCh) = {
         #         new amountCh, depositSuccessCh in {
         #           depositPurse!("getBalance", *amountCh) |
         #           for (@amount <- amountCh) {
         #             purse!("deposit", amount, *depositPurse, *depositSuccessCh) |
         #             @Either!("fromBoolean <-", *depositSuccessCh, "BUG FOUND: purse deposit failed", *retCh)
         #           }
         #         }
         #       }
         #   }
         # }
       """.stripMargin('#')
    }

  private val vaultConstructorProc: String =
    if (genesisVaults.isEmpty) {
      "vaultMapStore!({}) |"
    } else {
      s""" new ${vaultNames.mkString(", ")} in {
         #   vaultMapStore!($vaultMap) |
         #   ${if (vaultProcs.isEmpty) "Nil" else vaultProcs.mkString("|")}
         # } |
       """.stripMargin('#')
    }

  val optionalSubstitutions: Option[Seq[(String, String)]] = Seq(
    "vaultConstructorProc" -> vaultConstructorProc
  ).some
}
