package coop.rchain.node.revvaultexport

import coop.rchain.node.revvaultexport.reporting.Transaction
import coop.rchain.node.revvaultexport.reporting.Transaction.{
  deployInfoJ,
  transactionValueCodec,
  TransactionInfo
}
import io.circe.generic.auto._
import io.circe.syntax._
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.BitVector

/**
  * This is just some tests to be ensure scala code json encoder and decoder are fully compatible with Python version.
  * https://github.com/zsluedem/transaction-server/blob/master/rserver/models.py
  *
  * This is not a important part. This should be removed when first hard fork is done.
  */
class TransactionParserTest extends FlatSpec with Matchers {
  "transactionInfo json parser" should "simple json object" in {
    val rawJson =
      """[[{"fromAddr": "1111vuKThzapwgwB5rwikNKhBCdk7EvDLSnRc2UuTJaMNHjVNoC1C",
        |"toAddr": "1111fTFCBE727Ex5AHDhAD38HyNca66U5vKVCoQDLwauVCY9DDbBX",
        |"amount": 3999999837677,
        |"retUnforeable": "OiQKIgogBjk1viZEkQSFBfaavQBYfKYOdZvshbvY0WteEjqeGcQ=\n",
        |"deploy":
        | {"deployer": "049d3c4ac59b5436929c79ad1b39aa4fedee213fdaf8eed3f63047886f8b09d3d536a8ed6f7ed77162c7c4f4a6bb7edb7fdd00ace530245445bd1d9f9801ad8d0b",
        | "term": "\n  new rl(`rho:registry:lookup`), RevVaultCh, vaultCh, toVaultCh, deployerId(`rho:rchain:deployerId`), revVaultKeyCh, resultCh in {\n    rl!(`rho:rchain:revVault`, *RevVaultCh) |\n    for (@(_, RevVault) <- RevVaultCh) {\n      @RevVault!(\"findOrCreate\", \"1111vuKThzapwgwB5rwikNKhBCdk7EvDLSnRc2UuTJaMNHjVNoC1C\", *vaultCh) |\n      @RevVault!(\"findOrCreate\", \"1111fTFCBE727Ex5AHDhAD38HyNca66U5vKVCoQDLwauVCY9DDbBX\", *toVaultCh) |\n      @RevVault!(\"deployerAuthKey\", *deployerId, *revVaultKeyCh) |\n      for (@(true, vault) <- vaultCh; key <- revVaultKeyCh; @(true, toVault) <- toVaultCh) {\n        @vault!(\"transfer\", \"1111fTFCBE727Ex5AHDhAD38HyNca66U5vKVCoQDLwauVCY9DDbBX\", 3999999837677, *key, *resultCh) |\n        for (_ <- resultCh) { Nil }\n      }\n    }\n  }\n  ",
        | "timestamp": 1589130245375,
        | "sig": "3045022100fe9a9ee7de17ca865d080db90958a495b4998e4fa07f8b6123fb37601caf0cea02201e899175861966bc515e3b801f6b70b9ae0dc8b99a98630c011da6805e0a2ec4",
        |  "sigAlgorithm": "secp256k1",
        |  "phloPrice": 1,
        |  "phloLimit": 300000,
        |  "validAfterBlockNumber": 149348,
        |  "cost": 162323,
        |  "errored": false,
        |  "systemDeployError": ""},
        |  "success": true,
        |  "reason": ""}]]""".stripMargin
    val data            = Transaction.parseTransactionInfoJson(rawJson)
    val transactionInfo = data.right.get.head.head
    transactionInfo.fromAddr should equal("1111vuKThzapwgwB5rwikNKhBCdk7EvDLSnRc2UuTJaMNHjVNoC1C")
    transactionInfo.toAddr should equal("1111fTFCBE727Ex5AHDhAD38HyNca66U5vKVCoQDLwauVCY9DDbBX")
    transactionInfo.success should equal(true)
    transactionInfo.amount should equal(3999999837677L)
    transactionInfo.reason should equal("")
    transactionInfo.retUnforeable should equal(
      "OiQKIgogBjk1viZEkQSFBfaavQBYfKYOdZvshbvY0WteEjqeGcQ=\n"
    )
    transactionInfo.deploy.errored should equal(false)
    transactionInfo.deploy.cost should equal(162323L)
    transactionInfo.deploy.deployer should equal(
      "049d3c4ac59b5436929c79ad1b39aa4fedee213fdaf8eed3f63047886f8b09d3d536a8ed6f7ed77162c7c4f4a6bb7edb7fdd00ace530245445bd1d9f9801ad8d0b"
    )
    transactionInfo.deploy.timestamp should equal(1589130245375L)
    transactionInfo.deploy.sig should equal(
      "3045022100fe9a9ee7de17ca865d080db90958a495b4998e4fa07f8b6123fb37601caf0cea02201e899175861966bc515e3b801f6b70b9ae0dc8b99a98630c011da6805e0a2ec4"
    )
    transactionInfo.deploy.sigAlgorithm should equal("secp256k1")
    transactionInfo.deploy.phloPrice should equal(1L)
    transactionInfo.deploy.phloLimit should equal(300000L)
    transactionInfo.deploy.validAfterBlockNumber should equal(149348L)
    transactionInfo.deploy.systemDeployError should equal("")
    data.right.get.length should equal(1)
    data.right.get.head.length should equal(1)
  }

  "transactionInfo json parser" should "embedded json object" in {
    val rawJson =
      """[[{"deploy": {"deployer": "044f28eacd466e3a44d5b8e246a10afbfe7970dd0266e9b4378d1ef58e2461d856e1d9f3e1ade80dfab9fe7fcbfdb6c09bb5ea964d0c67137e1b3a310b79f0fe04", "term": "\nnew rl(`rho:registry:lookup`), RevVaultCh in {\nrl!(`rho:rchain:revVault`, *RevVaultCh) |\nfor (@(_, RevVault) <- RevVaultCh) {\nnew vaultCh, vaultTo, revVaultkeyCh,\ndeployerId(`rho:rchain:deployerId`),\ndeployId(`rho:rchain:deployId`)\nin {\nmatch (\"11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns\", \"11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns\", 1) {\n(revAddrFrom, revAddrTo, amount) => {\n@RevVault!(\"findOrCreate\", revAddrFrom, *vaultCh) |\n@RevVault!(\"findOrCreate\", revAddrTo, *vaultTo) |\n@RevVault!(\"deployerAuthKey\", *deployerId, *revVaultkeyCh) |\nfor (@vault <- vaultCh; key <- revVaultkeyCh; _ <- vaultTo) {\nmatch vault {\n(true, vault) => {\nnew resultCh in {\n@vault!(\"transfer\", revAddrTo, amount, *key, *resultCh) |\nfor (@result <- resultCh) {\nmatch result {\n(true , _  ) => deployId!((true, \"Transfer successful (not yet finalized).\"))\n(false, err) => deployId!((false, err))\n}\n}\n}\n}\nerr => {\ndeployId!((false, \"REV vault cannot be found or created.\"))\n}\n}\n}\n}\n}\n}\n}\n}\n", "timestamp": 1616104676051, "sig": "52f4d1138f4626bb5043f9acf24ececbd9e9f663b29764d95052a254742ba3072923f1efc370c4f988795cb172eb8a14bba1e71de393d9f94281d4271fd378a61b", "sigAlgorithm": "secp256k1:eth", "phloPrice": 1, "phloLimit": 250000, "validAfterBlockNumber": 664485, "cost": 164532, "errored": false, "systemDeployError": ""}, "fromAddr": "11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns", "toAddr": "11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns", "amount": 1, "retUnforeable": "OiQKIgoghjQH8BapKR1GoK7c36Xbw/NscMvfjNRuXFucZH+ENn4=\n", "success": true, "reason": ""}], [{"deploy": {"deployer": "044f28eacd466e3a44d5b8e246a10afbfe7970dd0266e9b4378d1ef58e2461d856e1d9f3e1ade80dfab9fe7fcbfdb6c09bb5ea964d0c67137e1b3a310b79f0fe04", "term": "\nnew rl(`rho:registry:lookup`), RevVaultCh in {\nrl!(`rho:rchain:revVault`, *RevVaultCh) |\nfor (@(_, RevVault) <- RevVaultCh) {\nnew vaultCh, vaultTo, revVaultkeyCh,\ndeployerId(`rho:rchain:deployerId`),\ndeployId(`rho:rchain:deployId`)\nin {\nmatch (\"11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns\", \"11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns\", 1) {\n(revAddrFrom, revAddrTo, amount) => {\n@RevVault!(\"findOrCreate\", revAddrFrom, *vaultCh) |\n@RevVault!(\"findOrCreate\", revAddrTo, *vaultTo) |\n@RevVault!(\"deployerAuthKey\", *deployerId, *revVaultkeyCh) |\nfor (@vault <- vaultCh; key <- revVaultkeyCh; _ <- vaultTo) {\nmatch vault {\n(true, vault) => {\nnew resultCh in {\n@vault!(\"transfer\", revAddrTo, amount, *key, *resultCh) |\nfor (@result <- resultCh) {\nmatch result {\n(true , _  ) => deployId!((true, \"Transfer successful (not yet finalized).\"))\n(false, err) => deployId!((false, err))\n}\n}\n}\n}\nerr => {\ndeployId!((false, \"REV vault cannot be found or created.\"))\n}\n}\n}\n}\n}\n}\n}\n}\n", "timestamp": 1616104672728, "sig": "9fca3071525e18cb169037d056c4dee62ac31ea7d5e4096416f240a6a52792d737905a264d9f1ee2a9175d52ab3233204904c9d509668ad4d45659254583925a1c", "sigAlgorithm": "secp256k1:eth", "phloPrice": 1, "phloLimit": 250000, "validAfterBlockNumber": 664485, "cost": 164532, "errored": false, "systemDeployError": ""}, "fromAddr": "11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns", "toAddr": "11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns", "amount": 1, "retUnforeable": "OiQKIgogRzlriWrxOpLmNXqdwVhcjm2z7u3z+3rZGqpNjWa/+3M=\n", "success": true, "reason": ""}], [{"deploy": {"deployer": "044f28eacd466e3a44d5b8e246a10afbfe7970dd0266e9b4378d1ef58e2461d856e1d9f3e1ade80dfab9fe7fcbfdb6c09bb5ea964d0c67137e1b3a310b79f0fe04", "term": "\nnew rl(`rho:registry:lookup`), RevVaultCh in {\nrl!(`rho:rchain:revVault`, *RevVaultCh) |\nfor (@(_, RevVault) <- RevVaultCh) {\nnew vaultCh, vaultTo, revVaultkeyCh,\ndeployerId(`rho:rchain:deployerId`),\ndeployId(`rho:rchain:deployId`)\nin {\nmatch (\"11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns\", \"11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns\", 1) {\n(revAddrFrom, revAddrTo, amount) => {\n@RevVault!(\"findOrCreate\", revAddrFrom, *vaultCh) |\n@RevVault!(\"findOrCreate\", revAddrTo, *vaultTo) |\n@RevVault!(\"deployerAuthKey\", *deployerId, *revVaultkeyCh) |\nfor (@vault <- vaultCh; key <- revVaultkeyCh; _ <- vaultTo) {\nmatch vault {\n(true, vault) => {\nnew resultCh in {\n@vault!(\"transfer\", revAddrTo, amount, *key, *resultCh) |\nfor (@result <- resultCh) {\nmatch result {\n(true , _  ) => deployId!((true, \"Transfer successful (not yet finalized).\"))\n(false, err) => deployId!((false, err))\n}\n}\n}\n}\nerr => {\ndeployId!((false, \"REV vault cannot be found or created.\"))\n}\n}\n}\n}\n}\n}\n}\n}\n", "timestamp": 1616104670906, "sig": "a7de019ae061a60a63e6f3b7d4cfc56ebaa81293db69615cd9d8933b4dbe9c4b4b5a7ac8f8e03ee6925dd5e996c0daa6bae33af875eb73b33ed7950095c4eded1c", "sigAlgorithm": "secp256k1:eth", "phloPrice": 1, "phloLimit": 250000, "validAfterBlockNumber": 664485, "cost": 164532, "errored": false, "systemDeployError": ""}, "fromAddr": "11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns", "toAddr": "11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns", "amount": 1, "retUnforeable": "OiQKIgogsXwzWDgL58Fwsrn0wzl5C8LXm1e4SlBGKR766Q517+s=\n", "success": true, "reason": ""}], [{"deploy": {"deployer": "044f28eacd466e3a44d5b8e246a10afbfe7970dd0266e9b4378d1ef58e2461d856e1d9f3e1ade80dfab9fe7fcbfdb6c09bb5ea964d0c67137e1b3a310b79f0fe04", "term": "\nnew rl(`rho:registry:lookup`), RevVaultCh in {\nrl!(`rho:rchain:revVault`, *RevVaultCh) |\nfor (@(_, RevVault) <- RevVaultCh) {\nnew vaultCh, vaultTo, revVaultkeyCh,\ndeployerId(`rho:rchain:deployerId`),\ndeployId(`rho:rchain:deployId`)\nin {\nmatch (\"11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns\", \"11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns\", 1) {\n(revAddrFrom, revAddrTo, amount) => {\n@RevVault!(\"findOrCreate\", revAddrFrom, *vaultCh) |\n@RevVault!(\"findOrCreate\", revAddrTo, *vaultTo) |\n@RevVault!(\"deployerAuthKey\", *deployerId, *revVaultkeyCh) |\nfor (@vault <- vaultCh; key <- revVaultkeyCh; _ <- vaultTo) {\nmatch vault {\n(true, vault) => {\nnew resultCh in {\n@vault!(\"transfer\", revAddrTo, amount, *key, *resultCh) |\nfor (@result <- resultCh) {\nmatch result {\n(true , _  ) => deployId!((true, \"Transfer successful (not yet finalized).\"))\n(false, err) => deployId!((false, err))\n}\n}\n}\n}\nerr => {\ndeployId!((false, \"REV vault cannot be found or created.\"))\n}\n}\n}\n}\n}\n}\n}\n}\n", "timestamp": 1616104674494, "sig": "f7aa18fe329cf63d13a07bb6cbf9a342c4f3a533344e4adf2b17577719f183272d34815de14b9ee1f35e90dfd7e3578f12e5aa1bce152e4dd4b7015317636f6a1c", "sigAlgorithm": "secp256k1:eth", "phloPrice": 1, "phloLimit": 250000, "validAfterBlockNumber": 664485, "cost": 164532, "errored": false, "systemDeployError": ""}, "fromAddr": "11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns", "toAddr": "11112aVmLgtqyBSWAJVm6RuokCkD8NG2Qagxzr2rnwcpQdHo35orns", "amount": 1, "retUnforeable": "OiQKIgog0IXmSPgaIjoZBMUTu3y5VMoqoaRaxl0arl2XLP+lTB0=\n", "success": true, "reason": ""}]]"""
    val data            = Transaction.parseTransactionInfoJson(rawJson)
    val transactionInfo = data.right.get
    transactionInfo.length should equal(4)
    transactionInfo(0).length should equal(1)
    transactionInfo(1).length should equal(1)
    transactionInfo(2).length should equal(1)
    transactionInfo(3).length should equal(1)
  }

  "transactionInfo json encoder" should "work" in {
    val primitive = TransactionInfo(
      fromAddr = "",
      toAddr = "String",
      amount = 1L,
      retUnforeable = "String",
      deploy = deployInfoJ(
        deployer = "String",
        term = "String",
        timestamp = 1L,
        sig = "String",
        sigAlgorithm = "String",
        phloPrice = 1L,
        phloLimit = 2L,
        validAfterBlockNumber = 3L,
        cost = 4L,
        errored = true,
        systemDeployError = "String"
      ),
      success = true,
      reason = "String"
    )
    val originData = List(
      List(primitive, primitive.copy(fromAddr = "123123123")),
      List(primitive.copy(toAddr = "asdasvq34g24t"))
    )
    val originDataString = originData.asJson.toString()
    val decodeData       = Transaction.parseTransactionInfoJson(originDataString)
    val encodeData       = transactionValueCodec.encode(originData)
    val targetData       = transactionValueCodec.decode(encodeData.getOrElse(BitVector.empty))
    targetData.toEither.right.get.value should equal(originData)
    decodeData.right.get should equal(originData)
  }

  "empty json" should "parsed" in {
    val rawJson =
      """[[]]""".stripMargin
    val data = Transaction.parseTransactionInfoJson(rawJson)
    data.right.get should equal(List(List.empty))
  }

}
