package coop.rchain.models

import com.google.protobuf.ByteString
import coop.rchain.casper.protocol._
import coop.rchain.crypto.{PrivateKey, PublicKey}
import coop.rchain.crypto.codec.Base16
import coop.rchain.crypto.hash.Blake2b256
import coop.rchain.crypto.signatures.Secp256k1
import coop.rchain.models.BlockHash.BlockHash
import coop.rchain.models.Validator.Validator
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Gen.listOfN
import org.scalacheck.util.Buildable

object blockImplicits {

  val blockHashGen: Gen[BlockHash] = for {
    byteArray <- listOfN(BlockHash.Length, arbitrary[Byte])
  } yield ByteString.copyFrom(byteArray.toArray)

  val validatorGen: Gen[Validator] = for {
    byteArray <- listOfN(Validator.Length, arbitrary[Byte])
  } yield ByteString.copyFrom(byteArray.toArray)

  val arbitraryHash: Arbitrary[BlockHash]      = Arbitrary(blockHashGen)
  val arbitraryValidator: Arbitrary[Validator] = Arbitrary(validatorGen)
  val arbitraryValidators: Arbitrary[Seq[Validator]] =
    Arbitrary.arbContainer[Seq, Validator](
      arbitraryValidator,
      Buildable.buildableCanBuildFrom,
      identity
    )

  val justificationGen: Gen[Justification] = for {
    latestBlockHash <- arbitrary[BlockHash](arbitraryHash)
  } yield Justification.from(JustificationProto().withLatestBlockHash(latestBlockHash))

  implicit val arbitraryJustification: Arbitrary[Justification] = Arbitrary(justificationGen)

  val processedDeployGen: Gen[ProcessedDeploy] =
    for {
      term        <- listOfN(32, Gen.alphaNumChar).map(_.mkString)
      timestamp   <- arbitrary[Long]
      (sec, pub)  = Secp256k1.newKeyPair
      bytesLength <- Gen.chooseNum(128, 256)
      randomBytes <- listOfN(bytesLength, arbitrary[Byte]).map(_.toArray)
      hash        = Blake2b256.hash(randomBytes)
      deployData = DeployData(
        deployer = ByteString.copyFrom(pub.bytes),
        term = term,
        timestamp = timestamp,
        sig = ByteString.copyFrom(Secp256k1.sign(hash, sec)),
        sigAlgorithm = Secp256k1.name,
        phloLimit = 90000,
        phloPrice = 1L,
        validAfterBlockNumber = 0
      )
    } yield ProcessedDeploy(
      deploy = deployData,
      cost = PCost(0L),
      deployLog = List.empty,
      paymentLog = List.empty,
      errored = false
    )

  implicit val arbitraryProcessedDeploy: Arbitrary[ProcessedDeploy] = Arbitrary(processedDeployGen)

  val blockElementGen: Gen[BlockMessage] =
    for {
      hash            <- arbitrary[BlockHash](arbitraryHash)
      validator       <- arbitrary[Validator](arbitraryValidator)
      version         <- arbitrary[Long]
      timestamp       <- arbitrary[Long]
      parentsHashList <- arbitrary[Seq[Validator]](arbitraryValidators)
      justifications  <- arbitrary[Seq[Justification]]
      deploys         <- arbitrary[Seq[ProcessedDeploy]]
    } yield BlockMessage(
      blockHash = hash,
      header = Header(
        parentsHashList = parentsHashList.toList,
        deploysHash = ByteString.EMPTY,
        timestamp = timestamp,
        version = version,
        deployCount = 0
      ),
      body = Body(
        state = RChainState(
          preStateHash = ByteString.EMPTY,
          postStateHash = ByteString.EMPTY,
          bonds = List.empty,
          blockNumber = 0L
        ),
        deploys = deploys.toList
      ),
      justifications = justifications.toList,
      sender = validator,
      seqNum = 0,
      sig = ByteString.EMPTY,
      sigAlgorithm = "",
      shardId = ""
    )

  val blockElementsGen: Gen[List[BlockMessage]] =
    Gen.listOf(blockElementGen)

  val blockBatchesGen: Gen[List[List[BlockMessage]]] =
    Gen.listOf(blockElementsGen)

  def blockElementsWithParentsGen: Gen[List[BlockMessage]] =
    Gen.sized { size =>
      (0 until size).foldLeft(Gen.listOfN(0, blockElementGen)) {
        case (gen, _) =>
          for {
            blocks       <- gen
            b            <- blockElementGen
            parents      <- Gen.someOf(blocks)
            parentHashes = parents.map(_.blockHash).toList
            newBlock     = b.copy(header = b.header.copy(parentsHashList = parentHashes))
          } yield newBlock :: blocks
      }
    }

  def blockWithNewHashesGen(blockElements: List[BlockMessage]): Gen[List[BlockMessage]] =
    Gen.listOfN(blockElements.size, blockHashGen).map { blockHashes =>
      blockElements.zip(blockHashes).map {
        case (b, hash) => b.copy(blockHash = hash)
      }
    }
}
