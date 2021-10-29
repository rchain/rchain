package com.revdefine.node.store

import cats.effect.ExitCase.Completed
import cats.effect.{Async, ContextShift, Resource}
import cats.syntax.all._
import com.revdefine.node.web.account.Account.Account
import com.revdefine.node.web.transfer.Transfer._
import org.mongodb.scala._
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Sorts._
import org.mongodb.scala.model.Updates._
import org.bson.codecs.configuration.CodecRegistries.{fromProviders, fromRegistries}
import org.bson.codecs.configuration.{CodecProvider, CodecRegistry}
import org.mongodb.scala.MongoClient.DEFAULT_CODEC_REGISTRY
import org.mongodb.scala.bson.codecs.Macros
import com.revdefine.node.store.pagination.Paginate
import com.revdefine.node.store.pagination.Paginate.MongoPaginate
import com.revdefine.node.store.MongoStore.{emptyQuery, MongoOperateError}
import com.revdefine.syntax.all._
import org.mongodb.scala.bson.BsonArray
import org.mongodb.scala.model.UpdateOptions
import org.mongodb.scala.result.InsertManyResult

/**
  * Mongo initialize steps:
  * $ mongo
  * > a =    {       _id: "rs0",       version: 1, term:1,       members: [          { _id: 0, host : "define-mongo:27017" }       ]  }
  * > rs.initiate(a)
  *
  * ## create indexes
  * > db.account.createIndex({"address": 1}, {unique: true, background: true})
  * > db.transactions.createIndex({"blockHash": 1}, { background: true})
  * > db.transactions.createIndex({"blockNumber": -1}, { background: true})
  * > db.transactions.createIndex({"timestamp": -1}, { background: true})
  * > db.transactions.createIndex({"fromAddr": 1, "toAddr": 1,"transactionType._t": -1}, { background: true})
  *
  */
final case class MongoStore[F[_]: Async: ContextShift](uri: String) {

  private val mongoClient: MongoClient = MongoClient(uri)
  private val database: MongoDatabase =
    mongoClient.getDatabase("revdefine").withCodecRegistry(MongoStore.codecRegistry)
  val transactionCollection: MongoCollection[Transaction] =
    database.getCollection("transactions")
  val accountCollection: MongoCollection[Account] = database.getCollection("account")

  def countTransaction(transactionType: String, timeWindow: (Long, Long)): F[Long] =
    transactionCollection
      .countDocuments(
        and(
          equal("transactionType._t", transactionType),
          gt("timestamp", timeWindow._1),
          lte("timestamp", timeWindow._2)
        )
      )
      .liftToF
  def insertTransaction(transactions: List[Transaction]): F[InsertManyResult] =
    Async.fromFuture(Async[F].delay(transactionCollection.insertMany(transactions).toFuture()))

  def insertAccount(accounts: List[Account]): F[InsertManyResult] =
    Async.fromFuture(Async[F].delay(accountCollection.insertMany(accounts).toFuture()))

  def startSession: Resource[F, ClientSession] =
    Resource.makeCase[F, ClientSession](
      for {
        session <- mongoClient.startSession().liftToF
        _       = session.startTransaction()
      } yield session
    ) {
      case (session, exit) =>
        exit match {
          case Completed => session.commitTransaction().toSingle().liftToF.void
          case _         => session.abortTransaction().toSingle().liftToF.void
        }
    }

  def finalizedBlock(blockHash: String): F[Unit] =
    startSession.use { session =>
      for {
        r <- transactionCollection
              .updateMany(session, equal("blockHash", blockHash), set("isFinalized", true))
              .liftToF
        _ <- Async[F]
              .raiseError(MongoOperateError(s"Finalized block: $blockHash failed with ${r}"))
              .whenA(r.getModifiedCount == 0)
        transactions <- transactionCollection
                         .find[Transaction](session, equal("blockHash", blockHash))
                         .liftToF
        _ <- transactions
              .filter(_.isSucceeded)
              .filter(_.amount != 0)
              .toList
              .traverse(
                t =>
                  for {
                    updateFromResult <- accountCollection
                                         .updateOne(
                                           session,
                                           equal("address", t.fromAddr),
                                           inc("balance", -t.amount)
                                         )
                                         .liftToF
                    _ <- Async[F]
                          .raiseError(
                            MongoOperateError(
                              s"Update fromAddr ${t.fromAddr} balance in ${t} failed on block ${blockHash} with $updateFromResult"
                            )
                          )
                          .whenA(updateFromResult.getModifiedCount != 1)
                    updateToResult <- accountCollection
                                       .updateOne(
                                         session,
                                         equal("address", t.toAddr),
                                         inc("balance", t.amount),
                                         UpdateOptions().upsert(true)
                                       )
                                       .liftToF
                    isUpsertF = Async[F].delay(
                      updateToResult.getUpsertedId.isObjectId
                    )
                    isUpsert <- isUpsertF.redeem(_ => false, _ => true)
                    updateTags <- if (isUpsert) {
                                   accountCollection
                                     .updateOne(
                                       session,
                                       equal("address", t.toAddr),
                                       set("tags", BsonArray())
                                     )
                                     .liftToF
                                     .map(Some(_))
                                 } else Async[F].pure(None)
                    isUpdateTagsWorked = updateTags.exists(_.getModifiedCount == 1)
                    _ <- Async[F]
                          .raiseError(
                            MongoOperateError(
                              s"Update toAddr ${t.fromAddr} balance in ${t} failed on block ${blockHash} with $updateToResult"
                            )
                          )
                          .whenA(
                            updateToResult.getModifiedCount != 1 && !isUpsert && !isUpdateTagsWorked
                          )
                  } yield ()
              )
      } yield ()
    }

  def lastFinalizedTransaction: F[Transaction] =
    transactionCollection
      .find(equal("isFinalized", true))
      .sort(descending("blockNumber"))
      .first()
      .liftToF

  def lastTransaction: F[Transaction] =
    transactionCollection
      .find(emptyQuery)
      .sort(descending("blockNumber"))
      .first()
      .liftToF

  def latestTransactions(rowsPerPage: Long, currentPage: Long): Paginate[F, Transaction] =
    MongoPaginate[F, Transaction](
      transactionCollection,
      equal("transactionType._t", "UserDeploy"),
      descending("blockNumber"),
      rowsPerPage,
      currentPage
    )

  def transfer(
      address: String,
      isSucceeded: Option[Boolean],
      rowsPerPage: Long,
      currentPage: Long
  ): Paginate[F, Transaction] = {
    val filter = isSucceeded.fold(or(equal("fromAddr", address), equal("toAddr", address)))(
      isSucc =>
        and(
          or(equal("fromAddr", address), equal("toAddr", address)),
          equal("isSucceeded", isSucc)
        )
    )
    MongoPaginate[F, Transaction](
      transactionCollection,
      filter,
      descending("timestamp"),
      rowsPerPage,
      currentPage
    )
  }

  def transferWithoutValidator(
      address: String,
      isSucceeded: Option[Boolean],
      rowsPerPage: Long,
      currentPage: Long
  ): Paginate[F, Transaction] = {
    val filter = isSucceeded.fold(
      and(
        or(equal("fromAddr", address), equal("toAddr", address)),
        equal("transactionType._t", "UserDeploy")
      )
    )(
      isSucc =>
        and(
          or(equal("fromAddr", address), equal("toAddr", address)),
          equal("transactionType._t", "UserDeploy"),
          equal("isSucceeded", isSucc)
        )
    )
    MongoPaginate[F, Transaction](
      transactionCollection,
      filter,
      descending("timestamp"),
      rowsPerPage,
      currentPage
    )
  }

  def deployTransfer(
      deployId: String,
      rowsPerPage: Long,
      currentPage: Long
  ): Paginate[F, Transaction] = MongoPaginate[F, Transaction](
    transactionCollection,
    equal("deployId", deployId),
    descending("timetsamp"),
    rowsPerPage,
    currentPage
  )

  def balance(address: String): F[Option[Account]] =
    Async.fromFuture(
      Async[F].delay(
        accountCollection
          .find(
            equal("address", address)
          )
          .headOption()
      )
    )

  def revAccounts(rowsPerPage: Long, currentPage: Long): Paginate[F, Account] =
    MongoPaginate[F, Account](
      accountCollection,
      emptyQuery,
      descending("balance"),
      rowsPerPage,
      currentPage
    )

  def blockTransaction(blockHash: String): Paginate[F, Transaction] = MongoPaginate[F, Transaction](
    transactionCollection,
    equal("blockHash", blockHash),
    ascending("timestamp"),
    rowsPerPage = 100,
    currentPage = 1
  )

}

@SuppressWarnings(
  Array(
    "org.wartremover.warts.NonUnitStatements",
    "org.wartremover.warts.Throw",
    "org.wartremover.warts.JavaSerializable"
  )
)
object MongoStore {
  // 1111sLThnUPArncn2bm57ivrvqK6BjJycSyLc2AuodyAtQ4qpwzYX
  final case class MongoOperateError(message: String) extends Exception

  val transactionTypeCodecProvider: CodecProvider = Macros.createCodecProvider[TransactionType]()
  val transactionCodecProvider: CodecProvider     = Macros.createCodecProvider[Transaction]()
  val accountCodeProvider: CodecProvider          = Macros.createCodecProvider[Account]()

  val codecRegistry: CodecRegistry = fromRegistries(
    fromProviders(
      transactionTypeCodecProvider,
      transactionCodecProvider,
      accountCodeProvider
    ),
    DEFAULT_CODEC_REGISTRY
  )

  val emptyQuery = Document()

  def createStore[F[_]: Async: ContextShift](uri: String): MongoStore[F] = MongoStore(uri)

}
