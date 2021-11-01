package com.revdefine.node.web

import cats.effect.{Async, ContextShift}
import cats.syntax.all._
import com.revdefine.node.store.MongoStore
import com.revdefine.node.store.pagination.Paginate
import com.revdefine.node.web.account.Account.Account
import com.revdefine.node.web.transfer.Transfer.{Transaction, TransactionType}
import org.http4s.{EntityEncoder, HttpRoutes}
import org.http4s.circe.jsonEncoderOf
import io.circe.generic.auto._
import com.revdefine.node.web.transfer.Transfer
import coop.rchain.shared.Log
import io.circe.{Encoder, Json}
import org.http4s.circe.CirceEntityCodec.circeEntityEncoder

object DefineRoutes {
  final case class PageInfo(totalPage: Long, currentPage: Long)
  final case class TransactionsResponse(transactions: Seq[Transaction], pageInfo: PageInfo)
  final case class RankAccountsResponse(accounts: Seq[RankAccount])
  final case class RangeDataResponse(datas: List[StatItem])
  final case class AccountTopStatDataResponse(data: AccountTopStatData)

  final case class RankAccount(address: String, balance: Long, tags: List[String], rank: Long)
  object RankAccount {
    def apply(account: Account, rank: Long): RankAccount =
      RankAccount(account.address, account.balance, account.tags, rank)
  }

  def service[F[_]: Async: ContextShift: Log](
      mongo: MongoStore[F],
      cachedStat: CacheStat[F]
  ): HttpRoutes[F] = {

    import coop.rchain.node.encode.JsonEncoder._
    val dsl = org.http4s.dsl.Http4sDsl[F]
    import dsl._

    object RowsPerPageParam extends OptionalQueryParamDecoderMatcher[Long]("rowsPerPage")
    object CurrentPageParam extends OptionalQueryParamDecoderMatcher[Long]("page")
    object IsFinalizedParam extends OptionalQueryParamDecoderMatcher[Boolean]("isFinalized")
    object IsSucceededParam extends OptionalQueryParamDecoderMatcher[Boolean]("isSucceeded")

    val DefaultIsFinalized = true
    val DefaultIsSucceeded = true
    val DefaultCurrentPage = 1L
    val DefaultRowsPerPage = 20L

    val statApi = StatAPI(mongo, cachedStat)

    implicit def transactionTypeEncoder: Encoder[TransactionType] = new Encoder[TransactionType] {
      override def apply(a: TransactionType): Json = a match {
        case Transfer.PreCharge()         => Json.fromString("PreCharge")
        case Transfer.Refund()            => Json.fromString("Refund")
        case Transfer.UserDeploy()        => Json.fromString("UserDeploy")
        case Transfer.CloseBlock()        => Json.fromString("CloseBlock")
        case Transfer.Slashing()          => Json.fromString("Slashing")
        case Transfer.Genesis()           => Json.fromString("Genesis")
        case Transfer.CustomType(message) => Json.fromString(s"CustomType(${message})")
      }
    }
    implicit val transactionTypeEntityEncoder: EntityEncoder[F, TransactionType] =
      jsonEncoderOf[F, TransactionType]
    implicit val transactionEncoder: EntityEncoder[F, Transaction] = jsonEncoderOf[F, Transaction]

    def PaginTransToResp(page: Paginate[F, Transaction]) =
      for {
        transactions <- page.items
        totalPage    <- page.totalPage
      } yield TransactionsResponse(transactions, PageInfo(totalPage, page.currentPage))

    HttpRoutes.of[F] {
      case GET -> Root / "transactions" :? RowsPerPageParam(rowsPerPage) +& CurrentPageParam(
            currentPage
          ) =>
        Ok(
          PaginTransToResp(
            mongo.latestTransactions(
              rowsPerPage.getOrElse(DefaultRowsPerPage),
              currentPage.getOrElse(DefaultCurrentPage)
            )
          )
        )

      case GET -> Root / "transactions" / revAddress :? RowsPerPageParam(rowsPerPage) +& CurrentPageParam(
            currentPage
          ) +& IsSucceededParam(isSucceeded) =>
        Ok(
          PaginTransToResp(
            mongo.transfer(
              revAddress,
              isSucceeded,
              rowsPerPage.getOrElse(DefaultRowsPerPage),
              currentPage.getOrElse(DefaultCurrentPage)
            )
          )
        )

      case GET -> Root / "transactions" / revAddress / "transfer" :? RowsPerPageParam(rowsPerPage) +& CurrentPageParam(
            currentPage
          ) +& IsSucceededParam(isSucceeded) =>
        Ok(
          PaginTransToResp(
            mongo.transferWithoutValidator(
              revAddress,
              isSucceeded,
              rowsPerPage.getOrElse(DefaultRowsPerPage),
              currentPage.getOrElse(DefaultCurrentPage)
            )
          )
        )

      case GET -> Root / "deploy" / deployId / "transfer" :? RowsPerPageParam(rowsPerPage) +& CurrentPageParam(
            currentPage
          ) =>
        Ok(
          PaginTransToResp(
            mongo.deployTransfer(
              deployId,
              rowsPerPage.getOrElse(DefaultRowsPerPage),
              currentPage.getOrElse(DefaultCurrentPage)
            )
          )
        )
      case GET -> Root / "revaccount" / revAddress =>
        mongo.balance(revAddress).flatMap(_.fold(NotFound())(Ok(_)))

      case GET -> Root / "revaccounts" :? RowsPerPageParam(rowsPerPage) +& CurrentPageParam(
            currentPage
          ) =>
        val rows    = rowsPerPage.getOrElse(100L)
        val current = currentPage.getOrElse(DefaultCurrentPage)
        Ok(
          mongo
            .revAccounts(rows, current)
            .items
            .map(_.zip((1L to rows).toList.map(_ * DefaultCurrentPage)).map {
              case (account, i) => RankAccount(account, i)
            })
            .map(RankAccountsResponse)
        )

      case GET -> Root / "block" / blockHash / "transactions" =>
        Ok(mongo.blockTransaction(blockHash).items)

      case GET -> Root / "stat" / "deploy"   => Ok(statApi.statDeploy.map(RangeDataResponse))
      case GET -> Root / "stat" / "transfer" => Ok(statApi.statTransfer.map(RangeDataResponse))
      case GET -> Root / "stat" / "accounts" =>
        Ok(statApi.statAccount.map(AccountTopStatDataResponse))

    }
  }

}
