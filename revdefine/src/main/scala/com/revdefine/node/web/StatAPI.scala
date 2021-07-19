package com.revdefine.node.web

import cats.syntax.all._
import cats.effect.concurrent.Ref
import cats.effect.{Async, ContextShift, Sync}
import com.revdefine.node.store.MongoStore
import com.github.nscala_time.time.Imports._
import com.revdefine.node.web.StatAPI.{statRange, OneDayTimestamp}
import com.revdefine.node.web.account.Account.BURNED_TAG
import coop.rchain.shared.Log
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Sorts.descending
import com.revdefine.syntax.all._

import scala.collection.concurrent.TrieMap
final case class TimeWindow(start: Long, end: Long)
final case class StatItem(start: Long, end: Long, data: Long)

final case class CacheStat[F[_]](
    transfer: Ref[F, TrieMap[TimeWindow, StatItem]],
    deploy: Ref[F, TrieMap[TimeWindow, StatItem]],
    account: TTLCache[F, AccountTopStatData]
)

final case class TTLCache[F[_]: Sync, T](
    cached: Ref[F, Option[T]],
    lastOp: Ref[F, Long],
    ttl: Long
) {
  def get(retrieve: => F[T]): F[T] = {
    val c = DateTime.now(DateTimeZone.UTC).getMillis
    for {
      cachedOpt <- cached.get
      last      <- lastOp.get
      item <- if (cachedOpt.isEmpty || c - last > ttl) for {
               i <- retrieve
               _ <- cached.update(_ => Some(i))
               _ <- lastOp.update(_ => c)
             } yield i
             else {
               cachedOpt.get.pure
             }
    } yield item
  }
}

final case class AccountTopStatData(
    top10: Long,
    top50: Long,
    top100: Long,
    total: Long,
    totalAccount: Long,
    last24hActiveAccountAmount: Long,
    last7dActiveAccountAmount: Long,
    last1mActiveAccountAmount: Long
)

final case class StatAPI[F[_]: Async: ContextShift: Log](
    mongo: MongoStore[F],
    cache: CacheStat[F]
) {

  val totalRev = 106000000000000460L

  private def getWindows(
      transactionType: String,
      cached: Ref[F, TrieMap[TimeWindow, StatItem]]
  ): F[List[StatItem]] = {
    val e = DateTime.nextDay.hour(0).minute(0).second(0).millis(0).getMillis
    val startToEnd = ((1 to statRange)
      .map(i => e - i * OneDayTimestamp)
      .reverse zip (0 until statRange).map(i => e - i * OneDayTimestamp).reverse)
    val windows      = startToEnd.map { case (s, e) => TimeWindow(s, e) }
    val (wins, last) = (windows.take(windows.length - 1), windows.last)

    for {
      stats <- wins.toList.traverse(
                w =>
                  for {
                    transfer <- cached.get
                    dataOpt  = transfer.get(w)
                    item <- dataOpt.fold(for {
                             _ <- Log[F].debug(
                                   s"Stat data for ${transactionType} range (${w.start}, ${w.end}) not exist"
                                 )
                             count <- mongo.countTransaction(
                                       transactionType = transactionType,
                                       (w.start, w.end)
                                     )
                             _ <- Log[F].debug(
                                   s"Stat data for ${transactionType} range (${w.start}, ${w.end}) is ${count}"
                                 )
                             stateItem = StatItem(w.start, w.end, count)
                             _         <- cached.update(t => t += ((w, stateItem)))
                           } yield stateItem)(identity(_).pure[F])
                  } yield item
              )
      // don't cache the last one and update in every request
      lastStat <- mongo.countTransaction(transactionType = transactionType, (last.start, last.end))
    } yield stats :+ StatItem(last.start, last.end, lastStat)
  }

  def statDeploy: F[List[StatItem]] = getWindows("PreCharge", cache.deploy)

  def statTransfer: F[List[StatItem]] = getWindows("UserDeploy", cache.transfer)

  def statAccount: F[AccountTopStatData] = cache.account.get(statAccountFromMongo)
  private def statAccountFromMongo: F[AccountTopStatData] = {
    val n = DateTime.now(DateTimeZone.UTC).getMillis
    for {
      accounts <- mongo.accountCollection
                   .find(nin("tags", BURNED_TAG))
                   .sort(descending("balance"))
                   .limit(100)
                   .liftToF
      (top10, top50, top100) = accounts.zipWithIndex.foldLeft((0L, 0L, 0L)) {
        case ((t10, t50, t100), (account, i)) =>
          val t10c = if (i <= 10) {
            t10 + account.balance
          } else t10
          val t50c = if (i <= 50) {
            t50 + account.balance
          } else t50
          val t100c = t100 + account.balance
          (t10c, t50c, t100c)
      }
      totalAccounts <- mongo.accountCollection.countDocuments().liftToF
      fromAddrLast24h <- mongo.transactionCollection
                          .find(gt("timestamp", n - OneDayTimestamp))
                          .liftToF
                          .map(a => a.map(_.fromAddr).toSet.size.toLong)
      fromAddrLast7d <- mongo.transactionCollection
                         .find(gt("timestamp", n - OneDayTimestamp * 7))
                         .liftToF
                         .map(a => a.map(_.fromAddr).toSet.size.toLong)
      fromAddrLast30d <- mongo.transactionCollection
                          .find(gt("timestamp", n - OneDayTimestamp * 30))
                          .liftToF
                          .map(a => a.map(_.fromAddr).toSet.size.toLong)

    } yield AccountTopStatData(
      top10,
      top50,
      top100,
      totalRev,
      totalAccounts,
      fromAddrLast24h,
      fromAddrLast7d,
      fromAddrLast30d
    )
  }
}

object StatAPI {
  val statRange: Int = sys.env.getOrElse("StatRange", "30").toInt
  // one day milli seconds
  val OneDayTimestamp: Long = 60L * 60L * 24L * 1000L

}
