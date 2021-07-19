package com.revdefine.node

import cats.syntax.all._
import cats.effect.concurrent.Ref
import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, Sync, Timer}
import com.revdefine.node.store.MongoStore
import com.revdefine.node.web.node.api.API
import coop.rchain.shared.Log
import monix.execution.Scheduler
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import fs2.Stream
import org.http4s.server.middleware.CORS

import scala.collection.concurrent.TrieMap
import scala.concurrent.duration.{DurationInt, FiniteDuration}
package object web {

  val PORT: Int               = 40406
  val HOST: String            = "0.0.0.0"
  val TIMEOUT: FiniteDuration = 40.seconds
  def initService[F[_]: Sync: ConcurrentEffect: Timer: ContextShift: Log](
      mongo: MongoStore[F],
      api: API[F]
  )(
      implicit scheduler: Scheduler
  ): Stream[F, ExitCode] = {

    val routeS = Stream.eval(for {
      transferStatCached <- Ref.of[F, TrieMap[TimeWindow, StatItem]](
                             TrieMap.empty[TimeWindow, StatItem]
                           )
      deployStatCached  <- Ref.of(TrieMap.empty[TimeWindow, StatItem])
      accountStatCached <- Ref.of(none[AccountTopStatData])
      lastOp            <- Ref.of(0L)
      accCache          = TTLCache(accountStatCached, lastOp, 1.hour.toMillis)
      c                 = CacheStat(transferStatCached, deployStatCached, accCache)
      rnodeService      = RnodeRoutes.service(api)
      defineService     = DefineRoutes.service(mongo, c)
      route = Router(
        ("api", CORS(rnodeService)),
        ("define/api", CORS(defineService))
      ).orNotFound
    } yield route)

    routeS.flatMap(
      r =>
        BlazeServerBuilder[F](scheduler)
          .bindHttp(PORT, HOST)
          .withHttpApp(r)
          .withIdleTimeout(TIMEOUT)
          .serve
    )
  }
}
