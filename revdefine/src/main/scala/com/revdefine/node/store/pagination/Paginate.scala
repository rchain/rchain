package com.revdefine.node.store.pagination

import cats.syntax.all._
import cats.effect.{Async, ContextShift}
import org.mongodb.scala.MongoCollection
import org.mongodb.scala.bson.conversions.Bson

import scala.reflect.ClassTag

trait Paginate[F[_], A] {
  def totalPage: F[Long]
  def currentPage: Long
  def rowsPerPage: Long
  def items: F[Seq[A]]
}

object Paginate {
  final case class PageInfo(
      totalPage: Long,
      currentPage: Long,
      rowsPerPage: Long
  )
  final case class PageResponse[T](
      data: Seq[T],
      pageInfo: PageInfo
  )

  final case class MongoPaginate[F[_]: Async: ContextShift, A: ClassTag](
      mongoCollection: MongoCollection[A],
      query: Bson,
      sort: Bson,
      rowsPerPage: Long,
      currentPage: Long
  ) extends Paginate[F, A] {
    override def items: F[Seq[A]] =
      Async.fromFuture(
        Async[F].delay(
          mongoCollection
            .find[A](query)
            .sort(sort)
            .skip((rowsPerPage * (currentPage - 1)).toInt)
            .limit(rowsPerPage.toInt)
            .toFuture()
        )
      )

    override def totalPage: F[Long] =
      Async
        .fromFuture(
          Async[F].pure(
            mongoCollection
              .countDocuments(query)
              .toFuture()
          )
        )
        .map(c => if (c % rowsPerPage != 0) c / rowsPerPage + 1 else c / rowsPerPage)

  }
}
