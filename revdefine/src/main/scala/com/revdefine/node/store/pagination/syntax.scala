package com.revdefine.node.store.pagination

import cats.Monad
import cats.syntax.all._
import com.revdefine.node.store.pagination.Paginate.{PageInfo, PageResponse}

trait PaginateSyntax {
  implicit final def paginateSyntax[F[_], A](p: Paginate[F, A]): PaginateSyntaxOps[F, A] =
    new PaginateSyntaxOps[F, A](p)
}

final class PaginateSyntaxOps[F[_], A](private val p: Paginate[F, A]) extends AnyVal {
  def toPageResponse(implicit s: Monad[F]): F[PageResponse[A]] =
    p.items.flatMap(data => p.totalPage.map((data, _))).map {
      case (data, totalPage) =>
        PageResponse(
          data = data,
          pageInfo = PageInfo(totalPage, p.currentPage, p.rowsPerPage)
        )
    }
}
