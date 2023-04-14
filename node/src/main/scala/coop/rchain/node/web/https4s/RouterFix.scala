package coop.rchain.node.web.https4s

import cats.data.Kleisli
import cats.syntax.all._
import cats.{Functor, Monad}
import org.http4s.Uri.Path
import org.http4s.server.Router
import org.http4s.{HttpRoutes, Request}

/**
  * Original code from http4s [[Router]] with the fix for mappings to work with endpoints4s Path decoder.
  * The difference is in the [[RouterFix.translate]] function.
  *
  * https4s [[Router]] uses [[Request.attributes]] to set uri caret position
  *  which is not supported by _endpoints4s_ Path decoder.
  * This fix changes [[Request.uri]] instead by removing path prefix defined
  *  in mapping.
  */
object RouterFix {

  /**
    * Defines an [[HttpRoutes]] based on list of mappings.
    */
  def apply[F[_]: Monad](mappings: (String, HttpRoutes[F])*): HttpRoutes[F] =
    define(mappings: _*)(HttpRoutes.empty[F])

  /**
    * Defines an [[HttpRoutes]] based on list of mappings and
    * a default Service to be used when none in the list match incoming requests.
    *
    * The mappings are processed in descending order (longest first) of prefix length.
    */
  def define[F[_]: Monad](
      mappings: (String, HttpRoutes[F])*
  )(default: HttpRoutes[F]): HttpRoutes[F] =
    mappings.sortBy(_._1.length).foldLeft(default) {
      case (acc, (prefix, routes)) =>
        val prefixSegments = toSegments(prefix)
        if (prefixSegments.isEmpty) routes <+> acc
        else
          Kleisli { req =>
            (
              if (toSegments(req.pathInfo.renderString).startsWith(prefixSegments))
                routes.local(translate(prefix)) <+> acc
              else
                acc
            )(req)
          }
    }

  private def translate[F[_]: Functor](prefix: String)(req: Request[F]): Request[F] = {

    /**
      * Difference from original http4s [[Router]] is here, prefix is removed from request uri.
      */
    val path = req.uri.path.renderString.replaceAll(s"^$prefix", "")
    req.withUri(req.uri.copy(path = Path.unsafeFromString(path)))
  }

  private def toSegments(path: String): List[String] =
    path.split("/").filterNot(_.trim.isEmpty).toList
}
