package coop.rchain.casper.util.comm
import java.io.BufferedWriter
import java.nio.charset.Charset
import java.nio.file.{Files, Paths}
import java.time.format.DateTimeFormatter

import cats.effect.{Resource, Sync}
import cats.syntax.flatMap._
import cats.syntax.functor._
import cats.syntax.apply._
import coop.rchain.shared.DateTime

import scala.language.higherKinds

class DumpProgram[F[_]: Sync: DeployService: DateTime] {
  val formatter      = DateTimeFormatter.ofPattern("YYYY-mm-dd_HH-mm-ss")
  val charset        = Charset.forName("UTF-8")
  val standardPrefix = "node_dump"

  private def filename: F[String] =
    for {
      dateTime  <- DateTime[F].dateTime
      formatted = dateTime.format(formatter)
      name      = s"$standardPrefix-$formatted"
    } yield name

  private def fileResource: F[Resource[F, BufferedWriter]] =
    for {
      name <- filename
      path = Paths.get(name)
      res = Resource.fromAutoCloseable(
        Sync[F].delay(Files.newBufferedWriter(path, charset))
      )
    } yield res

  private def dumpBlocks(write: F[String => Unit]): F[Unit] =
    write.ap(DeployService[F].showBlocks())

  private def dumpNode(writer: BufferedWriter): F[Unit] = {
    val write = Sync[F].delay[String => Unit](writer.write)

    for {
      _ <- dumpBlocks(write)
    } yield ()
  }

  def run: F[Unit] =
    Resource.suspend(fileResource).use(dumpNode)
}
