package coop.rchain.rholang.util

import java.io.PrintWriter
import java.nio.file.{Files => JFiles, Path, Paths}

import scala.collection.JavaConverters._

object Files {
  def isDirectory(p: Path): Boolean = p.toFile.isDirectory

  def t[A]: A => Boolean = (a: A) => true

  def ofType(ext: String): Path => Boolean = (p: Path) => {
    p.toString.endsWith(s".$ext")
  }

  def walk(start: Path, filter: Path => Boolean = t[Path]): List[Path] = {
    val ds = JFiles.newDirectoryStream(start)
    val it = ds.iterator.asScala

    val (files, dirs) = it.foldLeft(List.empty[Path] -> List.empty[Path]) {
      case ((acc, dacc), curr) =>
        val newAcc  = if (filter(curr)) curr :: acc else acc
        val newDAcc = if (isDirectory(curr)) curr :: dacc else dacc

        newAcc -> newDAcc
    }

    ds.close()
    files ++ dirs.flatMap(walk(_, filter))
  }

  def write[A](output: Path, content: A): Unit = {
    val pw = new PrintWriter(output.toFile)
    pw.println(content)
    pw.close()
  }
}
