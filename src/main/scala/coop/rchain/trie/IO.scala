package coop.rchain.trie

import coop.rchain.trie.Datastore._

sealed trait IO[A] { self =>
  def run: A
  def map[B](f: A => B): IO[B] = new IO[B] { def run = f(self.run) }
  def flatMap[B](f: A => IO[B]): IO[B] = new IO[B] { def run = f(self.run).run }
}

object IO {
  def unit[A](a: => A): IO[A] = new IO[A] { def run = a }
  def flatMap[A,B](fa: IO[A])(f: A => IO[B]) = fa flatMap f
  def apply[A](a: => A): IO[A] = unit(a)

  def Update(n: Trie): IO[Trie] = IO { returning(n)(db.put) }
  def Insert(n: Trie): IO[Trie] = IO { returning(n)(db.insert) }
  def Get(id: String): IO[Option[Trie]] = IO { db.get(id) }
}
