package coop.rchain.catscontrib

package object ski {
  def kp[A, B](x: => B): A => B                               = _ => x
  def kp2[A, B, C](x: => C): (A, B) => C                      = (_, _) => x
  def kp3[A, B, C, D](x: => D): (A, B, C) => D                = (_, _, _) => x
  def kp6[A, B, C, D, E, F, G](x: G): (A, B, C, D, E, F) => G = (_, _, _, _, _, _) => x
  def id[A]: A => A                                           = x => x
}
