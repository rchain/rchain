package AbstractInterpreter

import scala.collection.immutable.HashMap

  sealed trait Lattice[A] {
    val bot: A
    val top: A
    val rel: (A, A) => Boolean
    val join: (A, A) => A
    val meet: (A, A) => A
  }

  object Lattice {

    implicit def stringLattice: Lattice[String] = {
      new Lattice[String] {
        val bot: String = "bot"
        val top: String = "top"
        val rel: (String, String) => Boolean = (a: String, b: String) => true
        val join: (String, String) => String = (a: String, b: String) => "top"
        val meet: (String, String) => String = (a: String, b: String) => "bot"
      }
    }

    implicit def unitLattice: Lattice[Unit] = {
      new Lattice[Unit] {
        val bot: Unit = Unit
        val top: Unit = Unit
        val rel: (Unit, Unit) => Boolean = (a: Unit, b: Unit) => true
        val join: (Unit, Unit) => Unit = (a: Unit, b: Unit) => top
        val meet: (Unit, Unit) => Unit = (a: Unit, b: Unit) => bot
      }
    }

    implicit def productLattice[A, B](implicit l1: Lattice[A], l2: Lattice[B]): Lattice[(A, B)] = {
      new Lattice[(A, B)] {
        val bot: (A, B) = (l1.bot, l2.bot)
        val top: (A, B) = (l1.top, l2.top)
        val rel: ((A, B), (A, B)) => Boolean = (a: (A, B), b: (A, B)) => l1.rel(a._1, b._1) && l2.rel(a._2, b._2)
        val join: ((A, B), (A, B)) => (A, B) = (a: (A, B), b: (A, B)) => (l1.join(a._1, b._1), l2.join(a._2, b._2))
        val meet: ((A, B), (A, B)) => (A, B) = (a: (A, B), b: (A, B)) => (l1.meet(a._1, b._1), l2.meet(a._2, b._2))
      }
    }

    implicit def ordEqLattice[S]: Lattice[Set[S]] = {
      new Lattice[Set[S]] {
        val bot: Set[S] = Set.empty[S]
        val top: Set[S] = sys.error("Set has no top element")
        val rel: (Set[S], Set[S]) => Boolean = (a: Set[S], b: Set[S]) => a subsetOf b
        val join: (Set[S], Set[S]) => Set[S] = (a: Set[S], b: Set[S]) => a union b
        val meet: (Set[S], Set[S]) => Set[S] = (a: Set[S], b: Set[S]) => a intersect b
      }
    }

    implicit def mapLattice[K, V](implicit l: Lattice[V]): Lattice[HashMap[K, V]] = {
      new Lattice[HashMap[K, V]] {
        val bot: HashMap[K, V] = HashMap.empty
        val top: HashMap[K, V] = sys.error("HashMap has no top element")
        val rel: (HashMap[K, V], HashMap[K, V]) => Boolean = (f: HashMap[K, V], g: HashMap[K, V]) => f.isSubHashMapOfBy(g)(l.rel)
        val join: (HashMap[K, V], HashMap[K, V]) => HashMap[K, V] = (f: HashMap[K, V], g: HashMap[K, V]) => f.unionWith(g)(l.join)
        val meet: (HashMap[K, V], HashMap[K, V]) => HashMap[K, V] = (f: HashMap[K, V], g: HashMap[K, V]) => f.intersectionWith(g)(l.meet)
      }
    }

    implicit def ⊎[K,V](m: HashMap[K,V], xs: List[(K,V)])(implicit l: Lattice[V]): HashMap[K,V] = {
      xs match {
        case Nil => m
        case (k,v)::tl => ⊎(m, tl) insertWith (k, v, l.join)
      }
    }

    implicit def ⨆[K, V](m: HashMap[K,V], xs: List[(K, V)])(implicit l: Lattice[V]): HashMap[K,V] = {
      xs match {
        case Nil => m
        case (k,v)::tl => ⨆(m, tl) insertWith (k, v, l.join)
      }
    }

    implicit def !![K, V](m: HashMap[K, V])(implicit l: Lattice[V]): K => V = {
      (k: K) => m getOrElse (k,l.bot)
    }

    implicit def /[K,V](m: HashMap[K,V], xs: List[(K,V)]): HashMap[K,V] = {
      for( (k,v) <- xs){
        m + (k -> v)
      }
      m
    }

    implicit class HashMapTheory[K, A](private val m1: HashMap[K, A]) {

      def insertWith[A1 >: A](k: K, v: A1, f: (A1, A1) => A1): HashMap[K, A1] = {
        m1 updated(k, m1 get k map(f(v, _)) getOrElse v )
      }

      def unionWithKey(m2: HashMap[K, A])(f: (K, A, A) => A): HashMap[K, A] = {
        val diff: HashMap[K,A] = m2 -- m1.keySet
        val aug: HashMap[K, A] = m1 map {
          case (k, v) => if (m2 contains k) k -> f(k, v, m2(k)) else (k, v)
        }
        aug ++: diff
      }

      def unionWith(m2: HashMap[K, A])(f: (A, A) => A): HashMap[K, A] = {
        unionWithKey(m2)((_, x, y) => f(x, y))
      }

      def intersectionWithKey[B, C](m2: HashMap[K, B])(f: (K, A, B) => C): HashMap[K, C] = {
        m1 collect {
          case (k, v) if m2 contains k => k -> f(k, v, m2(k))
        }
      }

      def intersectionWith[B, C](m2: HashMap[K, B])(f: (A, B) => C): HashMap[K, C] = {
        intersectionWithKey(m2)((_, x, y) => f(x, y))
      }

      def isSubHashMapOfBy[B](m2: HashMap[K, B])(g: (A, B) => Boolean): Boolean = {
        val f: (K, A) => Boolean = (k: K, v: A) => (m2 contains k) && g(v, m2(k))
        m1 forall { case (k, v) => f(k, v) }
      }

      def bigJoin[V](f: HashMap[K, Set[V]], list: List[(K, Set[V])]): HashMap[K, Set[V]] = list match {
        case Nil => f
        case (k, v) :: tl =>
          val newVal = f.getOrElse(k, Set()) ++ v
          (f - k) + (k -> newVal)
      }
    }
  }




