package coop.rchain.rspace.test

import scala.collection.mutable.ListBuffer

import coop.rchain.rspace._
import coop.rchain.rspace.examples.StringExamples.{Pattern, StringMatch, StringsCaptor, Wildcard}
import coop.rchain.rspace.history._
import coop.rchain.rspace.internal.{Datum, GNAT, WaitingContinuation}
import org.scalacheck._
import org.scalacheck.Arbitrary._
import scodec.bits.ByteVector

object ArbitraryInstances {

  implicit val arbitraryPattern: Arbitrary[Pattern] = {
    val genWildcard: Gen[Pattern] = Gen.const(Wildcard)
    val genMatch: Gen[Pattern]    = Arbitrary.arbitrary[String].map((str: String) => StringMatch(str))
    val genPattern: Gen[Pattern]  = Gen.oneOf(genWildcard, genMatch)
    Arbitrary(genPattern)
  }

  val arbNonEmptyString =
    Arbitrary(Gen.nonEmptyListOf[Char](Arbitrary.arbChar.arbitrary).map(_.mkString))

  implicit def arbitraryDatum[T](implicit arbT: Arbitrary[T]): Arbitrary[Datum[T]] =
    Arbitrary(for {
      t <- arbT.arbitrary
      b <- Arbitrary.arbitrary[Boolean]
    } yield Datum(t, b))

  implicit val arbitraryBlake2b256Hash: Arbitrary[Blake2b256Hash] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(bytes => Blake2b256Hash.create(bytes)))

  implicit val arbitraryPointer: Arbitrary[Pointer] =
    Arbitrary(Arbitrary.arbitrary[Blake2b256Hash].map(LeafPointer))

  implicit val arbitraryByteVector: Arbitrary[ByteVector] =
    Arbitrary(Arbitrary.arbitrary[Array[Byte]].map(bytes => ByteVector(bytes)))

  implicit val arbitraryPointerBlock: Arbitrary[PointerBlock] =
    Arbitrary(Gen.sized { _ =>
      Gen
        .listOfN(256, Arbitrary.arbitrary[Pointer])
        .map(maybeHashes => PointerBlock.fromVector(maybeHashes.toVector))
    })

  implicit def arbitaryTrie[K, V](implicit
                                  arbK: Arbitrary[K],
                                  arbV: Arbitrary[V]): Arbitrary[Trie[K, V]] = {
    val genNode: Gen[Trie[K, V]] = Arbitrary.arbitrary[PointerBlock].map(pb => Node(pb))

    val genLeaf: Gen[Trie[K, V]] =
      for {
        k <- arbK.arbitrary
        v <- arbV.arbitrary
      } yield Leaf(k, v)

    val genTrie: Gen[Trie[K, V]] =
      Gen.oneOf(genLeaf, genNode)

    Arbitrary(genTrie)
  }

  implicit val arbitraryTestKey: Arbitrary[TestKey] =
    Arbitrary(Gen.sized { _ =>
      Gen
        .listOfN(4, Arbitrary.arbitrary[Int])
        .map(ints => TestKey.create(ints))
    })

  implicit val arbitraryNonEmptyMapTestKeyByteVector: Arbitrary[Map[TestKey, ByteVector]] = {
    Arbitrary(
      Gen
        .sized { size =>
          Gen.containerOfN[Seq, (TestKey, ByteVector)](if (size > 2) size else 2,
                                                       Arbitrary.arbitrary[(TestKey, ByteVector)])
        }
        .map(_.toMap))
  }

  implicit val arbitraryNonEmptyMapStringDatumString: Arbitrary[Map[String, Datum[String]]] = {
    Arbitrary(
      Gen
        .sized { size =>
          Gen.nonEmptyContainerOf[Seq, (String, Datum[String])](for {
            str <- arbNonEmptyString.arbitrary
            dat <- Arbitrary.arbitrary[Datum[String]]
          } yield (str, dat))
        }
        .map(_.toMap))
  }

  def arbitraryWaitingContinuation(
      patternsLength: Int): Arbitrary[WaitingContinuation[Pattern, StringsCaptor]] =
    Arbitrary(
      for {
        pats    <- Gen.containerOfN[List, Pattern](patternsLength, Arbitrary.arbitrary[Pattern])
        boolean <- Arbitrary.arbitrary[Boolean]
      } yield WaitingContinuation(pats, new StringsCaptor, boolean)
    )

  implicit val arbitraryGnat: Arbitrary[GNAT[String, Pattern, String, StringsCaptor]] = {
    Arbitrary(Gen.sized { size =>
      val constrainedSize = if (size > 1) size else 1
      for {
        chans <- Gen.containerOfN[List, String](constrainedSize, Arbitrary.arbitrary[String])
        data  <- Gen.nonEmptyContainerOf[List, Datum[String]](Arbitrary.arbitrary[Datum[String]])
        wks <- Gen.nonEmptyContainerOf[List, WaitingContinuation[Pattern, StringsCaptor]](
                arbitraryWaitingContinuation(constrainedSize).arbitrary)
      } yield GNAT(chans, data, wks)
    })
  }

  implicit val arbitraryNonEmptyMapListStringWaitingContinuation
    : Arbitrary[Map[List[String], WaitingContinuation[Pattern, StringsCaptor]]] = {
    Arbitrary(Gen.sized { size =>
      val constrainedSize = if (size > 1) size else 1
      Gen
        .nonEmptyContainerOf[List, (List[String], WaitingContinuation[Pattern, StringsCaptor])](
          for {
            wc    <- arbitraryWaitingContinuation(constrainedSize).arbitrary
            chans <- Gen.containerOfN[List, String](wc.patterns.length, Arbitrary.arbitrary[String])
          } yield (chans, wc)
        )
        .map(_.toMap)
    })
  }

  /**
   Credit: https://gist.github.com/etorreborre/d0616e704ed85d7276eb12b025df8ab0

   Distinct list of elements from a given arbitrary
    */
  def distinctListOf[T: Arbitrary] =
    distinctListOfGen(arbitrary[T])

  /**
   Distinct list of elements from a given generator
   with a maximum number of elements to discard
    */
  def distinctListOfGen[T](gen: Gen[T], maxDiscarded: Int = 1000): Gen[List[T]] = {
    val seen: ListBuffer[T] = new ListBuffer[T]
    var discarded           = 0

    Gen.sized { size =>
      if (size == seen.size) seen.toList
      else {
        while (seen.size <= size && discarded < maxDiscarded) gen.sample match {
          case Some(t) if !seen.contains(t) =>
            seen.+=:(t)
          case _ => discarded += 1
        }
        seen.toList
      }
    }
  }

}
