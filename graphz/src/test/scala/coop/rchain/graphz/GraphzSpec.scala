package coop.rchain.graphz

import cats.effect.IO
import cats.syntax.all._
import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import cats.effect.Ref
import cats.effect.unsafe.implicits.global

class GraphzSpec extends AnyFunSpec with Matchers with BeforeAndAfterEach with AppendedClues {

  describe("Graphz") {
    it("simple graph") {
      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("G", Graph, ser)
        _   <- g.close
      } yield ref
      graph.show shouldBe
        """graph "G" {
          |}""".stripMargin
    }

    it("simple digraph") {
      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("G", DiGraph, ser)
        _   <- g.close
      } yield ref
      graph.show shouldBe
        """digraph "G" {
          |}""".stripMargin
    }

    it("simple graph with comment") {
      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("G", Graph, ser, comment = Some("this is comment"))
        _   <- g.close
      } yield ref
      graph.show shouldBe
        """// this is comment
          |graph "G" {
          |}""".stripMargin
    }

    it("graph, two nodes one edge") {
      // given
      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("G", Graph, ser)
        _   <- g.edge("Hello", "World")
        _   <- g.close
      }
      // then
      yield ref
      graph.show shouldBe
        """graph "G" {
          |  "Hello" -- "World"
          |}""".stripMargin
    }

    it("digraph, two nodes one edge") {
      // given
      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("G", DiGraph, ser)
        _   <- g.edge("Hello", "World")
        _   <- g.close
      }
      // then
      yield ref
      graph.show shouldBe
        """digraph "G" {
          |  "Hello" -> "World"
          |}""".stripMargin
    }

    it("digraph, nodes with style") {
      // given
      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("G", DiGraph, ser)
        _   <- g.node("Hello", shape = Box)
        _   <- g.node("World", shape = DoubleCircle)
        _   <- g.edge("Hello", "World")
        _   <- g.close
      }
      // then
      yield ref
      graph.show shouldBe
        """digraph "G" {
          |  "Hello" [shape=box]
          |  "World" [shape=doublecircle]
          |  "Hello" -> "World"
          |}""".stripMargin
    }

    it("digraph with simple subgraphs") {
      def process1(ser: StringSerializer[IO]): IO[Unit] =
        for {
          g <- Graphz.subgraph[IO]("", DiGraph, ser)
          _ <- g.node("A")
          _ <- g.node("B")
          _ <- g.node("C")
          _ <- g.edge("A", "B")
          _ <- g.edge("B", "C")
          _ <- g.close
        } yield ()

      def process2(ser: StringSerializer[IO]): IO[Unit] =
        for {
          g <- Graphz.subgraph[IO]("", DiGraph, ser)
          _ <- g.node("K")
          _ <- g.node("L")
          _ <- g.node("M")
          _ <- g.edge("K", "L")
          _ <- g.edge("L", "M")
          _ <- g.close
        } yield ()

      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("Process", DiGraph, ser)
        _   <- g.node("0")
        _   <- process1(ser)
        _   <- g.edge("0", "A")
        _   <- process2(ser)
        _   <- g.edge("0", "K")
        _   <- g.node("1")
        _   <- g.edge("M", "1")
        _   <- g.edge("C", "1")
        _   <- g.close
      } yield ref
      graph.show shouldBe
        """digraph "Process" {
            |  "0"
            |  subgraph {
            |    "A"
            |    "B"
            |    "C"
            |    "A" -> "B"
            |    "B" -> "C"
            |  }
            |  "0" -> "A"
            |  subgraph {
            |    "K"
            |    "L"
            |    "M"
            |    "K" -> "L"
            |    "L" -> "M"
            |  }
            |  "0" -> "K"
            |  "1"
            |  "M" -> "1"
            |  "C" -> "1"
            |}""".stripMargin
    }

    it("digraph with fancy subgraphs") {
      def process1(ser: StringSerializer[IO]): IO[Unit] =
        for {
          g <- Graphz
                .subgraph[IO](
                  "cluster_p1",
                  DiGraph,
                  ser,
                  label = Some("process #1"),
                  color = Some("blue")
                )
          _ <- g.node("A")
          _ <- g.node("B")
          _ <- g.node("C")
          _ <- g.edge("A", "B")
          _ <- g.edge("B", "C")
          _ <- g.close
        } yield ()

      def process2(ser: StringSerializer[IO]): IO[Unit] =
        for {
          g <- Graphz
                .subgraph[IO](
                  "cluster_p2",
                  DiGraph,
                  ser,
                  label = Some("process #2"),
                  color = Some("green")
                )
          _ <- g.node("K")
          _ <- g.node("L")
          _ <- g.node("M")
          _ <- g.edge("K", "L")
          _ <- g.edge("L", "M")
          _ <- g.close
        } yield ()

      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("Process", DiGraph, ser)
        _   <- g.node("0")
        _   <- process1(ser)
        _   <- g.edge("0", "A")
        _   <- process2(ser)
        _   <- g.edge("0", "K")
        _   <- g.node("1")
        _   <- g.edge("M", "1")
        _   <- g.edge("C", "1")
        _   <- g.close
      } yield ref
      graph.show shouldBe
        """digraph "Process" {
          |  "0"
          |  subgraph "cluster_p1" {
          |    label = "process #1"
          |    color=blue
          |    "A"
          |    "B"
          |    "C"
          |    "A" -> "B"
          |    "B" -> "C"
          |  }
          |  "0" -> "A"
          |  subgraph "cluster_p2" {
          |    label = "process #2"
          |    color=green
          |    "K"
          |    "L"
          |    "M"
          |    "K" -> "L"
          |    "L" -> "M"
          |  }
          |  "0" -> "K"
          |  "1"
          |  "M" -> "1"
          |  "C" -> "1"
          |}""".stripMargin
    }

    it("blockchain, simple") {
      // given
      def lvl1(ser: StringSerializer[IO]): IO[Unit] =
        for {
          g <- Graphz.subgraph[IO]("", DiGraph, ser, rank = Some(Same))
          _ <- g.node("1")
          _ <- g.node("ddeecc", shape = Box)
          _ <- g.node("ffeeff", shape = Box)
          _ <- g.close
        } yield ()

      def lvl0(ser: StringSerializer[IO]): IO[Unit] =
        for {
          g <- Graphz.subgraph[IO]("", DiGraph, ser, rank = Some(Same))
          _ <- g.node("0")
          _ <- g.node("000000", shape = Box)
          _ <- g.close
        } yield ()

      def timeline(ser: StringSerializer[IO]): IO[Unit] =
        for {
          g <- Graphz.subgraph[IO]("timeline", DiGraph, ser)
          _ <- g.node("3", shape = PlainText)
          _ <- g.node("2", shape = PlainText)
          _ <- g.node("1", shape = PlainText)
          _ <- g.node("0", shape = PlainText)
          _ <- g.edge("0" -> "1")
          _ <- g.edge("1" -> "2")
          _ <- g.edge("2" -> "3")
          _ <- g.close
        } yield ()

      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("Blockchain", DiGraph, ser, rankdir = Some(BT))
        _   <- lvl1(ser)
        _   <- g.edge("000000" -> "ffeeff")
        _   <- g.edge("000000" -> "ddeecc")
        _   <- lvl0(ser)
        _   <- timeline(ser)
        _   <- g.close
      }
      // then
      yield ref
      graph.show shouldBe
        """digraph "Blockchain" {
            |  rankdir=BT
            |  subgraph {
            |    rank=same
            |    "1"
            |    "ddeecc" [shape=box]
            |    "ffeeff" [shape=box]
            |  }
            |  "000000" -> "ffeeff"
            |  "000000" -> "ddeecc"
            |  subgraph {
            |    rank=same
            |    "0"
            |    "000000" [shape=box]
            |  }
            |  subgraph "timeline" {
            |    "3" [shape=plaintext]
            |    "2" [shape=plaintext]
            |    "1" [shape=plaintext]
            |    "0" [shape=plaintext]
            |    "0" -> "1"
            |    "1" -> "2"
            |    "2" -> "3"
            |  }
            |}""".stripMargin
    }

    // from https://github.com/xflr6/graphviz/blob/master/examples/process.py
    it("Process example") {
      val graph = for {
        ref   <- Ref[IO].of(new StringBuffer(""))
        ser   = new StringSerializer(ref)
        graph <- Graphz[IO]("G", Graph, ser)
        _     <- graph.edge("run", "intr")
        _     <- graph.edge("intr", "runbl")
        _     <- graph.edge("runbl", "run")
        _     <- graph.edge("run", "kernel")
        _     <- graph.edge("kernel", "zombie")
        _     <- graph.edge("kernel", "sleep")
        _     <- graph.edge("kernel", "runmem")
        _     <- graph.edge("sleep", "swap")
        _     <- graph.edge("swap", "runswap")
        _     <- graph.edge("runswap", "new")
        _     <- graph.edge("runswap", "runmem")
        _     <- graph.edge("new", "runmem")
        _     <- graph.edge("sleep", "runmem")
        _     <- graph.close
      } yield ref
      graph.show shouldBe
        """graph "G" {
          |  "run" -- "intr"
          |  "intr" -- "runbl"
          |  "runbl" -- "run"
          |  "run" -- "kernel"
          |  "kernel" -- "zombie"
          |  "kernel" -- "sleep"
          |  "kernel" -- "runmem"
          |  "sleep" -- "swap"
          |  "swap" -- "runswap"
          |  "runswap" -- "new"
          |  "runswap" -- "runmem"
          |  "new" -- "runmem"
          |  "sleep" -- "runmem"
          |}""".stripMargin
    }

    it("Huge graph") { // test for a stack overflow
      val graph = for {
        ref <- Ref[IO].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[IO]("G", DiGraph, ser)
        _   <- (1 to 1000).toList.traverse(i => g.edge(s"e$i" -> s"e${i + 1}"))
        _   <- g.close
      } yield ref
      graph.show // ignore
    }
  }

  implicit class RefOps(ref: IO[Ref[IO, StringBuffer]]) {
    def show: String = ref.flatMap(_.get).map(_.toString).unsafeRunSync
  }
}
