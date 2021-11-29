package coop.rchain.graphz

import cats._
import cats.data._
import cats.syntax.all._
import cats.mtl.implicits._

import org.scalatest._

class GraphzSpec extends FunSpec with Matchers with BeforeAndAfterEach with AppendedClues {

  type Effect[A] = State[StringBuffer, A]

  implicit val ser = new StringSerializer[Effect]

  describe("Graphz") {
    it("simple graph") {
      val graph = for {
        g <- Graphz[Effect]("G", Graph)
        _ <- g.close
      } yield g
      graph.show shouldBe (
        """graph "G" {
          |}""".stripMargin
      )
    }

    it("simple digraph") {
      val graph = for {
        g <- Graphz[Effect]("G", DiGraph)
        _ <- g.close
      } yield g
      graph.show shouldBe (
        """digraph "G" {
          |}""".stripMargin
      )
    }

    it("simple graph with comment") {
      val graph = for {
        g <- Graphz[Effect]("G", Graph, comment = Some("this is comment"))
        _ <- g.close
      } yield g
      graph.show shouldBe (
        """// this is comment
          |graph "G" {
          |}""".stripMargin
      )
    }

    it("graph, two nodes one edge") {
      // given
      val graph = for {
        g <- Graphz[Effect]("G", Graph)
        _ <- g.edge("Hello", "World")
        _ <- g.close
      } yield g
      // then
      graph.show shouldBe (
        """graph "G" {
          |  "Hello" -- "World"
          |}""".stripMargin
      )
    }

    it("digraph, two nodes one edge") {
      // given
      val graph = for {
        g <- Graphz[Effect]("G", DiGraph)
        _ <- g.edge("Hello", "World")
        _ <- g.close
      } yield g
      // then
      graph.show shouldBe (
        """digraph "G" {
          |  "Hello" -> "World"
          |}""".stripMargin
      )
    }

    it("digraph, nodes with style") {
      // given
      val graph = for {
        g <- Graphz[Effect]("G", DiGraph)
        _ <- g.node("Hello", shape = Box)
        _ <- g.node("World", shape = DoubleCircle)
        _ <- g.edge("Hello", "World")
        _ <- g.close
      } yield g
      // then
      graph.show shouldBe (
        """digraph "G" {
	  |  "Hello" [shape=box]
	  |  "World" [shape=doublecircle]
          |  "Hello" -> "World"
          |}""".stripMargin
      )
    }

    it("digraph with simple subgraphs") {
      val process1 = for {
        g <- Graphz.subgraph[Effect]("", DiGraph)
        _ <- g.node("A")
        _ <- g.node("B")
        _ <- g.node("C")
        _ <- g.edge("A", "B")
        _ <- g.edge("B", "C")
        _ <- g.close
      } yield g

      val process2 = for {
        g <- Graphz.subgraph[Effect]("", DiGraph)
        _ <- g.node("K")
        _ <- g.node("L")
        _ <- g.node("M")
        _ <- g.edge("K", "L")
        _ <- g.edge("L", "M")
        _ <- g.close
      } yield g

      val graph = for {
        g <- Graphz[Effect]("Process", DiGraph)
        _ <- g.node("0")
        _ <- g.subgraph(process1)
        _ <- g.edge("0", "A")
        _ <- g.subgraph(process2)
        _ <- g.edge("0", "K")
        _ <- g.node("1")
        _ <- g.edge("M", "1")
        _ <- g.edge("C", "1")
        _ <- g.close
      } yield g
      graph.show shouldBe (
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
      )
    }

    it("digraph with fancy subgraphs") {
      val process1 = for {
        g <- Graphz
              .subgraph[Effect](
                "cluster_p1",
                DiGraph,
                label = Some("process #1"),
                color = Some("blue")
              )
        _ <- g.node("A")
        _ <- g.node("B")
        _ <- g.node("C")
        _ <- g.edge("A", "B")
        _ <- g.edge("B", "C")
        _ <- g.close
      } yield g

      val process2 = for {
        g <- Graphz
              .subgraph[Effect](
                "cluster_p2",
                DiGraph,
                label = Some("process #2"),
                color = Some("green")
              )
        _ <- g.node("K")
        _ <- g.node("L")
        _ <- g.node("M")
        _ <- g.edge("K", "L")
        _ <- g.edge("L", "M")
        _ <- g.close
      } yield g

      val graph = for {
        g <- Graphz[Effect]("Process", DiGraph)
        _ <- g.node("0")
        _ <- g.subgraph(process1)
        _ <- g.edge("0", "A")
        _ <- g.subgraph(process2)
        _ <- g.edge("0", "K")
        _ <- g.node("1")
        _ <- g.edge("M", "1")
        _ <- g.edge("C", "1")
        _ <- g.close
      } yield g
      graph.show shouldBe (
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
      )
    }

    it("blockchain, simple") {
      // given
      val lvl1 = for {
        g <- Graphz.subgraph[Effect]("", DiGraph, rank = Some(Same))
        _ <- g.node("1")
        _ <- g.node("ddeecc", shape = Box)
        _ <- g.node("ffeeff", shape = Box)
        _ <- g.close
      } yield g

      val lvl0 = for {
        g <- Graphz.subgraph[Effect]("", DiGraph, rank = Some(Same))
        _ <- g.node("0")
        _ <- g.node("000000", shape = Box)
        _ <- g.close
      } yield g

      val timeline = for {
        g <- Graphz.subgraph[Effect]("timeline", DiGraph)
        _ <- g.node("3", shape = PlainText)
        _ <- g.node("2", shape = PlainText)
        _ <- g.node("1", shape = PlainText)
        _ <- g.node("0", shape = PlainText)
        _ <- g.edge("0" -> "1")
        _ <- g.edge("1" -> "2")
        _ <- g.edge("2" -> "3")
        _ <- g.close
      } yield g

      val graph = for {
        g <- Graphz[Effect]("Blockchain", DiGraph, rankdir = Some(BT))
        _ <- g.subgraph(lvl1)
        _ <- g.edge("000000" -> "ffeeff")
        _ <- g.edge("000000" -> "ddeecc")
        _ <- g.subgraph(lvl0)
        _ <- g.subgraph(timeline)
        _ <- g.close
      } yield g
      // then
      graph.show shouldBe (
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
      )
    }

    // from https://github.com/xflr6/graphviz/blob/master/examples/process.py
    it("Process example") {
      (for {
        graph <- Graphz[Effect]("G", Graph)
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
      } yield graph).show shouldBe (
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
      )
    }

    it("Huge graph") { // test for a stack overflow
      val graph = for {
        g <- Graphz[Effect]("G", DiGraph)
        _ <- (1 to 1000).toList.traverse(i => g.edge(s"e$i" -> s"e${i + 1}"))
        _ <- g.close
      } yield g
      graph.show // ignore
    }
  }

  implicit class GraphzOps(graph: Effect[Graphz[Effect]]) {
    def show: String =
      graph.runS(new StringBuffer).value.toString

    import java.io.{File, PrintWriter}

    def view(): Unit = {
      val sourcePath = "/Users/rabbit/temp.gv"
      val outputPath = "/Users/rabbit/output.pdf"
      new File(sourcePath).createNewFile()
      val writer = new PrintWriter(sourcePath)
      writer.println(show)
      writer.flush()
      writer.close
      val dotCmd  = s"dot -Tpdf $sourcePath -o $outputPath"
      val openCmd = s"open $outputPath"
      import sys.process._
      (dotCmd !)
      (openCmd !)
    }
  }
}
