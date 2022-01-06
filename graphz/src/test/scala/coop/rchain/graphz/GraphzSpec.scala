package coop.rchain.graphz

import cats.effect.concurrent.Ref
import cats.syntax.all._
import monix.eval.Task
import monix.execution.Scheduler.Implicits.global
import org.scalatest._

class GraphzSpec extends FunSpec with Matchers with BeforeAndAfterEach with AppendedClues {

  describe("Graphz") {
    it("simple graph") {
      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        g       <- Graphz[Task]("G", Graph, ser)
        _       <- g.close
        content <- g.show
      } yield {
        content shouldBe
          """graph "G" {
          |}""".stripMargin
      }).runSyncUnsafe()
    }

    it("simple digraph") {
      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        g       <- Graphz[Task]("G", DiGraph, ser)
        _       <- g.close
        content <- g.show
      } yield {
        content shouldBe
          """digraph "G" {
          |}""".stripMargin
      }).runSyncUnsafe()
    }

    it("simple graph with comment") {
      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        g       <- Graphz[Task]("G", Graph, ser, comment = Some("this is comment"))
        _       <- g.close
        content <- g.show
      } yield {
        content shouldBe
          """// this is comment
          |graph "G" {
          |}""".stripMargin
      }).runSyncUnsafe()
    }

    it("graph, two nodes one edge") {
      // given
      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        g       <- Graphz[Task]("G", Graph, ser)
        _       <- g.edge("Hello", "World")
        _       <- g.close
        content <- g.show
      }
      // then
      yield {
        content shouldBe
          """graph "G" {
          |  "Hello" -- "World"
          |}""".stripMargin
      }).runSyncUnsafe()
    }

    it("digraph, two nodes one edge") {
      // given
      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        g       <- Graphz[Task]("G", DiGraph, ser)
        _       <- g.edge("Hello", "World")
        _       <- g.close
        content <- g.show
      }
      // then
      yield {
        content shouldBe
          """digraph "G" {
          |  "Hello" -> "World"
          |}""".stripMargin
      }).runSyncUnsafe()
    }

    it("digraph, nodes with style") {
      // given
      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        g       <- Graphz[Task]("G", DiGraph, ser)
        _       <- g.node("Hello", shape = Box)
        _       <- g.node("World", shape = DoubleCircle)
        _       <- g.edge("Hello", "World")
        _       <- g.close
        content <- g.show
      }
      // then
      yield {
        content shouldBe
          """digraph "G" {
          |  "Hello" [shape=box]
          |  "World" [shape=doublecircle]
          |  "Hello" -> "World"
          |}""".stripMargin
      }).runSyncUnsafe()
    }

    it("digraph with simple subgraphs") {
      val process1 = for {
        ref <- Ref[Task].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz.subgraph[Task]("", DiGraph, ser)
        _   <- g.node("A")
        _   <- g.node("B")
        _   <- g.node("C")
        _   <- g.edge("A", "B")
        _   <- g.edge("B", "C")
        _   <- g.close
      } yield g

      val process2 = for {
        ref <- Ref[Task].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz.subgraph[Task]("", DiGraph, ser)
        _   <- g.node("K")
        _   <- g.node("L")
        _   <- g.node("M")
        _   <- g.edge("K", "L")
        _   <- g.edge("L", "M")
        _   <- g.close
      } yield g

      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        g       <- Graphz[Task]("Process", DiGraph, ser)
        _       <- g.node("0")
        p1      <- process1
        _       <- g.subgraph(p1)
        _       <- g.edge("0", "A")
        p2      <- process2
        _       <- g.subgraph(p2)
        _       <- g.edge("0", "K")
        _       <- g.node("1")
        _       <- g.edge("M", "1")
        _       <- g.edge("C", "1")
        _       <- g.close
        content <- g.show
      } yield {
        content shouldBe
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
      }).runSyncUnsafe()
    }

    it("digraph with fancy subgraphs") {
      val process1 = for {
        ref <- Ref[Task].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g <- Graphz
              .subgraph[Task](
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
      } yield g

      val process2 = for {
        ref <- Ref[Task].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g <- Graphz
              .subgraph[Task](
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
      } yield g

      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        g       <- Graphz[Task]("Process", DiGraph, ser)
        _       <- g.node("0")
        p1      <- process1
        _       <- g.subgraph(p1)
        _       <- g.edge("0", "A")
        p2      <- process2
        _       <- g.subgraph(p2)
        _       <- g.edge("0", "K")
        _       <- g.node("1")
        _       <- g.edge("M", "1")
        _       <- g.edge("C", "1")
        _       <- g.close
        content <- g.show
      } yield {
        content shouldBe
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
      }).runSyncUnsafe()
    }

    it("blockchain, simple") {
      // given
      val lvl1 = for {
        ref <- Ref[Task].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz.subgraph[Task]("", DiGraph, ser, rank = Some(Same))
        _   <- g.node("1")
        _   <- g.node("ddeecc", shape = Box)
        _   <- g.node("ffeeff", shape = Box)
        _   <- g.close
      } yield g

      val lvl0 = for {
        ref <- Ref[Task].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz.subgraph[Task]("", DiGraph, ser, rank = Some(Same))
        _   <- g.node("0")
        _   <- g.node("000000", shape = Box)
        _   <- g.close
      } yield g

      val timeline = for {
        ref <- Ref[Task].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz.subgraph[Task]("timeline", DiGraph, ser)
        _   <- g.node("3", shape = PlainText)
        _   <- g.node("2", shape = PlainText)
        _   <- g.node("1", shape = PlainText)
        _   <- g.node("0", shape = PlainText)
        _   <- g.edge("0" -> "1")
        _   <- g.edge("1" -> "2")
        _   <- g.edge("2" -> "3")
        _   <- g.close
      } yield g

      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        g       <- Graphz[Task]("Blockchain", DiGraph, ser, rankdir = Some(BT))
        l1      <- lvl1
        _       <- g.subgraph(l1)
        _       <- g.edge("000000" -> "ffeeff")
        _       <- g.edge("000000" -> "ddeecc")
        l0      <- lvl0
        _       <- g.subgraph(l0)
        tl      <- timeline
        _       <- g.subgraph(tl)
        _       <- g.close
        content <- g.show
      }
      // then
      yield {
        content shouldBe
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
      }).runSyncUnsafe()
    }

    // from https://github.com/xflr6/graphviz/blob/master/examples/process.py
    it("Process example") {
      (for {
        ref     <- Ref[Task].of(new StringBuffer(""))
        ser     = new StringSerializer(ref)
        graph   <- Graphz[Task]("G", Graph, ser)
        _       <- graph.edge("run", "intr")
        _       <- graph.edge("intr", "runbl")
        _       <- graph.edge("runbl", "run")
        _       <- graph.edge("run", "kernel")
        _       <- graph.edge("kernel", "zombie")
        _       <- graph.edge("kernel", "sleep")
        _       <- graph.edge("kernel", "runmem")
        _       <- graph.edge("sleep", "swap")
        _       <- graph.edge("swap", "runswap")
        _       <- graph.edge("runswap", "new")
        _       <- graph.edge("runswap", "runmem")
        _       <- graph.edge("new", "runmem")
        _       <- graph.edge("sleep", "runmem")
        _       <- graph.close
        content <- graph.show
      } yield {
        content shouldBe
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
      }).runSyncUnsafe()
    }

    it("Huge graph") { // test for a stack overflow
      val graph = for {
        ref <- Ref[Task].of(new StringBuffer(""))
        ser = new StringSerializer(ref)
        g   <- Graphz[Task]("G", DiGraph, ser)
        _   <- (1 to 1000).toList.traverse(i => g.edge(s"e$i" -> s"e${i + 1}"))
        _   <- g.close
        _   <- g.show
      } yield g
      graph // ignore
    }
  }

  implicit class GraphzOps(graph: Graphz[Task]) {

    import java.io.{File, PrintWriter}

    def view(): Unit = {
      val sourcePath = "/Users/rabbit/temp.gv"
      val outputPath = "/Users/rabbit/output.pdf"
      new File(sourcePath).createNewFile()
      val writer = new PrintWriter(sourcePath)
      writer.println(graph)
      writer.flush()
      writer.close()
      val dotCmd  = s"dot -Tpdf $sourcePath -o $outputPath"
      val openCmd = s"open $outputPath"
      import sys.process._
      (dotCmd !)
      (openCmd !)
    }
  }
}
