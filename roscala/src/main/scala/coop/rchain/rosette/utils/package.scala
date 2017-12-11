package coop.rchain.rosette

import java.io.{File, PrintWriter}

package object utils {
  def printToFile(f: File)(op: PrintWriter => Unit) {
    val p = new PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
