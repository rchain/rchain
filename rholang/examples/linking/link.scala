import java.io.{File, PrintWriter}

import scala.annotation.tailrec
import scala.io.Source
import scala.util.Try

/**
 * Provides facility for "linking" Rholang source code. Linking is done
 * by trans-piling extended Rholang source into standard Rholang source.
 * The extended Rholang includes two new keywords: `export` and `import`.
 * These two keywords work very similarly to the `new` keyword in standard
 * Rholang, but `export` has the restriction that only a single 
 * name can be declared (i.e. `export x, y in { ... }` would
 * be INVALID). Also note that `export` and `import` declarations can only
 * appear at the "top level" of a file -- i.e. NOT inside `contract` definitions,
 * bodies of `for` statements or `match` cases, etc. `export`s can 
 * use `import`s from other packages so long as there is not a loop of `import`s
 * (e.g. if Y imports X then X cannot import Y or anything that depends on Y).
 *
 * When this linker is used on a Rholang source containing the
 * `import` keyword, the import is mapped into a standard `new` statement, but with
 * the code block following the `in` extended to include the code from
 * the corresponding `export` declaration (which can, and should, reside in
 * a separate file). 
 *
 * Example: 
 * Let's say X.rho contains
 *   export X in { contract X(input, return) = { return( 2 * input ) } }
 * and that Y.rho contains
 *   import X in { new Y in { contract Y(return) = { X(5, return) } } }
 * Then linking Y.rho would result in the file Y.rho.linked with the contents
 *    new X in {
 *      contract X(input, return) = { return( 2 * input ) } | 
 *      new Y in { contract Y(return) = { X(5, return) } }
 *    }
 *
 * USAGE: scala link.scala <rholangSource> <libraryDirectory>
 * Where <rholangSource> is the file to be linked with packages (i.e. imports resolved)
 * and <libraryDirectory> is a directory with all the Rholang sources with
 * `export` declarations.
 */
object RholangLinker {

  def readRhoFile(f: File): String = {
    Source.fromFile(f).getLines()
      .map(_.split("//").head) //ignore comments
      .map(_.trim)
      .filter(_.nonEmpty)
      .mkString("\n")
  }
  
  def splitOnPars(code: String): Vector[String] = {
    code.foldLeft((List.empty[BlockDef], Vector(""))){
      case ((blockingList, currResult), c) =>
        if (blockingList.isEmpty) {
          c match {
            case '|' => (Nil, currResult.padTo(currResult.length + 1, ""))
            case b if b == BlockDef.brace.open => 
              (BlockDef.brace :: blockingList, currResult.updated(currResult.length - 1, currResult.last + c))
            case q if q == BlockDef.quote.open => 
              (BlockDef.quote :: blockingList, currResult.updated(currResult.length - 1, currResult.last + c))
            case _ => (blockingList, currResult.updated(currResult.length - 1, currResult.last + c))
          }
        } else {
          val blocker = blockingList.head
          c match {
            case close if close == blocker.close =>
              (blockingList.tail, currResult.updated(currResult.length - 1, currResult.last + c))
            case b if b == BlockDef.brace.open => 
              (BlockDef.brace :: blockingList, currResult.updated(currResult.length - 1, currResult.last + c))
            case q if q == BlockDef.quote.open => 
              (BlockDef.quote :: blockingList, currResult.updated(currResult.length - 1, currResult.last + c))
            case _ => (blockingList, currResult.updated(currResult.length - 1, currResult.last + c))
          }
        }
    }._2.map(_.trim)
  }
  
  def findCloseBrace(startIndex: Int, str: String): Int = {
    val chars = str
      .iterator
      .zipWithIndex
      .dropWhile{ case (_, i) => i <= startIndex }
    
    @tailrec
    def search(chars: Iterator[(Char, Int)], blockingList: List[BlockDef] = List(BlockDef.brace)): Int = {
      val (c, i) = Try(chars.next()).getOrElse((' ', str.length))
      if (blockingList.isEmpty || i == str.length) {
        if(blockingList.nonEmpty){ throw new Exception("Imbalanced braces!") }
        i - 1
      } else {
        val blocker = blockingList.head
        c match {
            case close if close == blocker.close => search(chars, blockingList.tail)
              
            case b if b == BlockDef.brace.open => search(chars, BlockDef.brace :: blockingList)

            case q if q == BlockDef.quote.open => search(chars, BlockDef.quote :: blockingList)

            case _ => search(chars, blockingList)
          }
      }
    }
    
    search(chars)
  }
  
  def parseNamedBlock(typ: String, code: String): Option[NamedBlock] = {
    val blockStart = code.indexOf(typ)
    val inIndex = code.indexOf("in ", blockStart)
    if (blockStart < 0 || inIndex < 0) {
      None
    } else {
      
      val name = code.slice(blockStart + typ.length, inIndex).trim()
      val openBrace = code.indexOf("{", inIndex)
      val closeBrace = findCloseBrace(openBrace, code)
      val internalCode = code.slice(openBrace + 1, closeBrace).trim()
    
      Some(NamedBlock(typ, name, parseRholang(internalCode)))
    }
  }
  
  
  def parseRholang(code: String): Code = {
    val split = splitOnPars(code)
    if (split.length > 1) {
      Par(split.map(parseRholang))
    } else if (code.startsWith("export")) {
      parseNamedBlock("export", code).getOrElse(Base(code))
    } else if (code.startsWith("import")) {
      parseNamedBlock("import", code).getOrElse(Base(code))
    } else if (code.startsWith("new ")) {
      parseNamedBlock("new", code).getOrElse(Base(code))
    } else {
      Base(code)
    }
  }
  
  def findPackages(code: Code, acc: List[NamedBlock] = Nil): List[NamedBlock] = {
    code match {
      case b: Base => acc
      case p: Par => acc ++ p.processes.toList.flatMap(c => findPackages(c))
      case nb : NamedBlock if nb.typ == "export" => nb :: acc
      case nb: NamedBlock => findPackages(nb.code) ++ acc
    }
  }
  
  def readPackages(dir: String): Vector[NamedBlock] = {
    val d = new File(dir)
    
    val rhoFiles = if(d.exists && d.isDirectory) {
      d.listFiles.filter(f => f.isFile && f.getName.endsWith("rho")).toVector
    } else {
      throw new Exception(s"$dir is not a directory!")
    }
    
    resolvePackageImports(
      rhoFiles.flatMap(f => {
        val code = parseRholang(readRhoFile(f))
        findPackages(code)
      })
    )
  }
  
  def mapImports(code: Code, packages: Vector[NamedBlock]): Code = {
    def f(c: Code): Code = {
      c match {
        case im: NamedBlock if im.typ == "import" =>
          val names = im.name.split(",").map(_.trim).toVector
          val packageCode = names.flatMap(n => packages.find(_.name == n).map(_.code))
          if (packageCode.length != names.length) {
            println("Warning! The following packages were not found in lib. directory:")
            println(names.filter(n => packages.find(_.name == n).isEmpty).mkString(", "))
          }
          val inBlockCode = f(im.code)
          val fullCode = Par(packageCode ++ Vector(inBlockCode))
          NamedBlock("new", im.name, fullCode)
        
        case im: NamedBlock => NamedBlock(im.typ, im.name, f(im.code))
        
        case par: Par => Par(par.processes.map(f))
        
        case b: Base => b
      }
    }
    
    f(code)
  }
  
  @tailrec
  def resolvePackageImports(packages: Vector[NamedBlock], count: Int = 0): Vector[NamedBlock] = {
    val (basePackages, packagesWithImports) = packages.partition(_.numImports == 0)
    
    if(packagesWithImports.isEmpty) {
      basePackages
    } else if (basePackages.isEmpty || count > 10) {
      throw new Exception(
        "Could not resolve dependencies within packages. Likely cause: packages which mutually import each other."
      )
    } else {
      resolvePackageImports(
        basePackages ++ packagesWithImports.map(p => mapImports(p, basePackages).asInstanceOf[NamedBlock]), 
        count + 1
      )
    }
  }
 
  def printUsage(): Unit = {
    val usage = "USAGE: scala link.scala <rholangSource> <libraryDirectory>"
    println(usage)
  }
 
  def main(args: Array[String]) : Unit = {
    if (args.length != 2) {
      printUsage()
    } else {
      val Array(filename, libDir) = args
      val packages = readPackages(libDir)
      
      if (packages.iterator.map(_.name).toSet.size != packages.length) {
        throw new Exception("Error! Cannot have duplicate exports!")
      }
      
      val parsedInput = parseRholang(readRhoFile(new File(filename)))
      val subedCode = mapImports(parsedInput, packages)
 
      val outFilename = filename + ".linked"
      val output = new PrintWriter(outFilename)
      output.println(subedCode.toString)
      output.close()
      println(s"Wrote $outFilename")
    }
  }
}

case class BlockDef(open: Char, close: Char)
object BlockDef {
  val brace = BlockDef(123.toChar, 125.toChar)
  val quote = BlockDef(34.toChar, 34.toChar)
}

sealed trait Code {
  def numImports: Int
}
case class Par(processes: Vector[Code]) extends Code {
  override def numImports: Int = processes.iterator.map(_.numImports).sum
  
  override def toString: String = {
    processes.map(_.toString).mkString(" | \n")
  }
}
case class Base(code: String) extends Code {
  override def numImports: Int = 0

  override def toString: String = code
}
case class NamedBlock(typ: String, name: String, code: Code) extends Code {
  override def numImports: Int = {
    val baseNumber = if (typ == "import") 1 else 0
    baseNumber + code.numImports
  }

  override def toString: String = {
    s"new $name in {\n" + code.toString + "\n}"
  }
}