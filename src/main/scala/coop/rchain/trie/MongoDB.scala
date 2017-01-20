/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

import org.mongodb.scala._
import org.mongodb.scala.model.Filters._
import java.util.concurrent.TimeUnit
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization
import org.json4s.jackson.Serialization.{read, write}

object MongoDB {
  private val client: MongoClient = MongoClient()
  private val db: MongoDatabase = client.getDatabase("rchain")
  val trie: MongoCollection[Document] = db.getCollection("trie")
  
  implicit val formats = Serialization.formats(NoTypeHints)
  
  implicit private def trieToDocument(t: Trie): Document = {
    Document("_id" -> t.id, "sx" -> t.suffixes, "kx" -> t.keys, "v" -> t.value)
  }
  
  implicit private def documentToTrie(doc: Document): Trie = {
    val json = parse(doc.toJson).transformField {
      case ("sx", x) => ("suffixes", x)
      case ("kx", x) => ("keys",  x)
      case ("v",  x) => ("value", x)
    }
    json.extract[NonEmptyTrie]
  }
  
  def get(k: String) = {
    val query = Document("_id" -> k) 
    trie.find(query).subscribe(
      (doc: Document) => documentToTrie(doc),
      (error: Throwable) => println(s"Query failed: ${error.getMessage}")
    )
  }

  def put(t: Trie): Unit = {
    trie.insertOne(trieToDocument(t)).subscribe(
      (result: Completed) => println(s"MongoDB put: $t"),
      (error: Throwable) => println(s"Query failed: ${error.getMessage}")
    )
  }
  
  def delete(k: String) = {
    println(s"MongoDB delete $k")
    Await.result(trie.deleteOne(Document("_id" -> k)).toFuture, Duration(5, TimeUnit.SECONDS ))
  }
  
  def getNode(id: String): Trie = {
    val results = getResult(Document("_id" -> id) )
    results(0)
  }
  
  def getParent(childId: String): Option[Trie] = {
    val results = getResult(Document("kx" -> childId))
    if(results.isEmpty) None
    else Some(results(0))
  }
 
  private def getResult(query: Document) = {
    Await.result(trie.find(query).toFuture, Duration(5, TimeUnit.SECONDS))
  }
}