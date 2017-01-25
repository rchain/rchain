/*               __          __                                         *\
**    __________/ /_  ____ _/_/___                                      **
**   / ___/ ___/ __ \/ __ `/ / __ \     RChain API                      **
**  / /  / /__/ / / / /_/ / / / / /     (c) http://rchain.coop          **
** /_/   \___/_/ /_/\____/_/_/ /_/                                      **
\*                                                                      */

package coop.rchain.trie

import org.mongodb.scala._
import org.mongodb.scala.model.Filters

import org.json4s._
import org.json4s.jackson.JsonMethods._
import org.json4s.jackson.Serialization

import scala.concurrent.Await
import scala.concurrent.duration._

class MongoDB extends Store {
  //TODO move to config
  lazy private val client: MongoClient = MongoClient()
  lazy private val db: MongoDatabase = client.getDatabase("rchain")
  lazy val trie: MongoCollection[Document] = db.getCollection("trie")

  implicit val formats = Serialization.formats(NoTypeHints)

  implicit private def trieToDocument(t: Trie): Document = t match {
    case Node(id,p,v) if p.isEmpty => Document("_id" -> id, "v" -> v)
    case Node(id,p,v) if v == None => Document("_id" -> id, "sx" -> p.sx, "kx" -> p.kx)
    case Node(id,p,v) if v != None => Document("_id" -> id, "sx" -> p.sx, "kx" -> p.kx, "v" -> v)
  }

  implicit private def documentToTrie(doc: Document): Trie = {
    val json = parse(doc.toJson)
    Node((json \ "_id").extract[String], json.extract[SuffixMap], (json \ "v").extract[Option[String]])
  }

  implicit private def optionDocToOptionTrie(doc: Option[Document]):
    Option[Trie] = doc match {
      case None      => None
      case Some(doc) => Some(doc)
  }

  def get(id: String): Option[Trie] = getKey("_id", id)

  def put(t: Trie): Unit = trie.replaceOne(Filters.eq("_id", t.id), t).toFuture

  def insert(t: Trie): Unit = trie.insertOne(t).toFuture

  def delete(id: String): Unit = trie.deleteOne(Document("_id" -> id)).toFuture

  def getKey(k: String, v: String): Option[Trie] = getAwait(Document(k -> v)).headOption

  private def getAwait(query: Document) = Await.result(trie.find(query).toFuture, 1000.milliseconds)
}
