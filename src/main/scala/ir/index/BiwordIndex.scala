package ir.index

import scala.collection.mutable.HashMap
import ir.models.PostingsList
import ir.models.PostingsEntry
import ir.models.Query
import java.util.LinkedList
import scala.collection.mutable.ListBuffer
import java.io._

class BiwordIndex extends BasicIndex {
  private class BiIndex extends Serializable {
    private val index = HashMap[String, PostingsList]()

    private var previousDocId = -1
    private var previousToken = ""
    private var previousOffset = -1

    def insert(token: String, docId: Int, offset: Int) {
      if (docId == previousDocId) {
        var newToken = previousToken + " " + token
        var list = index.getOrElseUpdate(newToken, new PostingsList(newToken))
        def addEntry(): PostingsEntry = {
          var entry = new PostingsEntry(docId)
          list += entry
          entry
        }
        var entry = list.lastOption.getOrElse(addEntry)
        if (entry.docId != docId) {
          entry = addEntry()
        }
        entry.offsets += previousOffset
      }
      previousDocId = docId
      previousToken = token
      previousOffset = offset
    }

    def getPostings(bigrams: List[String]): List[PostingsList] = {
      println("Get postings: " + bigrams)
      var r = new ListBuffer[PostingsList]()
      for (bigram <- bigrams) {
        var list = index.get(bigram)
        if (list.isDefined) {
          r += list.get
        }
      }
      r.toList
    }

    def apply(term: String) = index(term)
    def size() = index.size
  }
  private val index = new BiIndex

  override def initialize() = { println("Number of bigrams: " + index.size) }

  override def insert(token: String, docId: Int, offset: Int) = index.insert(token, docId, offset)

  override def getPostings(token: String, top: Boolean = false): PostingsList = index(token)

  override def search(query: Query, queryType: QueryType, rankingType: RankingType): PostingsList = queryType match {
    case RankedQuery => rankedStrategy(query, index.getPostings(query.terms), rankingType)
    case _           => println("*** Unsupported bigram query ***"); new PostingsList
  }

}