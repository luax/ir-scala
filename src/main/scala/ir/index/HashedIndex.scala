package ir.index

import scala.collection.mutable.HashMap
import ir.models.PostingsList
import ir.models.PostingsEntry
import ir.models.Query
import ir.Index
import java.util.LinkedList
import scala.collection.mutable.ListBuffer
import java.io._

class Dictionary {
  val r = 50
  val index = HashMap[String, PostingsList]()
  val topLists = HashMap[String, PostingsList]()

  def insert(token: String, docId: Int, offset: Int) {
    var list = index.getOrElseUpdate(token, new PostingsList(token))
    def addEntry(): PostingsEntry = {
      var entry = new PostingsEntry(docId)
      list += entry
      entry
    }
    var entry = list.lastOption.getOrElse(addEntry)
    if (entry.docId != docId) {
      entry = addEntry()
    }
    entry.offsets += offset
  }

  def get(token: String) = index.get(token)

  def getPostings(tokens: List[String], top: Boolean = false): List[PostingsList] = {
    var r = new ListBuffer[PostingsList]()
    for (token <- tokens) {
      var list = index.get(token)
      if (top) {
        list = topLists.get(token)
      }
      if (list.isDefined) {
        r += list.get
      }
    }
    r.toList
  }

  def computeTopLists() = {
    for ((token, list) <- index) {
      val idf = index(token).idf
      topLists(token) = list.sortBy((x) => -(x.tf * idf))
        .take(r)
        .foldLeft(new PostingsList(token))((x, y) => x += y)
    }
  }

}

class HashedIndex extends BasicIndex {
  val index = new Dictionary

  override def initialize() = index.computeTopLists

  override def getPostings(tokens: List[String]): List[PostingsList] = index.getPostings(tokens)

  override def insert(token: String, docId: Int, offset: Int) = index.insert(token, docId, offset)

  override def getPostings(token: String): Option[PostingsList] = index.get(token)

  override def search(query: Query, queryType: Int, rankingType: Int, structureType: Int): PostingsList = queryType match {
    case Index.INTERSECTION_QUERY => intersectionStrategy(query, index.getPostings(query.terms))
    case Index.PHRASE_QUERY       => phraseStrategy(query, index.getPostings(query.terms))
    case Index.RANKED_QUERY       => rankedStrategy(query, index.getPostings(query.terms, true), rankingType)
  }

}