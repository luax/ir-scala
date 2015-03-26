package ir.index

import scala.collection.mutable.HashMap
import ir.models.PostingsList
import ir.models.PostingsEntry
import ir.models.Query
import java.util.LinkedList
import scala.collection.mutable.ListBuffer
import java.io._

class HashedIndex extends BasicIndex {
  private class Index {
    private val r = 50
    private val index = HashMap[String, PostingsList]()
    private val topLists = HashMap[String, PostingsList]()

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
        topLists(token) = list.sortBy((x) => -(x.tf * idf) / Index.docLengths(x.docId))
          .take(r)
          .foldLeft(new PostingsList(token))((x, y) => x += y)
      }
    }

    def apply(term: String) = index(term)
    def top(term: String) = topLists(term)
  }

  private val index = new Index

  override def initialize() = index.computeTopLists

  override def insert(token: String, docId: Int, offset: Int) = index.insert(token, docId, offset)

  override def getPostings(token: String, top: Boolean = false): PostingsList = {
    if (!top) {
      index(token)
    } else {
      index.top(token)
    }
  }

  override def search(query: Query, queryType: QueryType, rankingType: RankingType): PostingsList = queryType match {
    case IntersectionQuery => intersectionStrategy(query, index.getPostings(query.terms))
    case PhraseQuery       => phraseStrategy(query, index.getPostings(query.terms))
    case RankedQuery       => rankedStrategy(query, index.getPostings(query.terms, true), rankingType)
  }

}