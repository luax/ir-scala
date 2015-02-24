package ir.index

import scala.collection.mutable.HashMap
import ir.models.PostingsList
import ir.models.PostingsEntry
import ir.Query
import ir.Index
import java.util.LinkedList
import scala.collection.mutable.ListBuffer

class HashedIndex extends BasicIndex {
  val index: HashMap[String, PostingsList] = new HashMap[String, PostingsList]()

  override def getPostings(tokens: LinkedList[String]): List[PostingsList] = {
    var r = new ListBuffer[PostingsList]()
    var i = tokens.iterator
    while (i.hasNext) {
      var list = getPostings(i.next)
      if (list.isDefined) {
        r += list.get
      }
    }
    r.toList
  }

  override def insert(token: String, docId: Int, offset: Int) {
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

  override def getPostings(token: String): Option[PostingsList] = index.get(token)

  override def search(query: ir.Query, queryType: Int, rankingType: Int, structureType: Int): PostingsList = {
    var postingsList = getPostings(query.terms)
    if (postingsList.isEmpty) return new PostingsList("")
    queryType match {
      case Index.INTERSECTION_QUERY => intersectionStrategy(postingsList)
      case Index.PHRASE_QUERY       => phraseStrategy(postingsList)
      case Index.RANKED_QUERY       => rankedStrategy(postingsList)
    }
  }

  private def phraseStrategy(postingsLists: List[PostingsList]): PostingsList = {
    val addResult = (left: PostingsEntry, right: PostingsEntry, result: PostingsList) => {
      left.offsets.foreach { x =>
        {
          if (right.offsets.contains(x + 1)) {
            var entryOption = result.lastOption
            if (entryOption.isDefined && entryOption.get.docId == left.docId) {
              entryOption.get.offsets += x + 1
            } else {
              var entry = new PostingsEntry(left.docId)
              entry.offsets += x + 1
              result += entry
            }
          }
        }
      }
    }
    postingsLists
      .drop(1)
      .foldLeft(postingsLists(0))((b, a) => intersect(b.iterator, a.iterator, addResult))
  }

  private def intersectionStrategy(postingsLists: List[PostingsList]): PostingsList = {
    val addResult = (left: PostingsEntry, right: PostingsEntry, result: PostingsList) => {
      result += new PostingsEntry(left.docId)
    }: Unit
    // Sort all postings in increasing order and intersect
    postingsLists
      .sortBy { x => x.size }
      .foldLeft(postingsLists(0))((b, a) => intersect(b.iterator, a.iterator, addResult))
  }

  private def rankedStrategy(postingsLists: List[PostingsList]): PostingsList = {
    var scores = new HashMap[Int, Double]()
    var result = new PostingsList
  
    val score = (entry: PostingsEntry, list: PostingsList) =>
      entry.tf * list.idf / Index.docLengths.get(entry.docId.toString)

    postingsLists.foreach { list =>
      list.foreach { entry =>
        scores(entry.docId) = scores.getOrElse(entry.docId, 0.0) + score(entry, list)
      }
    }

    scores
      .toSeq
      .sortBy(-_._2)
      .foreach((elem: (Int, Double)) => {
        result += new PostingsEntry(elem._1, elem._2)
      })

    result
  }

  private def intersect(p1: Iterator[PostingsEntry], p2: Iterator[PostingsEntry], addResult: (PostingsEntry, PostingsEntry, PostingsList) => Unit): PostingsList = {
    val nextPost = (p1: Iterator[PostingsEntry]) => {
      if (p1.hasNext) p1.next() else null
    }
    var result = new PostingsList("")
    var left = nextPost(p1)
    var right = nextPost(p2)
    while (left != null && right != null) {
      if (left.docId == right.docId) {
        addResult(left, right, result)
        left = nextPost(p1)
        right = nextPost(p2)
      } else if (left.docId < right.docId) {
        left = nextPost(p1)
      } else {
        right = nextPost(p2)
      }
    }
    result
  }

}