package ir.index

import scala.collection.mutable.HashMap
import ir.models.PostingsList
import ir.models.PostingsEntry
import ir.models.Query
import ir.Index
import java.util.LinkedList
import scala.collection.mutable.ListBuffer
import java.io._

class HashedIndex extends BasicIndex {
  val index = HashMap[String, PostingsList]()

  override def getPostings(tokens: List[String]): List[PostingsList] = {
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

  override def search(query: Query, queryType: Int, rankingType: Int, structureType: Int): PostingsList = {
    var postingsLists = getPostings(query.terms)
    if (postingsLists.isEmpty) return new PostingsList("")
    queryType match {
      case Index.INTERSECTION_QUERY => intersectionStrategy(query, postingsLists)
      case Index.PHRASE_QUERY       => phraseStrategy(query, postingsLists)
      case Index.RANKED_QUERY       => rankedStrategy(query, postingsLists, rankingType)
    }
  }

  private def phraseStrategy(query: Query, postingsLists: List[PostingsList]): PostingsList = {
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

  private def intersectionStrategy(query: Query, postingsLists: List[PostingsList]): PostingsList = {
    val addResult = (left: PostingsEntry, right: PostingsEntry, result: PostingsList) => {
      result += new PostingsEntry(left.docId)
    }: Unit
    // Sort all postings in increasing order and intersect
    postingsLists
      .sortBy { x => x.size }
      .foldLeft(postingsLists(0))((b, a) => intersect(b.iterator, a.iterator, addResult))
  }

  private def rankedStrategy(query: Query, postingsLists: List[PostingsList], rankingType: Int): PostingsList = {
    val scores = new HashMap[Int, Double]()
    val result = new PostingsList

    if (rankingType == Index.PAGERANK) {
      return returnPageRank
    }

    val score = (entry: PostingsEntry, list: PostingsList) => {
      val file = getFilename(Index.docFilepaths.get(entry.docId))
      val tfidf = entry.tf * list.idf
      // w_t,d = tf_t,d * log(N / df_t)
      val wtd = tfidf / Index.docLengths.get(entry.docId)
      val wtq = query.score(list.token)
      val w1 = 2
      val w2 = 1
      rankingType match {
        case Index.TF_IDF      => wtd * wtq
        case Index.COMBINATION => w1 * pageRank.getOrElse(file, 0.0) + w2 * wtd * wtq
      }
    }

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
    var result = new PostingsList()
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