package ir.index

import ir.models.PostingsList
import ir.models.PostingsEntry
import ir.models.Query
import ir.Index
import java.util.LinkedList
import java.io._
import scala.collection.mutable.HashMap

abstract class BasicIndex extends Index {
  val pageRankPath = "./data/pagerank.ser"
  val pageRank = readPageRank

  def getPostings(token: String): Option[PostingsList]
  def getPostings(tokens: List[String]): List[PostingsList]
  def insert(token: String, docID: Int, offset: Int): Unit
  def search(query: Query, queryType: Int, rankingType: Int, structureType: Int): PostingsList
  def initialize(): Unit

  protected def returnPageRank(): PostingsList = {
    val reversed = new HashMap[String, Int]()
    val result = new PostingsList
    var docFilepaths = Index.docFilepaths.entrySet()
    var it = docFilepaths.iterator();
    while (it.hasNext()) {
      var pair = it.next()
      reversed(getFilename(pair.getValue)) = pair.getKey.toInt
    }

    pageRank
      .toSeq
      .sortBy(-_._2)
      .foreach((elem: (String, Double)) => {
        if (reversed.contains(elem._1))
          result += new PostingsEntry(reversed(elem._1), elem._2)
      })
    result
  }

  private def readPageRank(): HashMap[String, Double] = {
    val fis = new FileInputStream(pageRankPath)
    val ois = new ObjectInputStream(fis)
    val pageRank = ois.readObject.asInstanceOf[HashMap[String, Double]]
    ois.close
    pageRank
  }

  protected def getFilename(filePath: String) = {
    // TODO: better
    var s = "([\\w-]+)\\.".r.findFirstIn(filePath).get
    s.substring(0, s.length - 1)
  }

  def phraseStrategy(query: Query, postingsLists: List[PostingsList]): PostingsList = {
    if (postingsLists.isEmpty) return new PostingsList

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
      .foldLeft(postingsLists(0))((res, list) => intersect(res.iterator, list.iterator, addResult))
  }

  def intersectionStrategy(query: Query, postingsLists: List[PostingsList]): PostingsList = {
    if (postingsLists.isEmpty) return new PostingsList
    val addResult = (left: PostingsEntry, right: PostingsEntry, result: PostingsList) => {
      result += new PostingsEntry(left.docId)
    }: Unit
    // Sort all postings in increasing order and intersect
    postingsLists
      .sortBy(_.size)
      .foldLeft(postingsLists(0))((res, list) => intersect(res.iterator, list.iterator, addResult))
  }

  def rankedStrategy(query: Query, postingsLists: List[PostingsList], rankingType: Int): PostingsList = {
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
    val nextPost = (p: Iterator[PostingsEntry]) => if (p.hasNext) p.next else null
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