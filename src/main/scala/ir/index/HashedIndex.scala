package ir.index

import scala.collection.mutable.HashMap
import ir.models.PostingsList
import ir.models.PostingsEntry
import ir.Query
import ir.Index
import java.util.LinkedList
import scala.collection.mutable.ListBuffer
import java.io._

class HashedIndex extends BasicIndex {
  val index = HashMap[String, PostingsList]()

  val pageRankPath = "./pagerank.ser"
  val pageRank = readPageRank

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
      case Index.RANKED_QUERY       => rankedStrategy(postingsList, rankingType)
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

  private def rankedStrategy(postingsLists: List[PostingsList], rankingType: Int): PostingsList = {
    val scores = new HashMap[Int, Double]()
    val result = new PostingsList

    if (rankingType == Index.PAGERANK) {
      return returnPageRank()
    }

    val score = (entry: PostingsEntry, list: PostingsList) => {
      val file = Index.docIDs.get(entry.docId.toString)
      val tfidf = entry.tf * list.idf / Index.docLengths.get(entry.docId.toString)
      rankingType match {
        case Index.COMBINATION => pageRank(file) * tfidf // 2 * pageRank(file) + 0.5 * tfidf
        case Index.TF_IDF      => tfidf
        // case Index.PAGERANK    => pageRank(file)
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

  private def returnPageRank(): PostingsList = {
    val reversed = new HashMap[String, Int]()
    val result = new PostingsList
    var set = Index.docIDs.entrySet()
    var it = set.iterator();
    while (it.hasNext()) {
      var pair = it.next();
      reversed(pair.getValue) = pair.getKey.toInt
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

}