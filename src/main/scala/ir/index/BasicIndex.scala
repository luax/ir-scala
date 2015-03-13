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
  def cleanup(): Unit = ???
  def getDictionary(): java.util.Iterator[String] = ???

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

}