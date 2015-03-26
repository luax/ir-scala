package ir.index

import scala.collection.mutable.HashMap
import ir.models.Query
import ir.models.PostingsList

trait QueryType
case object IntersectionQuery extends QueryType
case object PhraseQuery extends QueryType
case object RankedQuery extends QueryType

trait RankingType
case object TfIdf extends RankingType
case object Pagerank extends RankingType
case object Combination extends RankingType

trait StructureType
case object Unigram extends StructureType
case object Bigram extends StructureType
case object Subphrase extends StructureType

trait Index {
  def initialize(): Unit
  def insert(token: String, docId: Int, position: Int): Unit
  def search(query: Query, queryType: QueryType, rankingType: RankingType): PostingsList
  def getPostings(term: String, top: Boolean = false): PostingsList
}

object Index {
  val docFilepaths = new HashMap[Int, String]
  val docLengths = new HashMap[Int, Int]
  def getFilename(filePath: String) = {
    var s = "([\\w-]+)\\.".r.findFirstIn(filePath).get
    s.substring(0, s.length - 1)
  }
}