package ir.index

import ir.models._

class SubphraseIndex extends BasicIndex {
  private val unigramIndex = new HashedIndex
  private val bigramIndex = new BiwordIndex

  override def initialize() = {
    unigramIndex.initialize
    bigramIndex.initialize
  }

  override def insert(token: String, docId: Int, offset: Int) = {
    unigramIndex.insert(token, docId, offset)
    bigramIndex.insert(token, docId, offset)
  }

  override def search(query: Query, queryType: QueryType, rankingType: RankingType): PostingsList = {
    if (queryType != RankedQuery) {
      throw new UnsupportedOperationException("Unsupported query type: " + queryType)
    }
    if (query.terms.size <= 1) {
      println("Returning unigram")
      return unigramIndex.search(query, queryType, rankingType)
    }
    val k = 25
    val biQuery = new Query(query.terms.mkString(" "), Bigram)
    val bigramResult = bigramIndex.search(biQuery, queryType, rankingType)
    if (bigramResult.size >= k) {
      println("Returning bigram")
      return bigramResult
    }
    val unigramResult = unigramIndex.search(query, queryType, rankingType)
    if (bigramResult.size != 0 && unigramResult.size != 0) {
      // Weigh bigram so that it is always better
      val w = bigramResult.last.score.compare(unigramResult.head.score) match {
        case 1 => 0
        case _ => (unigramResult.head.score - bigramResult.last.score) * 1.1
      }
      bigramResult.foreach((x) => x.score += w)
    }
    val result = new PostingsList("")
    (bigramResult ++ unigramResult).foreach((x) => result += x)
    println("Returning bigram ++ unigram")
    return result
  }

  override def getPostings(token: String, top: Boolean = false): PostingsList = throw new UnsupportedOperationException

}