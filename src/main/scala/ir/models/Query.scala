package ir.models

import ir.Indexer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import ir.Index

class Query(query: String) {
  private val termScore: HashMap[String, Double] = new HashMap[String, Double]()
  private val α = 1.0
  private val β = 0.5
  val terms = query.split(' ').toList

  // Initial scores
  terms.foreach(termScore(_) = 1.0) // TODO: idf_t score?

  def score(term: String) = termScore(term)

  def relevanceFeedback(result: PostingsList, docIsRelevant: Array[Boolean], indexer: Indexer) {
    val relevantDocs = result
      .zipWithIndex
      .take(docIsRelevant.length)
      .filter((r) => docIsRelevant(r._2))
      .map(x => new Document(x._1.docId))

    // Multiply alpha and normalize
    for ((k, v) <- termScore) {
      termScore(k) = (v * α) * 1 / termScore.size
    }

    // Add second vector
    for (doc <- relevantDocs) {
      for (term <- doc.terms) {
        val postingsList = indexer.index.getPostings(term).get
        val score = β * postingsList.getEntry(doc.id).tf() * postingsList.idf / Index.docLengths.get(doc.id)
        termScore(term) = termScore.getOrElse(term, 0.0) + score / relevantDocs.size
      }
    }
  }
}