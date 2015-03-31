package ir.models

import ir.index.Indexer
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap
import ir.index.Index
import ir.index.StructureType
import ir.index.Bigram

class Query(query: String, structureType: StructureType) {
  private val termScore: HashMap[String, Double] = new HashMap[String, Double]()
  private val α = 1.0
  private val β = 0.75

  var terms = structureType match {
    case Bigram => generateBigrams(query.split(' ').toList)
    case _      => query.split(' ').toList
  }

  terms.foreach(termScore(_) = 1.0 / terms.size)
 
  def score(term: String) = termScore(term)

  def relevanceFeedback(result: PostingsList, docIsRelevant: Array[Boolean], index: Index) {
    val relevantDocs = result
      .zipWithIndex
      .take(docIsRelevant.length)
      .filter((r) => docIsRelevant(r._2))
      .map(x => new Document(x._1.docId))

    // Multiply alpha
    for ((k, v) <- termScore) {
      termScore(k) = v * α
    }

    // Add second vector
    for (doc <- relevantDocs) {
      for (term <- doc.terms) {
        val postingsList = index.getPostings(term, true)
        var entry = postingsList.getEntry(doc.id)
        if (entry.isDefined) {
          val score = (β / relevantDocs.size) * ((entry.get.tf * postingsList.idf) / Index.docLengths(doc.id))
          termScore(term) = termScore.getOrElse(term, 0.0) + score
        }
      }
    }

    for (term <- termScore.keys) {
      if (!terms.contains(term)) {
        terms = term :: terms
      }
    }

    for ((k, v) <- termScore) {
      termScore(k) = termScore(k) / termScore.size
    }
  }

  private def generateBigrams(words: List[String]) = words.sliding(2).map(_.mkString(" ")).toList

}