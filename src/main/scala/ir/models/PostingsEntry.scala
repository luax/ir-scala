package ir.models

import scala.collection.mutable.ListBuffer
import ir.index.Index;

class PostingsEntry(val docId: Int, var score: Double = 0) {
  val offsets: ListBuffer[Int] = new ListBuffer[Int]
  def tf() = offsets.size
  override def toString() = Index.docFilepaths.get(docId) + " " + score
}
