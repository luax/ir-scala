package ir.models

import scala.collection.mutable.ListBuffer

class PostingsEntry(val docId: Int) {
  val offsets: ListBuffer[Int] = new ListBuffer[Int]
}