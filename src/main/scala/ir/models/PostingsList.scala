package ir.models

import scala.collection.mutable.MutableList

class PostingsList(val token: String) extends MutableList[PostingsEntry] {
  def getEntry(n: Int): PostingsEntry = {
    get(n).getOrElse(null)
  }
}