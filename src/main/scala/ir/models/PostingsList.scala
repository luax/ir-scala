package ir.models

import scala.collection.mutable.MutableList
import ir.Index
import util.Properties

class PostingsList(val token: String = "") extends MutableList[PostingsEntry] {
  def getEntry(n: Int): PostingsEntry = {
    get(n).getOrElse(null)
  }

  def entries() = size

  def df() = size

  def idf() = math.log(Index.docIDs.size / df)

  override def toString() = {
    foldLeft("")((x, y) => { x ++ " " + y + Properties.lineSeparator })
  }
}