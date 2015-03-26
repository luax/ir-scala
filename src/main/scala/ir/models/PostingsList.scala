package ir.models

import scala.collection.mutable.MutableList
import util.Properties
import ir.index._

class PostingsList(val token: String = "") extends MutableList[PostingsEntry] {
  def getEntry(docId: Int) = find(_.docId == docId)
  def entries() = size
  def df() = size
  def idf() = math.log(Index.docFilepaths.size / df)
  override def toString() = foldLeft("")((x, y) => { x ++ " " + y + Properties.lineSeparator })
}