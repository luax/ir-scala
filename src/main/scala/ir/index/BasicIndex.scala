package ir.index

import ir.models.PostingsList
import ir.Query
import ir.Index
import java.util.LinkedList

abstract class BasicIndex extends Index {
  def getPostings(token: String): Option[PostingsList]
  def getPostings(tokens: LinkedList[String]): List[PostingsList]
  def insert(token: String, docID: Int, offset: Int): Unit
  def search(query: Query, queryType: Int, rankingType: Int, structureType: Int): PostingsList
  def cleanup(): Unit = ???
  def getDictionary(): java.util.Iterator[String] = ???
}