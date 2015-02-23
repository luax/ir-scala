package ir

import collection.mutable.Stack
import org.scalatest._

class PhraseAndIntersectionTest() extends FunSuite with BeforeAndAfterAll {
  val data: String = "./data/wiki"
  val indexer: Indexer = new Indexer(Index.HASHED_INDEX)

  override def beforeAll() {
    indexer.processFiles(data)
  }

  def search(query: String, t: Int) = {
    indexer
      .index
      .search(new Query(SimpleTokenizer.normalize(query)), t, Index.TF_IDF, 0)
  }

  def intersectionQuery(query: String): Int = {
    search(query, Index.INTERSECTION_QUERY).size
  }

  def phraseSearch(query: String): Int = {
    search(query, Index.PHRASE_QUERY).size
  }

  test("intersection query") {
    assert(intersectionQuery("") == 0)
    assert(intersectionQuery("hnggggggggngggg") == 0)
  }

  test("intersection query \"zombie\"") {
    assert(intersectionQuery("zombie") == 36)
  }

  test("intersection query \"attack\"") {
    assert(intersectionQuery("attack") == 228)
  }

  test("intersection query \"zombie attack\"") {
    assert(intersectionQuery("zombie attack") == 15)
  }

  test("intersection query \"money transfer\"") {
    assert(intersectionQuery("money transfer") == 106)
  }

  test("intersection query \"sleeping on campus\"") {
    assert(intersectionQuery("sleeping on campus") == 82)
  }

  test("phrase query") {
    assert(phraseSearch("") == 0)
    assert(phraseSearch("hnggggggggngggg") == 0)
  }

  test("phrase query \"d d d\"") {
    assert(phraseSearch("d d d") == 1)
  }

  test("phrase query \"zombie\"") {
    assert(phraseSearch("zombie") == 36)
  }

  test("phrase query \"attack\"") {
    assert(phraseSearch("attack") == 228)
  }

  test("phrase query \"zombie attack\"") {
    assert(phraseSearch("zombie attack") == 14)
  }

  test("phrase query \"money transfer\"") {
    assert(phraseSearch("money transfer") == 2)
  }

  test("phrase query \"sleeping on campus\"") {
    assert(phraseSearch("sleeping on campus") == 19)
  }

  test("phrase query \"to be or not to be\"") {
    assert(phraseSearch("to be or not to be") == 1)
  }

}

