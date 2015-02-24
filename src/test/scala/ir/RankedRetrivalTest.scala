package ir

import collection.mutable.Stack
import org.scalatest._

class RankedRetrivalTest() extends FunSuite with BeforeAndAfterAll {
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

  def rankedQuery(query: String): Int = {
    search(query, Index.RANKED_QUERY).size
  }

  test("zombie") {
    assert(rankedQuery("zombie") == 36)
  }

  test("attack") {
    assert(rankedQuery("attack") == 228)
  }
  
  test("zombie attack") {
    assert(rankedQuery("zombie attack") == 249)
  }
  
  test("money transfer") {
    assert(rankedQuery("money transfer") == 1602)
  }
  
  test("sleeping on campus") {
    assert(rankedQuery("sleeping on campus") == 9885)
  }  
}

