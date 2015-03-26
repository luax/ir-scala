package ir.models

import ir.index.Index;
import java.io._
import scala.io.Source
import ir.SimpleTokenizer
import scala.collection.mutable.ArrayBuffer

class Document(val id: Int) {
  val terms = {
    var tokenizer = new SimpleTokenizer(Source.fromFile(Index.docFilepaths(id)).bufferedReader())
    var tokens = new ArrayBuffer[String]()
    while (tokenizer.hasMoreTokens()) {
      tokens += tokenizer.nextToken()
    }
    tokens
  }
}