package ir.index

import java.io.File
import javax.swing.JTextArea
import ir.SimpleTokenizer
import java.text.NumberFormat
import java.io._
import scala.io.Source

object Indexer {
  val hashedIndexPath = "./data/HashedIndex"

  def createIndex(dirs: List[String], structureType: StructureType): Index = {
    var index = structureType match {
      case Unigram   => new HashedIndex
      case Bigram    => new BiwordIndex
      case Subphrase => new SubphraseIndex
    }
    for (dir <- dirs) {
      indexDirectory(new File(dir), index)
    }
    index.initialize
    index
  }

  private def indexDirectory(dir: File, index: Index) {
    if (!dir.canRead) {
      println("Could not read " + dir)
    } else {
      val files = dir.listFiles.zipWithIndex
      println("Indexing " + files.size + " files")
      for ((file, docId) <- files) {
        val reader = Source.fromFile(file).bufferedReader
        val tokenizer = new SimpleTokenizer(reader)
        var position = 0
        while (tokenizer.hasMoreTokens()) {
          index.insert(tokenizer.nextToken, docId, position)
          position += 1
        }
        Index.docFilepaths(docId) = file.getPath
        Index.docLengths(docId) = position
        reader.close
      }
      println("Done")
    }
  }
}