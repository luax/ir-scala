package ir

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object PageRank {
  val Epsilon = 0.001
  val α = 0.15

  val A = HashMap[Int, HashSet[Int]]()
  val documentMap = HashMap[Int, Int]()

  def readFile(file: String) {
    val docs = HashSet[Int]()
    val internalMapping = HashMap[Int, Int]()
    var docIndex = 0
    for (line <- Source.fromFile(file).getLines()) {
      var docId = line.substring(0, line.indexOf(';')).toInt

      if (!internalMapping.contains(docId)) {
        internalMapping(docId) = docIndex
        documentMap(docIndex) = docId
        docIndex += 1
      }

      var links = line.substring(line.indexOf(';') + 1, line.length).split(',').filter(!_.isEmpty).map(_.toInt)

      if (!links.isEmpty) {
        A(internalMapping(docId)) = HashSet()
      }

      for (docLink <- links) {
        if (!internalMapping.contains(docLink)) {
          internalMapping(docLink) = docIndex
          documentMap(docIndex) = docLink
          docIndex += 1
        }
        A(internalMapping(docId)) += internalMapping(docLink)
      }
    }
  }

  def printMatrix() {
    val N = documentMap.size

    for (row <- 0 until N) {
      for (col <- 0 until N) {
        if (A.contains(row) && A(row).contains(col)) {
          print(((1.0 - α) / A(row).size) + α / N + " \t ")
        } else if (A.contains(row) && !A(row).isEmpty) {
          print(α / N + " \t ")
        } else {
          if (col != row) {
            print((1 - α) / (N - 1) + α / N + " \t ")
          } else {
            print(α / N + " \t ")
          }
        }
      }
      println("")
    }
  }

  def transition(x: Array[Double]): Array[Double] = {
    val N = documentMap.size
    val xx = new Array[Double](N)
    val rows = (0 until N).par
    rows.foreach(row => {
      val jump = x(row) * (α / N)
      val sink = x(row) * ((1.0 - α) / (N - 1.0) + α / N)
      if (A.contains(row)) {
        val link = x(row) * ((1.0 - α) / A(row).size + α / N)
        for (col <- 0 until N) {
          if (A(row).contains(col)) {
            xx(col) += link
          } else {
            xx(col) += jump
          }
        }
      } else {
        for (col <- 0 until N) {
          if (col != row) {
            xx(col) += sink
          } else {
            xx(col) += jump
          }
        }
      }
    })
    xx
  }

  def powerIterate() {
    val start = System.nanoTime

    val N = documentMap.size
    var x = new Array[Double](N)
    var xx = new Array[Double](N)
    xx(0) = 1.0

    var i = 0
    val diff = () => (x, xx).zipped.map((x, y) => math.abs(x - y)).sum

    while (diff() > Epsilon) {
      x = xx
      xx = transition(x);
      i += 1
      println("Diff: " + diff())
    }

    println("Result:")
    xx.zipWithIndex.sortBy(-_._1).take(50).foreach((x) => println(x._1 + " " + documentMap(x._2)))

    println("Time elapsed: " + (System.nanoTime - start) * 1e-9 + " s")
  }

  def main(args: Array[String]) {
    println("Reading links")
    readFile("./links.txt")
    powerIterate()
  }
}

