package ir

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet

object PageRank {
  val Epsilon = 0.001
  val α = 0.15

  val A = HashMap[Int, HashSet[Int]]()
  var N = 0

  def readFile(file: String) {
    val Docs = HashSet[Int]()
    for (line <- Source.fromFile(file).getLines()) {
      var docId = line.substring(0, line.indexOf(';')).toInt
      var links = line.substring(line.indexOf(';') + 1, line.length).split(',').filter(!_.isEmpty).map(_.toInt)
      A(docId) = new HashSet() ++ links
      Docs ++= links
      Docs += docId
    }
    N = Docs.size
  }

  def printMatrix() {
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

  def transition(x: Array[Double]) {
    var xx = new Array[Double](N)
    for (row <- 1 until N + 1) {
      if (A.contains(row)) {
        for (col <- 1 until N + 1) {
          if (A(row).contains(col)) {
            xx(col - 1) += x(row - 1) * ((1.0 - α) / A(row).size + α / N)
          } else {
            xx(col - 1) += x(row - 1) * (α / N)
          }
        }
      } else {
        for (col <- 1 until N + 1) {
          if (col != row) {
            xx(col - 1) += x(row - 1) * ((1.0 - α) / (N - 1.0) + α / N)
          } else {
            xx(col - 1) += x(row - 1) * (α / N)
          }
        }
      }
    }
    xx
  }

  def main(args: Array[String]) {
    readFile("./linksDavis.txt")

    var x = new Array[Double](N)
    var xx = new Array[Double](N)
    xx(0) = 1.0

    println("N: " + N)

    var i = 0
    while ((x, xx).zipped.map((x, y) => math.abs(x - y)).sum > Epsilon) {
      x = xx.clone
      xx = transition(x);
      i += 1
      println("+++")
      xx.zipWithIndex.sortBy(-_._1).take(10).foreach((x) => println(x._1 + " " + (x._2 + 1)))
      println("i: " + i + " diff: " + (x, xx).zipped.map((x, y) => math.abs(x - y)).sum + " " + x.sum + " " + xx.sum)
      println("+++")
    }

    xx.zipWithIndex.sortBy(-_._1).take(50).foreach((x) => println(x._1 + " " + (x._2 + 1)))
  }
}

