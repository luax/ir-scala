package ir

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.util.control.Breaks._

object PageRank {
  val Epsilon = 0.001
  val α = 0.15

  val A = HashMap[Int, HashSet[Int]]()
  val documentMap = HashMap[Int, Int]()

  val N = readFile("./links.txt")
  val M = 1000

  def readFile(file: String): Int = {
    println("Reading links")
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
    documentMap.size
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

  def transition(x: Array[Double]): Array[Double] = {
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

    printResult(xx)
    printTimeElapsed(start)
  }

  // 1
  // Simulate N runs of the random walk initiated at a randomly chosen page.
  def monteCarloRandomStart() {
    val start = System.nanoTime
    val r = scala.util.Random

    val x = new Array[Double](N)

    for (n <- 0 until N) {
      var page = walk(r.nextInt(N))
      x(page) += 1
    }

    printResult(x.map(_ / N))
    printTimeElapsed(start)
  }

  // 2
  // Simulate N = mn runs of the random walk initiated at 
  // each page exactly m times.
  def monteCarloCyclicStart() {
    val start = System.nanoTime

    val x = new Array[Double](N)

    for (n <- 0 until N) {
      var page = n
      for (m <- 0 until M) {
        x(walk(page)) += 1
      }
    }

    printResult(x.map(_ / N))
    printTimeElapsed(start)
  }

  // 3
  // Simulate the random walk initiated exactly m times from each page.
  // For any page, evaluate the total number of visits to page j multiplied by 
  // α / nm
  def monteCarloCompletePath() {
    val start = System.nanoTime

    val x = new Array[Double](N)
    val freq = new Array[Int](N)

    for (n <- 0 until N) {
      var page = n
      for (m <- 0 until M) {
        x(walk(page, freq)) += freq(page)
      }
    }

    printResult(x.map(_ * α / N * M))
    printTimeElapsed(start)
  }

  // 4
  // Simulate the random walk initiated exactly m times from each page.
  // For any page, evaluate the total number of visits to page j divided by 
  // the total number of visited pages
  def monteCarloCompletePathDangleStop() {
    val start = System.nanoTime

    val x = new Array[Double](N)
    val freq = new Array[Int](N)

    for (n <- 0 until N) {
      var page = n
      for (m <- 0 until M) {
        x(walk(page, freq, true)) += freq(page)
      }
    }

    val totalVisits = freq.foldLeft(0) { _ + _ }
    printResult(x.map(_ / totalVisits))
    printTimeElapsed(start)
  }

  // 5
  def monteCarloCompletePathRandomStartDangleStop() {
    val start = System.nanoTime
    val r = scala.util.Random

    val x = new Array[Double](N)
    val freq = new Array[Int](N)

    for (n <- 0 until N) {
      var page = r.nextInt(N)
      x(walk(page, freq, true)) += freq(page)
    }

    val totalVisits = freq.foldLeft(0) { _ + _ }
    printResult(x.map(_ / totalVisits))
    printTimeElapsed(start)
  }

  def walk(page: Int, freq: Array[Int] = Array(), dangling: Boolean = false): Int = {
    // TODO
    val r = scala.util.Random
    var p = page
    while (true) {
      val prob = r.nextDouble
      if (!freq.isEmpty) freq(p) += 1 // TODO
      if (prob >= 1 - α) {
        if (A.contains(p)) {
          p = A(p).toList(r.nextInt(A(p).size)) // TODO
        } else if (dangling) {
          return p
        } else {
          p = r.nextInt(N)
        }
      } else {
        return p
      }
    }
    0
  }

  def main(args: Array[String]) {
    //powerIterate
    //monteCarloRandomStart
    //monteCarloCyclicStart
    //monteCarloCompletePath
    monteCarloCompletePathRandomStartDangleStop
  }

  def printResult(x: Array[Double]) {
    println("Result:")
    x.zipWithIndex
      .sortBy(-_._1)
      .take(50)
      .foreach((x) => println(documentMap(x._2) + " " + x._1))
  }

  def printTimeElapsed(start: Double) {
    println("Time elapsed: " + (System.nanoTime - start) * 1e-9 + " s")
  }
}

