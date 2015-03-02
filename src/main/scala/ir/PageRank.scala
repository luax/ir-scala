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

  val Runs = 10

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

  def powerIterate(): Array[Double] = {
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

    xx
  }

  // 1
  def monteCarloRandomStart(): Array[Double] = {
    val start = System.nanoTime
    val r = scala.util.Random

    val x = new Array[Double](N)

    for (_ <- 0 until Runs) {
      for (n <- 0 until N) {
        x(walk(r.nextInt(N))) += 1
      }
    }

    x.map(_ / (N * Runs))
  }

  // 2
  def monteCarloCyclicStart(): Array[Double] = {
    val start = System.nanoTime

    val x = new Array[Double](N)

    for (_ <- 0 until Runs) {
      for (n <- 0 until N) {
        var page = n
        for (_ <- 0 until M) {
          x(walk(page)) += 1
        }
      }
    }

    x.map(_ / (N * Runs * M))
  }

  // 3
  def monteCarloCompletePath(): Array[Double] = {
    val start = System.nanoTime

    val x = new Array[Double](N)
    val freq = new Array[Int](N)

    for (_ <- 0 until Runs) {
      for (n <- 0 until N) {
        var page = n
        for (_ <- 0 until M) {
          var endPage = walk(page, freq)
          x(endPage) = freq(endPage)
        }
      }
    }

    // Note: N = n*m (N * M here) in Avrachenkov et al.
    x.map(_ * α / (N * M * Runs))
  }

  // 4
  def monteCarloCompletePathDangleStop(): Array[Double] = {
    val start = System.nanoTime

    val x = new Array[Double](N)
    val freq = new Array[Int](N)

    for (_ <- 0 until Runs) {
      for (n <- 0 until N) {
        var page = n
        for (_ <- 0 until M) {
          var endPage = walk(page, freq, true)
          x(endPage) = freq(endPage)
        }
      }
    }

    val totalVisits = freq.foldLeft(0) { _ + _ }

    x.map(_ / totalVisits)
  }

  // 5
  def monteCarloCompletePathRandomStartDangleStop(): Array[Double] = {
    val start = System.nanoTime
    val r = scala.util.Random

    val x = new Array[Double](N)
    val freq = new Array[Int](N)

    for (_ <- 0 until Runs) {
      for (n <- 0 until N) {
        var page = walk(r.nextInt(N), freq, true)
        x(page) = freq(page)
      }
    }

    val totalVisits = freq.foldLeft(0) { _ + _ }
    x.map(_ / totalVisits)
  }

  def walk(page: Int, freq: Array[Int] = Array(), dangling: Boolean = false): Int = {
    // TODO
    val r = scala.util.Random
    var p = page
    while (true) {
      val prob = r.nextDouble
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
      if (!freq.isEmpty) freq(p) += 1 // TODO
    }
    0
  }

  def main(args: Array[String]) {
    compareAlgorithms()
  }

  def compareAlgorithms() {
    var correct = powerIterate
    val algorithms = Array(
      monteCarloRandomStart,
      monteCarloCyclicStart,
      monteCarloCompletePath,
      monteCarloCompletePathDangleStop,
      monteCarloCompletePathRandomStartDangleStop)

    val top50 = correct
      .sortBy(-_)
      .take(50)
    val bottom50 = correct
      .sorted
      .take(50)

    val compare = (x: Array[Double], y: Array[Double]) => (x, y).zipped.map((x, y) => math.pow(x - y, 2)).sum

    for ((algorithm, index) <- algorithms.zipWithIndex) {
      println(index + " top: " + compare(top50, algorithm.sortBy(-_).take(50)));
      println(index + " bottom: " + compare(bottom50, algorithm.sortBy(+_).take(50)));
    }
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

