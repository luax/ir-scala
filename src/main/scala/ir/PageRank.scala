package ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.util.control.Breaks._
import java.io.FileOutputStream
import java.io.ObjectOutputStream
import java.io._
import scala.io.Source

object PageRank {
  val Epsilon = 0.001
  val α = 0.15

  val A = HashMap[Int, HashSet[Int]]()
  val documentMap = HashMap[Int, Int]()

  val rand = scala.util.Random

  // Note: N in Avrachenkov et al is not == num documents
  var n = readFile("./links.txt")

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
    for (row <- 0 until n) {
      for (col <- 0 until n) {
        if (A.contains(row) && A(row).contains(col)) {
          print(((1.0 - α) / A(row).size) + α / n + " \t ")
        } else if (A.contains(row) && !A(row).isEmpty) {
          print(α / n + " \t ")
        } else {
          if (col != row) {
            print((1 - α) / (n - 1) + α / n + " \t ")
          } else {
            print(α / n + " \t ")
          }
        }
      }
      println("")
    }
  }

  def transition(x: Array[Double]): Array[Double] = {
    val xx = new Array[Double](n)
    val rows = (0 until n).par
    rows.foreach(row => {
      val jump = x(row) * (α / n)
      val sink = x(row) * ((1.0 - α) / (n - 1.0) + α / n)
      if (A.contains(row)) {
        val link = x(row) * ((1.0 - α) / A(row).size + α / n)
        for (col <- 0 until n) {
          if (A(row).contains(col)) {
            xx(col) += link
          } else {
            xx(col) += jump
          }
        }
      } else {
        for (col <- 0 until n) {
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

    var x = new Array[Double](n)
    var xx = new Array[Double](n)
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
  def monteCarloRandomStart(N: Int): Array[Double] = {
    val start = System.nanoTime
    val r = scala.util.Random

    val x = new Array[Double](n)

    for (_ <- (0 until N).par) {
      x(walk(r.nextInt(n))) += 1
    }

    x.map(_ / N)
  }

  // 2
  def monteCarloCyclicStart(N: Int, M: Int): Array[Double] = {
    val start = System.nanoTime

    val x = new Array[Double](n)

    for (page <- 0 until N) {
      for (_ <- 0 until M) {
        x(walk(page)) += 1
      }
    }

    x.map(_ / (N * M))
  }

  // 3
  def monteCarloCompletePath(N: Int, M: Int): Array[Double] = {
    val start = System.nanoTime

    val x = new Array[Double](n)
    val freq = new Array[Int](n)

    for (page <- 0 until N) {
      for (_ <- 0 until M) {
        var endPage = walk(page, freq)
        x(endPage) = freq(endPage)
      }
    }

    x.map(_ * α / (N * M))
  }

  // 4
  def monteCarloCompletePathDangleStop(N: Int, M: Int): Array[Double] = {
    val start = System.nanoTime

    val x = new Array[Double](n)
    val freq = new Array[Int](n)

    for (page <- 0 until N) {
      for (_ <- 0 until M) {
        var endPage = walk(page, freq, true)
        x(endPage) = freq(endPage)
      }
    }

    val totalVisits = freq.foldLeft(0) { _ + _ }

    x.map(_ / totalVisits)
  }

  // 5
  def monteCarloCompletePathRandomStartDangleStop(N: Int): Array[Double] = {
    val start = System.nanoTime
    val r = scala.util.Random

    val x = new Array[Double](n)
    val freq = new Array[Int](n)

    for (_ <- 0 until N) {
      var page = walk(r.nextInt(n), freq, true)
      x(page) = freq(page)
    }

    val totalVisits = freq.foldLeft(0) { _ + _ }
    x.map(_ / totalVisits)
  }

  def walk(page: Int, freq: Array[Int] = Array(), dangling: Boolean = false): Int = {
    // TODO
    var p = page
    while (true) {
      if (!freq.isEmpty) freq(p) += 1 // TODO
      val prob = rand.nextDouble
      if (prob >= α) {
        if (A.contains(p)) {
          p = A(p).toList(rand.nextInt(A(p).size)) // TODO
        } else if (dangling) {
          return p
        } else {
          p = rand.nextInt(n)
        }
      } else {
        return p
      }
    }
    0
  }

  def main(args: Array[String]) {
    compareAlgorithms
  }

  def compareAlgorithms() {
    val docs = 50
    val M = 1
    val pageRanking = readPagerankFromFile
    val top50 = pageRanking
      .zipWithIndex
      .sortBy(-_._1)
      .take(docs)

    val bottom50 = pageRanking
      .zipWithIndex
      .sortBy(+_._1)
      .take(docs)

    val goodness = (y: Array[Double]) => {
      top50.map((x: (Double, Int)) => {
        math.pow(x._1 - y(x._2), 2)
      }).sum
    }

    val compare = (y: Array[Double]) => {
      (pageRanking, y).zipped.map((x: Double, y: Double) => {
        math.pow(x - y, 2)
      }).sum
    }

    val algorithms = List(
      (N: Int, M: Int) => monteCarloRandomStart(N),
      (N: Int, M: Int) => monteCarloCyclicStart(N, M),
      (N: Int, M: Int) => monteCarloCompletePath(N, M),
      (N: Int, M: Int) => monteCarloCompletePathDangleStop(N, M),
      (N: Int, M: Int) => monteCarloCompletePathRandomStartDangleStop(N))

    for ((f, index) <- algorithms.zipwithindex) {
      var n = n * 100
      var rank = f(n, m)
      var diff = compare(rank)
      while (diff > 1e-7) {
        n += 10000
        rank = f(n, m)
        diff = compare(rank)
        println(n + " " + diff)
      }
      println((index + 1) + " needed: " + n + " " + diff);
      printresult(rank)
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

  def writePagerankToFile() {
    val fos = new FileOutputStream("./pangerank.ser")
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(powerIterate)
    oos.close
  }

  def readPagerankFromFile(): Array[Double] = {
    val fis = new FileInputStream("./pangerank.ser")
    val ois = new ObjectInputStream(fis)
    val rank = ois.readObject.asInstanceOf[Array[Double]]
    ois.close
    rank
  }
}

