package ir

import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.util.control.Breaks._
import java.io._
import scala.io.Source

object PageRank {
  val pageRankPath = "./pagerank.ser"
  val pageRankArrayPath = "./pagerank_array.ser"

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

    println("Done")
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
    val x = new Array[Double](n)

    (0 until N).par.foreach { _ =>
      x(walk(rand.nextInt(n))) += 1
    }

    x.map(_ / N)
  }

  // 2
  def monteCarloCyclicStart(N: Int, M: Int): Array[Double] = {
    val x = new Array[Double](n)

    (0 until N).par.foreach { page =>
      for (_ <- 0 until M) {
        x(walk(page)) += 1
      }
    }

    x.map(_ / (N * M))
  }

  // 3
  def monteCarloCompletePath(N: Int, M: Int): Array[Double] = {
    val x = new Array[Double](n)

    (0 until N).par.foreach { page =>
      for (_ <- 0 until M) {
        walk(page, x)
      }
    }

    x.map(_ * α / (N * M))
  }

  // 4
  def monteCarloCompletePathDangleStop(N: Int, M: Int): Array[Double] = {
    val x = new Array[Double](n)

    (0 until N).par.foreach { page =>
      for (_ <- 0 until M) {
        walk(page, x, true)
      }
    }

    val totalVisits = x.foldLeft(0.0) { _ + _ }
    x.map(_ / totalVisits)
  }

  // 5
  def monteCarloCompletePathRandomStartDangleStop(N: Int): Array[Double] = {
    val x = new Array[Double](n)

    (0 until N).par.foreach { _ =>
      walk(rand.nextInt(n), x, true)
    }

    val totalVisits = x.foldLeft(0.0) { _ + _ }
    x.map(_ / totalVisits)
  }

  def walk(page: Int, freq: Array[Double] = Array(), dangling: Boolean = false): Int = {
    var p = page
    while (true) {
      if (!freq.isEmpty) { freq(p) += 1 } // TODO
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
    val pageRanking = readPagerankArrayFromFile
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

    val c3 = (y: Array[Double]) => {
      var test = 3
      val approx50 = y.zipWithIndex.sortBy(-_._1).take(docs + test).map(_._2)
      var size = top50.map(_._2).zipWithIndex.map((x) => {
        var j = 0
        var found = false
        if (x._2 - 2 >= 0) j = x._2 - 2
        while (j < top50.size + test && j < (x._2 + 2) && !found) {
          if (approx50(j) == x._1) found = true
          j += 1
        }
        found
      }).filter(_ == true).size
      println(size)
      size == 50
    }

    val c2 = (y: Array[Double]) => {
      !top50.map((x: (Double, Int)) => {
        math.abs(x._1 - y(x._2)) > 0.3e-3
      }).contains(true)
    }

    val c1 = (y: Array[Double]) => {
      val approx50 = y.zipWithIndex.sortBy(-_._1).take(docs).map(_._2).toSet
      val size = top50.map(_._2).toSet.intersect(approx50).size
      size >= docs - 2
    }

    val algorithms = List(
      (N: Int, _: Int) => monteCarloRandomStart(N),
      (N: Int, M: Int) => monteCarloCyclicStart(N, M),
      (N: Int, M: Int) => monteCarloCompletePath(N, M),
      (N: Int, M: Int) => monteCarloCompletePathDangleStop(N, M),
      (N: Int, _: Int) => monteCarloCompletePathRandomStartDangleStop(N))

    for ((f, index) <- algorithms.zipWithIndex) {
      var i = 1
      var N = n * i
      var rank = Array[Double]()
      var converged = false

      val start = System.nanoTime
      while (!converged) {

        if (N > n && List(1, 2, 3).contains(index)) {
          rank = f(n, i)
        } else {
          rank = f(N, 1)
        }
        converged = c1(rank) && c2(rank) // && c3(rank)

        //println("N = n * " + i)
        N = n * i
        i += 1

      }

      println("----")
      println("Algorithm: " + (index + 1) + ", Needed N = n * " + i + " = " + N)
      printTimeElapsed(start)
      printResult(rank)

      println("----")
    }
  }

  def printResult(x: Array[Double]) {
    println("Result:")
    x.zipWithIndex
      .sortBy(-_._1)
      .take(50)
      .foreach((x) => println(documentMap(x._2) + "," + x._1))
  }

  def printTimeElapsed(start: Double) {
    println("Time elapsed: " + (System.nanoTime - start) * 1e-9 + " s")
  }

  def writePagerankArrayToFile() {
    val fos = new FileOutputStream(pageRankArrayPath)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(powerIterate)
    oos.close
  }

  def readPagerankArrayFromFile(): Array[Double] = {
    val fis = new FileInputStream(pageRankArrayPath)
    val ois = new ObjectInputStream(fis)
    val rank = ois.readObject.asInstanceOf[Array[Double]]
    ois.close
    rank
  }

  def writePagerankToFile() {
    // Read id to title mapping
    var titles = HashMap[Int, String]()
    for (line <- Source.fromFile("titles.txt").getLines()) {
      var parts = line.split(";")
      var id = parts(0).toInt
      var title = parts(1)
      titles(id) = title
    }

    // Map id -> title = score
    var mapping = HashMap[String, Double]()
    powerIterate.zipWithIndex.foreach((x) => mapping(titles(documentMap(x._2))) = x._1)

    val fos = new FileOutputStream(pageRankPath)
    val oos = new ObjectOutputStream(fos)
    oos.writeObject(mapping)
    oos.close
  }
}

