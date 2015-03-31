package ir

import ir.index._

object Ir extends App {
  var s = new SearchGUI(RankedQuery, TfIdf, Unigram)
  s.createGUI()
  s.processArgs(args)
}