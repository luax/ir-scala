package ir

import ir.index._

object Ir extends App {
  var s = new SearchGUI(RankedQuery, TfIdf, Subphrase)
  s.createGUI()
  s.processArgs(args)
}