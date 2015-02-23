package ir

object Ir extends App {
  var s = new SearchGUI()
  s.createGUI()
  s.decodeArgs(args)
  s.index()
}