package patmat

object HuffTester extends App {

  val input = List('a', 'a', 'a', 'a', 'a', 'a', 'a', 'a', 'b', 'b', 'b',
    'c', 'd', 'e', 'f', 'g', 'h')
  val freqs = Huffman.times(input)
  val leafs = Huffman.makeOrderedLeafList(freqs)
  println("Leafs: " + leafs)
  val combined = Huffman.combine(leafs)
  println("Combined: " + combined)
  println("Combined until: " + Huffman.until(Huffman.singleton, Huffman.combine)(leafs))
  val codeTree = Huffman.createCodeTree(input)
  println("Create code tree: " + Huffman.createCodeTree(input))
  println("Decoded secret: " + Huffman.decodedSecret2)
  println("Encoded secret: " + Huffman.encodedSecret)
  println("Encoded secret: " + Huffman.encodedSecretQ)
  val ct1 = List(('a', List(1, 0)), ('c', List(1)), ('b', List(1)), ('d', List(0)))
  val ct2 = List(('a', List()), ('c', List(1)), ('b', List(1)), ('d', List(1)), ('e', List(1, 1)))
  val ct = Huffman.mergeCodeTables(ct1, ct2)
  println("Merged: " + ct)
  val codeTable = Huffman.convert(codeTree)
  println("CodeTable: " + codeTable)
}