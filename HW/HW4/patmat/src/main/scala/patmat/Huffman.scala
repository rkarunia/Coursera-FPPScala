package patmat

import common._

/**
 * Assignment 4: Huffman coding
 *
 */
object Huffman {

  /**
   * A huffman code is represented by a binary tree.
   *
   * Every `Leaf` node of the tree represents one character of the alphabet that the tree can encode.
   * The weight of a `Leaf` is the frequency of appearance of the character.
   *
   * The branches of the huffman tree, the `Fork` nodes, represent a set containing all the characters
   * present in the leaves below it. The weight of a `Fork` node is the sum of the weights of these
   * leaves.
   */
  abstract class CodeTree
  case class Fork(left: CodeTree, right: CodeTree, chars: List[Char], weight: Int) extends CodeTree
  case class Leaf(char: Char, weight: Int) extends CodeTree

  // Part 1: Basics

  def weight(tree: CodeTree): Int = tree match {
    case Leaf(ch, wt) => wt
    case Fork(left, right, chars, wt) => wt
  }

  def chars(tree: CodeTree): List[Char] = tree match {
    case Leaf(ch, wt) => List(ch)
    case Fork(left, right, chars, wt) => chars
  }

  def makeCodeTree(left: CodeTree, right: CodeTree) =
    Fork(left, right, chars(left) ::: chars(right), weight(left) + weight(right))

  // Part 2: Generating Huffman trees

  /**
   * In this assignment, we are working with lists of characters. This function allows
   * you to easily create a character list from a given string.
   */
  def string2Chars(str: String): List[Char] = str.toList

  /**
   * This function computes for each unique character in the list `chars` the number of
   * times it occurs. For example, the invocation
   *
   *   times(List('a', 'b', 'a'))
   *
   * should return the following (the order of the resulting list is not important):
   *
   *   List(('a', 2), ('b', 1))
   *
   * The type `List[(Char, Int)]` denotes a list of pairs, where each pair consists of a
   * character and an integer. Pairs can be constructed easily using parentheses:
   *
   *   val pair: (Char, Int) = ('c', 1)
   *
   * In order to access the two elements of a pair, you can use the accessors `_1` and `_2`:
   *
   *   val theChar = pair._1
   *   val theInt  = pair._2
   *
   * Another way to deconstruct a pair is using pattern matching:
   *
   *   pair match {
   *     case (theChar, theInt) =>
   *       println("character is: "+ theChar)
   *       println("integer is  : "+ theInt)
   *   }
   */

  //
  // Original implementation before watching the lecture
  //
  /*
   *
   *  
   def times(chars: List[Char]): List[(Char, Int)] = {
    def addCount(c: Char, pl: List[(Char, Int)]): List[(Char, Int)] = pl match {
      case List() => List((c, 1))
      case List((ch, ct)) => if (ch == c) List((ch, ct + 1)) else List((ch, ct), (c, 1))
      case (ch, ct) :: xs => if (c == ch) (ch, ct + 1) :: xs else List((ch, ct)) ::: addCount(c, xs)
    }
    chars match {
      case List() => throw new Error("Empty List")
      case List(x) => List((x, 1))
      case c :: cn => addCount(c, times(cn))
    }
  }
  * 
  *
  */

  //
  // Implementation after watching the lecture
  //
  def group[T](xs: List[T]): List[List[T]] = xs match {
    case Nil => Nil
    case y :: ys =>
      val (first, rest) = xs partition (z => z == y)
      first :: group(rest)
  }
  def times(chars: List[Char]): List[(Char, Int)] =
    group(chars) map (ys => (ys.head, ys.length))

  /**
   * Returns a list of `Leaf` nodes for a given frequency table `freqs`.
   *
   * The returned list should be ordered by ascending weights (i.e. the
   * head of the list should have the smallest weight), where the weight
   * of a leaf is the frequency of the character.
   */
  //
  // Original implementation before watching the lecture
  //
  /*
   *
   *  
   def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    def msort(xs: List[(Char, Int)]): List[(Char, Int)] = {
      val n = xs.length / 2
      if (n == 0) xs
      else {
        def merge(xs: List[(Char, Int)], ys: List[(Char, Int)]): List[(Char, Int)] = (xs, ys) match {
          case (Nil, ys) => ys
          case (xs, Nil) => xs
          case ((xch, xct) :: xs1, (ych, yct) :: ys1) =>
            if (xct < yct) (xch, xct) :: merge(xs1, ys)
            else (ych, yct) :: merge(xs, ys1)
        }
        val (fst, snd) = xs splitAt n
        merge(msort(fst), msort(snd))
      }
    }
    makeLeafList(msort(freqs))
  }
  def makeLeafList(xs: List[(Char, Int)]): List[Leaf] = xs match {
    case List() => throw new Error("Empty List")
    case List((ch, ct)) => List(Leaf(ch, ct))
    case (ch, ct) :: ys => List(Leaf(ch, ct)) ::: makeLeafList(ys)
  }
  *
  * 
  */

  //
  // Implementation after watching lecture
  //
  val tupleLessThan = (tp1: (Char, Int), tp2: (Char, Int)) => tp1._2 < tp2._2

  def msort[T](xs: List[T])(implicit lt: (T, T) => Boolean): List[T] = {
    val n = xs.length / 2
    if (n == 0) xs
    else {
      def merge(xs: List[T], ys: List[T]): List[T] = (xs, ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xsl, y :: ysl) =>
          if (lt(x, y)) x :: merge(xsl, ys)
          else y :: merge(xs, ysl)
      }
      val (first, second) = xs splitAt n
      merge(msort(first), msort(second))
    }
  }

  def makeOrderedLeafList(freqs: List[(Char, Int)]): List[Leaf] = {
    msort(freqs)(tupleLessThan) map (p => Leaf(p._1, p._2))
  }

  /**
   * Checks whether the list `trees` contains only one single code tree.
   */
  def singleton(trees: List[CodeTree]): Boolean = trees match {
    case List() => false
    case List(x) => true
    case y :: ys => false
  }

  /**
   * The parameter `trees` of this function is a list of code trees ordered
   * by ascending weights.
   *
   * This function takes the first two elements of the list `trees` and combines
   * them into a single `Fork` node. This node is then added back into the
   * remaining elements of `trees` at a position such that the ordering by weights
   * is preserved.
   *
   * If `trees` is a list of less than two elements, that list should be returned
   * unchanged.
   */
  def combine(trees: List[CodeTree]): List[CodeTree] = trees match {
    case List() => trees
    case List(x) => trees
    case x :: xs =>
      val first2 = trees take 2
      val combined = makeCodeTree(first2.head, first2.tail.head)
      //val rest = trees drop 2
      //val (lessThan, moreThan) = rest span (ct => weight(ct) < weight(combined))
      val (lessThan, moreThan) = trees drop 2 span (ct => weight(ct) < weight(combined))
      // colon operator is right associative, notice the double colon and triple colon below:
      lessThan ::: combined :: moreThan
  }
  /**
   * This function will be called in the following way:
   *
   *   until(singleton, combine)(trees)
   *
   * where `trees` is of type `List[CodeTree]`, `singleton` and `combine` refer to
   * the two functions defined above.
   *
   * In such an invocation, `until` should call the two functions until the list of
   * code trees contains only one single tree, and then return that singleton list.
   *
   * Hint: before writing the implementation,
   *  - start by defining the parameter types such that the above example invocation
   *    is valid. The parameter types of `until` should match the argument types of
   *    the example invocation. Also define the return type of the `until` function.
   *  - try to find sensible parameter names for `xxx`, `yyy` and `zzz`.
   */
  def until(sf: List[CodeTree] => Boolean, ct: List[CodeTree] => List[CodeTree])(trees: List[CodeTree]): List[CodeTree] = trees match {
    case List() => throw new Error("Empty Tree")
    case x :: xs => if (sf(trees)) trees else {
      val combined = combine(trees)
      until(sf, combine)(combined)
    }
  }

  /**
   * This function creates a code tree which is optimal to encode the text `chars`.
   *
   * The parameter `chars` is an arbitrary text. This function extracts the character
   * frequencies from that text and creates a code tree based on them.
   */
  def createCodeTree(chars: List[Char]): CodeTree =
    until(singleton, combine)(makeOrderedLeafList(times(chars))).head

  // Part 3: Decoding

  type Bit = Int

  /**
   * This function decodes the bit sequence `bits` using the code tree `tree` and returns
   * the resulting list of characters.
   */
  def decode(tree: CodeTree, bits: List[Bit]): List[Char] = {
    def decodeRecurse(oTree: CodeTree, wTree: CodeTree, bits: List[Bit], acc: List[Char]): List[Char] = {
      if (bits.length > 0) {
        wTree match {
          case Leaf(ch, wt) => decodeRecurse(oTree, oTree, bits, acc ::: List(ch))
          case Fork(left, right, chars, wt) =>
            if (bits.head == 0) {
              decodeRecurse(oTree, left, bits drop 1, acc)
            } else {
              decodeRecurse(oTree, right, bits drop 1, acc)
            }
        }
      } else {
        wTree match {
          case Leaf(ch, wt) => acc ::: List(ch)
          case Fork(left, right, chars, wt) => acc
        }
      }
    }
    decodeRecurse(tree, tree, bits, List())
  }
  /**
   * A Huffman coding tree for the French language.
   * Generated from the data given at
   *   http://fr.wikipedia.org/wiki/Fr%C3%A9quence_d%27apparition_des_lettres_en_fran%C3%A7ais
   */
  val frenchCode: CodeTree = Fork(Fork(Fork(Leaf('s', 121895), Fork(Leaf('d', 56269), Fork(Fork(Fork(Leaf('x', 5928), Leaf('j', 8351), List('x', 'j'), 14279), Leaf('f', 16351), List('x', 'j', 'f'), 30630), Fork(Fork(Fork(Fork(Leaf('z', 2093), Fork(Leaf('k', 745), Leaf('w', 1747), List('k', 'w'), 2492), List('z', 'k', 'w'), 4585), Leaf('y', 4725), List('z', 'k', 'w', 'y'), 9310), Leaf('h', 11298), List('z', 'k', 'w', 'y', 'h'), 20608), Leaf('q', 20889), List('z', 'k', 'w', 'y', 'h', 'q'), 41497), List('x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 72127), List('d', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 128396), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q'), 250291), Fork(Fork(Leaf('o', 82762), Leaf('l', 83668), List('o', 'l'), 166430), Fork(Fork(Leaf('m', 45521), Leaf('p', 46335), List('m', 'p'), 91856), Leaf('u', 96785), List('m', 'p', 'u'), 188641), List('o', 'l', 'm', 'p', 'u'), 355071), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u'), 605362), Fork(Fork(Fork(Leaf('r', 100500), Fork(Leaf('c', 50003), Fork(Leaf('v', 24975), Fork(Leaf('g', 13288), Leaf('b', 13822), List('g', 'b'), 27110), List('v', 'g', 'b'), 52085), List('c', 'v', 'g', 'b'), 102088), List('r', 'c', 'v', 'g', 'b'), 202588), Fork(Leaf('n', 108812), Leaf('t', 111103), List('n', 't'), 219915), List('r', 'c', 'v', 'g', 'b', 'n', 't'), 422503), Fork(Leaf('e', 225947), Fork(Leaf('i', 115465), Leaf('a', 117110), List('i', 'a'), 232575), List('e', 'i', 'a'), 458522), List('r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 881025), List('s', 'd', 'x', 'j', 'f', 'z', 'k', 'w', 'y', 'h', 'q', 'o', 'l', 'm', 'p', 'u', 'r', 'c', 'v', 'g', 'b', 'n', 't', 'e', 'i', 'a'), 1486387)

  /**
   * What does the secret message say? Can you decode it?
   * For the decoding use the `frenchCode' Huffman tree defined above.
   */
  val secret: List[Bit] = List(0, 0, 1, 1, 1, 0, 1, 0, 1, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, 1, 0, 1, 0, 1, 1, 0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 0, 1, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 1, 0, 1)

  /**
   * Write a function that returns the decoded secret
   */
  def decodedSecret: List[Char] = decode(frenchCode, secret)
  def encodedSecret: List[Bit] = encode(frenchCode)(decodedSecret)
  def encodedSecretQ: List[Bit] = quickEncode(frenchCode)(decodedSecret)
  val input = List('a', 'b', 'a', 'a', 'k', 'a', 'b', 'b', 'b')
  val secret2: List[Bit] = List(0, 0, 1, 1, 1, 0)
  def decodedSecret2: List[Char] = decode(createCodeTree(input), secret2)

  // Part 4a: Encoding using Huffman tree

  /**
   * This function encodes `text` using the code tree `tree`
   * into a sequence of bits.
   */
  def encodeSingleChar(tree: CodeTree, ch: Char, acc: List[Bit]): List[Bit] = {
    if (chars(tree).indexOf(ch) == -1) {
      //println("Acc NA: " + acc)
      acc
    }
    tree match {
      case Leaf(ch, wt) =>
        //println("Acc leaf: " + acc)
        acc
      case Fork(left, right, chs, wt) =>
        if (chars(left).indexOf(ch) != -1) encodeSingleChar(left, ch, acc ::: List(0))
        else if (chars(right).indexOf(ch) != -1) encodeSingleChar(right, ch, acc ::: List(1))
        else {
          //println("Acc: " + acc)
          //acc
          throw new Error("Invalid Huffman Code Tree")
        }
    }
  }
  val frenchEncoder = encode(frenchCode)(_)//(List('a','b','c'))
  def encode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    //println("Chars: " + text)
    text match {
      case List() => Nil
      case x :: xs => encodeSingleChar(tree, x, List()) ::: encode(tree)(xs)
    }
  }

  def encodedSecret2: List[Bit] = encode(createCodeTree(input))(List('a', 'a', 'b', 'k'))

  // Part 4b: Encoding using code table
  // Code Table is a list of tuples of char and a list of int/bit
  type CodeTable = List[(Char, List[Bit])]

  /**
   * This function returns the bit sequence that represents the character `char` in
   * the code table `table`.
   */
  def codeBits(table: CodeTable)(char: Char): List[Bit] = table match {
    case List() => List()
    case x :: xs => if (x._1 == char) x._2 else codeBits(xs)(char)
  }

  /**
   * Given a code tree, create a code table which contains, for every character in the
   * code tree, the sequence of bits representing that character.
   *
   * Hint: think of a recursive solution: every sub-tree of the code tree `tree` is itself
   * a valid code tree that can be represented as a code table. Using the code tables of the
   * sub-trees, think of how to build the code table for the entire tree.
   */
  def convert(tree: CodeTree): CodeTable = tree match {
    case Leaf(ch, wt) =>
      //println("I am a leaf")
      List((ch, List()))
    case Fork(left, right, chars, wt) =>
      (left, right) match {
        case (Leaf(chl, wtl), Leaf(chr, wtr)) =>
          //println("1")
          //println ("Code Table 1: " + ctm)
          mergeCodeTables(List((chl, List(0))), List((chr, List(1))))
        case (Leaf(chl, wtl), Fork(rleft, rright, rchars, rwt)) =>
          //println("2")
          val ctm = rchars map (x => List((x, List(1)))) reduceLeft ((x, y) => mergeCodeTables(x, y))
          //println ("Code Table 2: " + ctm)
          //mergeCodeTables(mergeCodeTables(List((chl, List(0))), ctm), convert(right))
          mergeCodeTables(ctm, mergeCodeTables(List((chl, List(0))), convert(right)))
        case (Fork(lleft, lright, lchars, lwt), Leaf(chr, wtr)) =>
          //println("3")
          val ctm = lchars map (x => List((x, List(0)))) reduceLeft ((x, y) => mergeCodeTables(x, y))
          //println ("Code Table 3: " + ctm)
          //mergeCodeTables(convert(left), mergeCodeTables(ctm, List((chr, List(1)))))
          mergeCodeTables(ctm, mergeCodeTables(convert(left), List((chr, List(1)))))
        case (Fork(lleft, lright, lchars, lwt), Fork(rleft, rright, rchars, rwt)) =>
          //println("4")
          val ctml = lchars map (x => List((x, List(0)))) reduceLeft ((x, y) => mergeCodeTables(x, y))
          val ctmr = rchars map (x => List((x, List(1)))) reduceLeft ((x, y) => mergeCodeTables(x, y))
          //println ("Code Table 4L: " + ctml)
          //println ("Code Table 4R: " + ctmr)
          //mergeCodeTables(mergeCodeTables(ctml, convert(left)), mergeCodeTables(convert(right), ctmr))
          mergeCodeTables(mergeCodeTables(ctml, ctmr), mergeCodeTables(convert(left), convert(right)))
      }
  }

  /**
   * This function takes two code tables and merges them into one. Depending on how you
   * use it in the `convert` method above, this merge method might also do some transformations
   * on the two parameter code tables.
   */
  def mergeCodeTables(a: CodeTable, b: CodeTable): CodeTable = (a, b) match {
    case (a, List()) => a
    case (List(), b) => b
    case (List(), List()) => List()
    //case ((x, List()) :: xs, (y, yb :: ybs) :: ys) => throw new Error("Invalid Code Table")
    //case ((x, xb :: xbs) :: xs, (y, List()) :: ys) => throw new Error("Invalid Code Table")
    //case ((x, List()) :: xs, (y, List()) :: ys) => throw new Error("Invalid Code Table")
    case ((x, xbs) :: xs, (y, ybs) :: ys) =>
      //println("OK")
      val (matched, notmatched) = b partition (i => i._1 == x)
      //println("matched: " + matched)
      matched match {
        case List() => (x, xbs) :: mergeCodeTables(xs, notmatched)
        case (m, mbs) :: ms =>
          (x, xbs ::: mbs) :: mergeCodeTables(xs, notmatched)
      }
  }

  /**
   * This function encodes `text` according to the code tree `tree`.
   *
   * To speed up the encoding process, it first converts the code tree to a code table
   * and then uses it to perform the actual encoding.
   */
  def quickEncode(tree: CodeTree)(text: List[Char]): List[Bit] = {
    val ct = convert(tree)
    def qEncode(ctable: CodeTable)(text: List[Char]): List[Bit] = {
      text match {
        case List() => List()
        case x :: xs =>
          val mcode = ct filter (i => i._1 == x)
          mcode match {
            case List() => throw new Error("Char not found")
            case (y, ybs) :: ys =>
              ybs ::: qEncode(ct)(text drop 1)
          }
      }
    }
    qEncode(ct)(text)
  }

  def encodedSecret3: List[Bit] = quickEncode(createCodeTree(input))(List('a', 'a', 'b', 'k'))
}
