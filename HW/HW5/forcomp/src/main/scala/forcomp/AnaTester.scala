package forcomp

object AnaTester extends App {

  //val aword = Anagrams.wordAnagrams("elvis")
  //println("Anagram: " + aword)
  println("Word occurrences: " + Anagrams.wordOccurrences("ELVIS"))
  val sentence = List("yes","man")
  println("Sentence map WO: " + (sentence map Anagrams.wordOccurrences))
  val sentOcc = Anagrams.sentenceOccurrences(sentence)
  println("Sentence Occurrences: " + sentOcc)

  println("Combo Elvis: " + Anagrams.combinations(Anagrams.wordOccurrences("Elvis")))

  val combo = Anagrams.combinations(sentOcc)
  //val listWords = combo map Anagrams.dictionaryByOccurrences
  println("Sentence Anagrams: " + Anagrams.sentenceAnagrams(sentence))

  /*
  val a = List(('a', 2), ('b', 2), ('c', 2))
  val aa = List(('a', 2))
  val bb = List(('b', 2))
  println("Zipped: " + (aa ::: bb))
  println("Unzipped: " + (a unzip))

  println("LL: " + (for (i <- a) yield i._2))

  val rr = a map (v => (v._1, for (i <- 1 to v._2) yield i))

  println("RR: " + rr)

  val tt = for {
    i <- a
    j <- 1 to i._2

  } yield List(i._1, j)

  //a combinations 1

  println("TT: " + (a.toSet.subsets).mkString)

  val occ1 = a
  val occ2 = List(('c', 2), ('d', 2))

  //val comb = occ1 combinations 3

  //println("Comb: " + comb.toList)

  def combine(occ: List[List[(Char, Int)]], occ2: List[List[(Char, Int)]]): List[List[(Char, Int)]] = {
    val co = for {
      i <- occ.flatten
      j <- occ2.flatten
    } yield List(i) ::: List(j)
    co
  }

  def combinations(occ: List[(Char, Int)]): List[List[(Char, Int)]] =
    {
      /*
    def combo(acc: List[List[(Char, Int)]], occs: List[(Char, Int)]): List[List[(Char, Int)]] = occs match {
      case List() => List() :: acc
      case x :: xs => {
        // Get a subset of x
        val subset = {
          for {
            i <- 1 to x._2
          } yield (x._1, i)
        }.toList
        
      }

    }
    val expanded = for {
      i <- occ1
      subset = for {
        j <- 1 to i._2
      } yield List((i._1, j))
    } yield subset.toList
    expanded.flatten
    //val retval = for {
    // i <- expanded
    // j <- expanded drop i
    //combo = combine(i,j)
    //}
    //println("Exp: " + expanded)

    //    val retval =
    //    for {
    //    i <- 1 to occ.length
    //  combo <- expanded combinations i
    //flat = combo flatMap (s => s)
    //ff = flat.filter(x != s._1)
    //} yield flat
    //retval.toList
     *
     * 
     */
      def combo(oc: List[(Char, Int)]): List[List[(Char, Int)]] = {
        oc match {
          case List() => List(List())
          case x :: xs =>
            val subsets = combo(oc drop 1)
            val subsetcombo = for {
              subset <- subsets
              //debug0 = println("OCC: " + occ)
              debug = println("A Subset: " + subset)
              i <- 1 to oc.head._2
              debug2 = println("OCC Head: " + oc.head._1 + ", " + i)
              //debug1 = println("SS: " + subset)
              matched = subset filter (s => s._1 == oc.head._1)
              //debug3 = println("Length: " + matched.length)
              //if (matched.length == 0)
            } yield {
              println("Yield subset: " + subset)
              if (matched.length == 0) ((oc.head._1, i) :: subset)
              else List((oc.head._1, i))
            }
            subsets ::: subsetcombo
        }
      }
      combo(occ)
    }

  val combo = combinations(occ1)

  println("Combo: " + combinations(occ1))

  //val combo2 = combo combinations 2

  //println("Combo: " + (combo2 map (s => s)).mkString)

  //val clist = combo2.toList
  //val flat = {
  //for {
  //   i <- clist
  //    j <- i
  //  } yield j
  // }

  //println("Flat: " + flat)

  val v1 = List(('b', 2), ('b', 2), ('b', 3))
  val v2 = List(('b', 1), ('b', 2))

  val filtered = v1 filter (s => s._1 != 'b')
  println("Filtered: " + filtered)

  val v1exp = v1 map (a => for (i <- 1 to a._2) yield ((a._1, i)))

  println("v1exp: " + v1exp)

  val co = for {
    i <- v1
    j <- v2
    //println ("Res: " + List(i):::List(j))
  } yield (i, j)
  println("CO: " + co)

  val alist = List()
  val expanded =
    for {
      i <- occ1
      subset = for {
        j <- 1 to i._2
      } yield (i._1, j)
    } yield subset

  println("Expanded: " + expanded)

  val ltm = occ1.toMap

  println("OCC1: " + ltm.toList.sorted)

  def subtract(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)] = {
    (x, y) match {
      case (List(), List()) => List()
      case (xs :: xss, List()) => x
      case (List(), y :: ys) => List()
      case (xs :: xss, ys :: yss) =>
        if (x.length < y.length) List()
        else {
          val xmap = x.toMap
          def subTerm(map: Map[Char, Int], occ: (Char, Int)): Map[Char, Int] = {
            if (map contains occ._1) {
              val (ch, ct) = occ
              if (map(occ._1) > occ._2)
                map updated (occ._1, map(occ._1) - occ._2)
              else if (map(occ._1) == occ._2)
                map - occ._1
              else throw new Exception("Invalid Operation")
            } else throw new Exception("Invalid Operation")
          }
          def submap(xm: Map[Char, Int], yl: List[(Char, Int)]): Map[Char, Int] = {
            yl match {
              case List() => xm
              case x :: xs => {
                val newmap = subTerm(xm, yl.head)
                submap(newmap, yl drop 1)
              }
            }
          }
          submap(xmap, y).toList.sorted
        }
    }
  }

  def subtract2(x: List[(Char, Int)], y: List[(Char, Int)]): List[(Char, Int)] = {
    (x, y) match {
      case (List(), List()) => List()
      case (xs :: xss, List()) => x
      case (List(), y :: ys) => List()
      case (xs :: xss, ys :: yss) =>
        if (x.length < y.length) List()
        else {
          val xmap = x.toMap
          val ymap = y.toMap
          def subTerm(map: Map[Char, Int], occ: (Char, Int)): Map[Char, Int] = {
            if (map contains occ._1) {
              val (ch, ct) = occ
              if (map(occ._1) > occ._2)
                map updated (occ._1, map(occ._1) - occ._2)
              else if (map(occ._1) == occ._2)
                map - occ._1
              else throw new Exception("Invalid Operation")
            } else throw new Exception("Invalid Operation")
          }

          def submap(xm: Map[Char, Int], yl: List[(Char, Int)]): Map[Char, Int] = {
            yl match {
              case List() => xm
              case x :: xs => {
                val newmap = subTerm(xm, yl.head)
                submap(newmap, yl drop 1)
              }
            }
          }
          //submap(xmap, y).toList.sorted
          ((ymap foldLeft xmap)(subTerm)).toList.sorted
        }
    }
  }
  val v11 = List(('c', 4), ('b', 4), ('a', 3))
  val v22 = List(('a', 1), ('b', 2))

  val v12 = subtract2(v11, v22)
  println("SUb: " + v12)
  * 
  */
}