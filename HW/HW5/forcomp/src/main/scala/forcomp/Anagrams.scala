package forcomp

import common._

object Anagrams {

  /** A word is simply a `String`. */
  type Word = String

  /** A sentence is a `List` of words. */
  type Sentence = List[Word]

  /**
   * `Occurrences` is a `List` of pairs of characters and positive integers saying
   *  how often the character appears.
   *  This list is sorted alphabetically w.r.t. to the character in each pair.
   *  All characters in the occurrence list are lowercase.
   *
   *  Any list of pairs of lowercase characters and their frequency which is not sorted
   *  is **not** an occurrence list.
   *
   *  Note: If the frequency of some character is zero, then that character should not be
   *  in the list.
   */
  type Occurrences = List[(Char, Int)]

  /**
   * The dictionary is simply a sequence of words.
   *  It is predefined and obtained as a sequence using the utility method `loadDictionary`.
   */
  val dictionary: List[Word] = loadDictionary

  /**
   * Converts the word into its character occurence list.
   *
   *  Note: the uppercase and lowercase version of the character are treated as the
   *  same character, and are represented as a lowercase character in the occurrence list.
   */
  def wordOccurrences(w: Word): Occurrences = {
    val map = w.toLowerCase groupBy (c => c)
    //println("wordOccurrences: map: " + map)
    //println("wordOccurrences: map.toList.sortBy(_._1): " + map.toList.sortBy(_._1))
    for ((ch, str) <- map.toList.sortBy(_._1)) yield (ch, str.length)
  }
  /** Converts a sentence into its character occurrence list. */
  def sentenceOccurrences(s: Sentence): Occurrences = {
    //println("sentenceOccurrences: s: " + s)
    val sfwo = s flatMap wordOccurrences
    //println("sentenceOccurrences: s flatMap wordOccurrences: " + (s flatMap wordOccurrences))
    val sfwogroupby = sfwo groupBy (c => c._1)
    //println("sentenceOccurrences: s flatMap wordOccurrences groupBy (c => c._1): " + sfwogroupby)
    //println("sentenceOccurrences: s flatMap wordOccurrences groupBy (c => c._1) toList: " + (sfwogroupby toList))
    //println("sentenceOccurrences: (s flatMap wordOccurrences groupBy (c => c._1) toList).sortBy(_._1): " + (sfwogroupby toList).sortBy(_._1))
    (s flatMap wordOccurrences groupBy (c => c._1) toList).sortBy(_._1) map (kv => (kv._1, kv._2.map(v => v._2).sum))

  }
  /**
   * The `dictionaryByOccurrences` is a `Map` from different occurrences to a sequence of all
   *  the words that have that occurrence count.
   *  This map serves as an easy way to obtain all the anagrams of a word given its occurrence list.
   *
   *  For example, the word "eat" has the following character occurrence list:
   *
   *     `List(('a', 1), ('e', 1), ('t', 1))`
   *
   *  Incidentally, so do the words "ate" and "tea".
   *
   *  This means that the `dictionaryByOccurrences` map will contain an entry:
   *
   *    List(('a', 1), ('e', 1), ('t', 1)) -> Seq("ate", "eat", "tea")
   *
   */
  lazy val dictionaryByOccurrences: Map[Occurrences, List[Word]] =
    // Breakdown:
    // dictionary is a list of Word
    // for each word generate a tuple of (word, Occurrences)
    // you end up with a list of those tuples: List[(Word, List[(Char, Int)]]
    // Group the list by the Occurrences, so you end up with:
    // a map of Occurrences(key) to List[(Word, Occurrences)](value)
    // Almost done, but what we need is a map of [Occurrences, List[Word]] instead
    //, so map the map with (kv => (kv._1, kv._2 map (_._1)))
    //, for the map value only get the first element of the tuple which is the Word
    dictionary map (s => (s, wordOccurrences(s))) groupBy (_._2) map (kv => (kv._1, kv._2 map (_._1))) withDefaultValue List()

  /** Returns all the anagrams of a given word. */
  def wordAnagrams(word: Word): List[Word] =
    dictionaryByOccurrences(wordOccurrences(word))

  /**
   * Returns the list of all subsets of the occurrence list.
   *  This includes the occurrence itself, i.e. `List(('k', 1), ('o', 1))`
   *  is a subset of `List(('k', 1), ('o', 1))`.
   *  It also include the empty subset `List()`.
   *
   *  Example: the subsets of the occurrence list `List(('a', 2), ('b', 2))` are:
   *
   *    List(
   *      List(),
   *      List(('a', 1)),
   *      List(('a', 2)),
   *      List(('b', 1)),
   *      List(('a', 1), ('b', 1)),
   *      List(('a', 2), ('b', 1)),
   *      List(('b', 2)),
   *      List(('a', 1), ('b', 2)),
   *      List(('a', 2), ('b', 2))
   *    )
   *
   *  Note that the order of the occurrence list subsets does not matter -- the subsets
   *  in the example above could have been displayed in some other order.
   */
  def combinations(occurrences: Occurrences): List[Occurrences] = occurrences match {
    case List() => List(List())
    case x :: xs =>
      val subsets = combinations(occurrences drop 1)
      // Cross combine the original subset with a new occurence
      val subsetcombo = for {
        subset <- subsets
        //debug = println("A Subset: " + subset)
        i <- 1 to occurrences.head._2
        //debug2 = println("OCC Head: " + occurrences.head._1 + ", " + i)
      } yield (occurrences.head._1, i) :: subset
      // Combine the original subset with a new subset
      subsets ::: subsetcombo
  }

  /**
   * Subtracts occurrence list `y` from occurrence list `x`.
   *
   *  The precondition is that the occurrence list `y` is a subset of
   *  the occurrence list `x` -- any character appearing in `y` must
   *  appear in `x`, and its frequency in `y` must be smaller or equal
   *  than its frequency in `x`.
   *
   *  Note: the resulting value is an occurrence - meaning it is sorted
   *  and has no zero-entries.
   */
  def subtract(x: Occurrences, y: Occurrences): Occurrences = (x, y) match {
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
            else throw new Exception("Invalid Operation, y is not a subset of x")
          } else throw new Exception("Invalid Operation, y is not a subset of x")
        }
        //def submap(xm: Map[Char, Int], yl: List[(Char, Int)]): Map[Char, Int] = {
        //  yl match {
        //   case List() => xm
        //    case x :: xs =>
        //      submap(subTerm(xm, yl.head), yl drop 1)
        //  }
        //}
        //submap(xmap, y).toList.sorted    
        ((ymap foldLeft xmap)(subTerm)).toList.sorted
      }
  }

  /**
   * Returns a list of all anagram sentences of the given sentence.
   *
   *  An anagram of a sentence is formed by taking the occurrences of all the characters of
   *  all the words in the sentence, and producing all possible combinations of words with those characters,
   *  such that the words have to be from the dictionary.
   *
   *  The number of words in the sentence and its anagrams does not have to correspond.
   *  For example, the sentence `List("I", "love", "you")` is an anagram of the sentence `List("You", "olive")`.
   *
   *  Also, two sentences with the same words but in a different order are considered two different anagrams.
   *  For example, sentences `List("You", "olive")` and `List("olive", "you")` are different anagrams of
   *  `List("I", "love", "you")`.
   *
   *  Here is a full example of a sentence `List("Yes", "man")` and its anagrams for our dictionary:
   *
   *    List(
   *      List(en, as, my),
   *      List(en, my, as),
   *      List(man, yes),
   *      List(men, say),
   *      List(as, en, my),
   *      List(as, my, en),
   *      List(sane, my),
   *      List(Sean, my),
   *      List(my, en, as),
   *      List(my, as, en),
   *      List(my, sane),
   *      List(my, Sean),
   *      List(say, men),
   *      List(yes, man)
   *    )
   *
   *  The different sentences do not have to be output in the order shown above - any order is fine as long as
   *  all the anagrams are there. Every returned word has to exist in the dictionary.
   *
   *  Note: in case that the words of the sentence are in the dictionary, then the sentence is the anagram of itself,
   *  so it has to be returned in this list.
   *
   *  Note: There is only one anagram of an empty sentence.
   */
  def sentenceAnagrams(sentence: Sentence): List[Sentence] = {

    def subSentence(occ: Occurrences): List[Sentence] = {
      //println("occ: "+occ)
      if (occ.isEmpty) List(List())
      else
        for {
          x <- combinations(occ)
          //debug = println("combo: " + x)
          y <- dictionaryByOccurrences(x)
          //debug = println("dict: " + y)
          z <- subSentence(subtract(occ, x))
        } yield y :: z
    }

    subSentence(sentenceOccurrences(sentence))
  }

}
