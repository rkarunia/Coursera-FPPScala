package objsets

import common._
import TweetReader._

/**
 * A class to represent tweets.
 */
class Tweet(val user: String, val text: String, val retweets: Int) {
  override def toString: String =
    "User: " + user + "\n" +
      "Text: " + text + " [" + retweets + "]"
}

/**
 * This represents a set of objects of type `Tweet` in the form of a binary search
 * tree. Every branch in the tree has two children (two `TweetSet`s). There is an
 * invariant which always holds: for every branch `b`, all elements in the left
 * subtree are smaller than the tweet at `b`. The eleemnts in the right subtree are
 * larger.
 *
 * Note that the above structure requires us to be able to compare two tweets (we
 * need to be able to say which of two tweets is larger, or if they are equal). In
 * this implementation, the equality / order of tweets is based on the tweet's text
 * (see `def incl`). Hence, a `TweetSet` could not contain two tweets with the same
 * text from different users.
 *
 *
 * The advantage of representing sets as binary search trees is that the elements
 * of the set can be found quickly. If you want to learn more you can take a look
 * at the Wikipedia page [1], but this is not necessary in order to solve this
 * assignment.
 *
 * [1] http://en.wikipedia.org/wiki/Binary_search_tree
 */
abstract class TweetSet {

  /**
   * This method takes a predicate and returns a subset of all the elements
   * in the original set for which the predicate is true.
   *
   * Question: Can we implment this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def filter(p: Tweet => Boolean): TweetSet = filterAcc(p, new Empty)

  /**
   * This is a helper method for `filter` that propagates the accumulated tweets.
   */
  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet

  /**
   * Returns a new `TweetSet` that is the union of `TweetSet`s `this` and `that`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def union(that: TweetSet): TweetSet

  /**
   * Returns the tweet from this set which has the greatest retweet count.
   *
   * Calling `mostRetweeted` on an empty set should throw an exception of
   * type `java.util.NoSuchElementException`.
   *
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def mostRetweeted: Tweet
  def mrAcc(most: Int, mostTweet: Tweet): Tweet
  /**
   * Returns a list containing all tweets of this set, sorted by retweet count
   * in descending order. In other words, the head of the resulting list should
   * have the highest retweet count.
   *
   * Hint: the method `remove` on TweetSet will be very useful.
   * Question: Should we implement this method here, or should it remain abstract
   * and be implemented in the subclasses?
   */
  def descendingByRetweet: TweetList
  def getDescendingSortedTweetByRetweet(ts: TweetSet): TweetList

  def isEmpty: Boolean
  /**
   * The following methods are already implemented
   */

  /**
   * Returns a new `TweetSet` which contains all elements of this set, and the
   * the new element `tweet` in case it does not already exist in this set.
   *
   * If `this.contains(tweet)`, the current set is returned.
   */
  def incl(tweet: Tweet): TweetSet

  /**
   * Returns a new `TweetSet` which excludes `tweet`.
   */
  def remove(tweet: Tweet): TweetSet

  /**
   * tring
   * Tests if `tweet` exists in this `TweetSet`.
   */
  def contains(tweet: Tweet): Boolean

  /**
   * This method takes a function and applies it to every element in the set.
   */
  def foreach(f: Tweet => Unit): Unit
}

class Empty extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = acc

  def union(that: TweetSet): TweetSet = that

  def mostRetweeted: Tweet = throw new java.util.NoSuchElementException("This is an Empty TweetSet")
  def mrAcc(most: Int, mostTweet: Tweet): Tweet = mostTweet

  def descendingByRetweet: TweetList = Nil
  def getDescendingSortedTweetByRetweet(ts: TweetSet): TweetList = Nil

  def isEmpty: Boolean = true
  /**
   * The following methods are already implemented
   */
  def contains(tweet: Tweet): Boolean = false

  def incl(tweet: Tweet): TweetSet = new NonEmpty(tweet, new Empty, new Empty)

  def remove(tweet: Tweet): TweetSet = this

  def foreach(f: Tweet => Unit): Unit = ()

  override def toString: String = "."
}

class NonEmpty(elem: Tweet, left: TweetSet, right: TweetSet) extends TweetSet {

  def filterAcc(p: Tweet => Boolean, acc: TweetSet): TweetSet = {
    val accleft = left filterAcc (p, if (p(elem)) acc incl elem else acc)
    val accright = right filterAcc (p, if (p(elem)) accleft incl elem else accleft)
    accright
  }

  def union(that: TweetSet): TweetSet = filterAcc(Tweet => true, that)

  def mostRetweeted: Tweet = mrAcc(elem.retweets, elem)

  def mrAcc(most: Int, mostTweet: Tweet): Tweet = {
    val mostLeft = left mrAcc (if (elem.retweets > most)elem.retweets else most, if (elem.retweets > most) elem else mostTweet)
    val mostRight = right mrAcc (mostLeft.retweets, mostLeft)
    mostRight
  }

  def descendingByRetweet: TweetList = {
    val mostPopular = mostRetweeted
    new Cons(mostPopular, getDescendingSortedTweetByRetweet(remove(mostPopular)))
  }

  def getDescendingSortedTweetByRetweet(ts: TweetSet): TweetList = {
    if (ts.isEmpty) Nil 
    else {
      val mostPopular = ts.mostRetweeted
      val newTree = ts.remove(mostPopular)
      new Cons(mostPopular, getDescendingSortedTweetByRetweet(newTree))
    }
  }
  
  def isEmpty: Boolean = false
  
  /**
   * The following methods are already implemented
   */

  def contains(x: Tweet): Boolean =
    if (x.text < elem.text) left.contains(x)
    else if (elem.text < x.text) right.contains(x)
    else true

  def incl(x: Tweet): TweetSet = {
    if (x.text < elem.text) new NonEmpty(elem, left.incl(x), right)
    else if (elem.text < x.text) new NonEmpty(elem, left, right.incl(x))
    else this
  }

  def remove(tw: Tweet): TweetSet =
    if (tw.text < elem.text) new NonEmpty(elem, left.remove(tw), right)
    else if (elem.text < tw.text) new NonEmpty(elem, left, right.remove(tw))
    else left.union(right)

  def foreach(f: Tweet => Unit): Unit = {
    f(elem)
    left.foreach(f)
    right.foreach(f)
  }

  override def toString: String = "{" + left + elem + right + "}"
}

trait TweetList {
  def head: Tweet
  def tail: TweetList
  def isEmpty: Boolean
  def foreach(f: Tweet => Unit): Unit =
    if (!isEmpty) {
      f(head)
      tail.foreach(f)
    }
}

object Nil extends TweetList {
  def head = throw new java.util.NoSuchElementException("head of EmptyList")
  def tail = throw new java.util.NoSuchElementException("tail of EmptyList")
  def isEmpty = true
}

class Cons(val head: Tweet, val tail: TweetList) extends TweetList {
  def isEmpty = false
}

object GoogleVsApple {
  val google = List("android", "Android", "galaxy", "Galaxy", "nexus", "Nexus")
  val apple = List("ios", "iOS", "iphone", "iPhone", "ipad", "iPad")
/*
  val c1 = new Tweet("c", "c body i like aple product i hate  ipad", 87)  
  val c2 = new Tweet("c", "c body i like aple product i hate Nexus galaxy", 77) 
  val c3 = new Tweet("c", "c body i like aple product i hate  ipad and Ipad", 67) 
  val c4 = new Tweet("c", "c body i like aple product i hate Nexus ios", 57) 
  val c5 = new Tweet("c", "c body i like aple product i hate 345", 47) 
  val c6 = new Tweet("c", "c body i like aple product i hate 567", 37) 
  val c7 = new Tweet("c", "c body i like aple product i hate 76544", 27) 
  val c8 = new Tweet("c", "c body i like aple product i hate Nexu", 6) 
  val c9 = new Tweet("c", "c body i like aple product i hate ios", 17) 
  val c10 = new Tweet("c", "c body i like aple product i hate galaxy", 7)
  val set1 = new Empty 
  val set2 = set1 incl c1 incl c2 incl c3 incl c4 incl c8 incl c7 incl c6 incl c5 incl c9 incl c10
*/
  val googleTweetPredicate = (twt: Tweet) => google.exists(s => twt.text.contains(s))
  val appleTweetPredicate  = (twt: Tweet) => apple.exists (s => twt.text.contains(s))
  //val googleAppleTweetPredicate = (twt: Tweet) => apple.exists (s => twt.text.contains(s) || google.exists(s => twt.text.contains(s)))

  lazy val googleTweets: TweetSet = {
    TweetReader.allTweets.filter(googleTweetPredicate)   
  }    

  lazy val appleTweets: TweetSet  = {
    TweetReader.allTweets.filter(appleTweetPredicate)   
  } 
  /**
   * A list of all tweets mentioning a keyword from either apple or google,
   * sorted by the number of retweets.
   */
  lazy val trending: TweetList = {
    val allTweets = appleTweets union googleTweets
    allTweets.descendingByRetweet
  }
}

object Main extends App {
  // Print the trending tweets
  GoogleVsApple.trending foreach println
}
