package objsets

object tester {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  val set1 = new Empty                            //> set1  : objsets.Empty = .
  val set2 = set1.incl(new Tweet("a", "a body", 23))
                                                  //> set2  : objsets.TweetSet = {.User: a
                                                  //| Text: a body [23].}
  val set3 = set2.incl(new Tweet("b", "b body", 20))
                                                  //> set3  : objsets.TweetSet = {.User: a
                                                  //| Text: a body [23]{.User: b
                                                  //| Text: b body [20].}}
  val c = new Tweet("c", "c body", 7)             //> c  : objsets.Tweet = User: c
                                                  //| Text: c body [7]
  val d = new Tweet("d", "d body", 9)             //> d  : objsets.Tweet = User: d
                                                  //| Text: d body [9]
  val set4c = set3.incl(c)                        //> set4c  : objsets.TweetSet = {.User: a
                                                  //| Text: a body [23]{.User: b
                                                  //| Text: b body [20]{.User: c
                                                  //| Text: c body [7].}}}
  val set4d = set3.incl(d)                        //> set4d  : objsets.TweetSet = {.User: a
                                                  //| Text: a body [23]{.User: b
                                                  //| Text: b body [20]{.User: d
                                                  //| Text: d body [9].}}}
  val set5 = set4c.incl(d)                        //> set5  : objsets.TweetSet = {.User: a
                                                  //| Text: a body [23]{.User: b
                                                  //| Text: b body [20]{.User: c
                                                  //| Text: c body [7]{.User: d
                                                  //| Text: d body [9].}}}}
  
  set5 remove(set5 mostRetweeted)                 //> res0: objsets.TweetSet = {.User: b
                                                  //| Text: b body [20]{.User: c
                                                  //| Text: c body [7]{.User: d
                                                  //| Text: d body [9].}}}
                
  set5 descendingByRetweet                        //> res1: objsets.TweetList = objsets.Cons@6fee5635
  
  
 
}
 
  
  