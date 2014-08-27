package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class BloxorzSuite extends FunSuite {

  trait SolutionChecker extends GameDef with Solver with StringParserTerrain {
    /**
     * This method applies a list of moves `ls` to the block at position
     * `startPos`. This can be used to verify if a certain list of moves
     * is a valid solution, i.e. leads to the goal.
     */
    def solve(ls: List[Move]): Block =
      ls.foldLeft(startBlock) {
        case (block, move) => move match {
          case Left => block.left
          case Right => block.right
          case Up => block.up
          case Down => block.down
        }
      }
  }

  trait Level1 extends SolutionChecker {
    /* terrain for level 1*/

    val level =
      """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin

    val optsolution = List(Right, Right, Down, Right, Right, Right, Down)
  }

  test("terrain function level 1") {
    new Level1 {
      assert(terrain(Pos(0, 0)), "0,0")
      assert(!terrain(Pos(4, 11)), "4,11")
    }
  }

  test("findChar level 1") {
    new Level1 {
      assert(startPos == Pos(1, 1))
    }
  }

  test("neighbors level 1") {
    new Level1 {
      val block = Block(Pos(3, 3), Pos(3, 3))
      val neighbors = List((Block(Pos(3, 1), Pos(3, 2)), Left),
        (Block(Pos(3, 4), Pos(3, 5)), Right),
        (Block(Pos(1, 3), Pos(2, 3)), Up),
        (Block(Pos(4, 3), Pos(5, 3)), Down))
      assert(block.neighbors == neighbors)
    }
  }

  test("legal neighbors level 1") {
    new Level1 {
      val block = Block(Pos(1, 1), Pos(1, 1))
      val neighbors = List((Block(Pos(1, 2), Pos(1, 3)), Right),
        (Block(Pos(2, 1), Pos(3, 1)), Down))
      assert(block.legalNeighbors == neighbors)
    }
  }

  test("neighbors with history level 1") {
    new Level1 {
      val hist = neighborsWithHistory(Block(Pos(1, 1), Pos(1, 1)), List(Left, Up))
      val answer = Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      //println("Hist: " + hist.toSet)
      assert(hist.take(2).toSet == answer)
    }
  }

  test("new neighbors only level1") {
    new Level1 {
      val test = newNeighborsOnly(Set((Block(Pos(1, 2), Pos(1, 3)), List(Right, Left, Up)),
        (Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up))).toStream,
        Set(Block(Pos(1, 2), Pos(1, 3)), Block(Pos(1, 1), Pos(1, 1))))
      val answer = Set((Block(Pos(2, 1), Pos(3, 1)), List(Down, Left, Up)))
      assert(test.toSet == answer)
    }
  }
  
  test("optimal solution for level 1") {
    new Level1 {
      assert(solve(solution) == Block(goal, goal))
    }
  }

  test("optimal solution length for level 1") {
    new Level1 {
      assert(solution.length == optsolution.length)
    }
  }
}
