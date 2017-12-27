package se.parwen.adoc.day17

import org.scalatest._

class SolverSpec extends FlatSpec with Matchers {

  "The Solver move 3 from pos 0 in buffer [0]" should "be 0" in {
    val buffer = List(0)
    Solver.move(0, 3, buffer.size) should be (0)
  }

  "The Solver move 3 from pos 1 in buffer [0, 1]" should "be 0" in {
    val buffer = List(0, 1)
    Solver.move(1, 3, buffer.size) should be (0)
  }

  "The Solver move 3 from pos 1 in buffer [0, 2, 1]" should "be 1" in {
    val buffer = List(0, 2, 1)
    Solver.move(1, 3, buffer.size) should be (1)
  }

  "The Solver move 3 from pos 1 in buffer [0, 2, 3, 1]" should "be 0" in {
    val buffer = List(0, 2, 3, 1)
    Solver.move(1, 3, buffer.size) should be (0)
  }

  "The Solver insert 3 at 1 in buffer [0, 1, 2, 3]" should "be [0, 3, 1, 2, 3]" in {
    val buffer = List(0, 1, 2, 3)
    Solver.insert(1, 3, buffer) should be (List(0,3,1,2,3))
  }

  "The Solver insert 2 at 0 in buffer [0, 1, 2, 3]" should "be [2, 0, 1, 2, 3]" in {
    val buffer = List(0, 1, 2, 3)
    Solver.insert(0, 2, buffer) should be (List(2,0,1,2,3))
  }

  "The Solver insert 4 at 4 in buffer [0, 1, 2, 3]" should "be [0, 1, 2, 3, 4]" in {
    val buffer = List(0, 1, 2, 3)
    Solver.insert(4, 4, buffer) should be (List(0,1,2,3,4))
  }

  "The Solver solve (3, 1)" should "be 0,(1)" in {
    Solver.solve(3, 1) should be ((List(0,1), 1))
  }

  "The Solver solve (3, 2)" should "be 0,(2),1" in {
    Solver.solve(3, 2) should be ((List(0,2,1), 1))
  }

  "The Solver solve (3, 3)" should "be 0, 2, (3), 1" in {
    Solver.solve(3, 3) should be ((List(0, 2, 3, 1), 2))
  }

  "The Solver solve (3, 4)" should "be 0  2 (4) 3  1" in {
    Solver.solve(3, 4) should be ((List(0, 2, 4, 3, 1), 2))
  }

  "The Solver solve (3, 5)" should "be 0 (5) 2  4  3  1" in {
    Solver.solve(3, 5) should be ((List(0, 5, 2, 4, 3, 1), 1))
  }

  "The Solver solveStep1 (3, 3)" should "be 1" in {
    Solver.solveStep1(3, 3) should be (1)
  }

  "The Solver solveStep1 (3, 4)" should "be 3" in {
    Solver.solveStep1(3, 4) should be (3)
  }

  "The Solver solveStep1 (3, 5)" should "be 2" in {
    Solver.solveStep1(3, 5) should be (2)
  }

  "The Solver solveStep1 (3, 2017)" should "be 638" in {
    Solver.solveStep1(3, 2017) should be (638)
  }

}
