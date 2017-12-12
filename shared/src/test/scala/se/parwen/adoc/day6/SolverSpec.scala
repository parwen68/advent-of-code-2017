package se.parwen.adoc.day6

import org.scalatest._

class SolverSpec extends FlatSpec with Matchers {

  import Solver._
  "The Solver" should "return 5" in {
    solveStep1(List(0, 2, 7, 0)) should be (5)
  }

  "The Solver" should "return 4" in {
    solveStep2(List(0, 2, 7, 0)) should be (4)
  }

  "The stream blocks 10 and 4 banks" should "distribute like" in {
    stream(10,4).toList should be (List(3,3,2,2))
  }

  "The stream blocks 2 and 4 banks" should "distribute like" in {
    stream(2,4).toList should be (List(1,1,0,0))
  }

}
