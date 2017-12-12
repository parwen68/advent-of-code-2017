package se.parwen.adoc.day5

import org.scalatest._

class SolverSpec extends FlatSpec with Matchers {

  import Solver._

  "The solver step1" should "return 5 for 0 3  0  1  -3" in {
    solveStep1(Array(0,3,0,1,-3)) should be (5)
  }

  "The solver step2" should "return 10 for 0 3  0  1  -3" in {
      solveStep2(Array(0,3,0,1,-3)) should be (10)
  }
}
