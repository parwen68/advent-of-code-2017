package se.parwen.adoc.day15

import org.scalatest.{FlatSpec, Matchers}

class SolverSpec extends FlatSpec with Matchers {

  "Solver step1" should "return 588" in {
    Solver.solveStep1(65, 8921) should be (588)
  }

  "Solver step2" should "return 309" in {
    Solver.solveStep2(65, 8921) should be (309)
  }
}
