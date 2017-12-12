package se.parwen.adoc.day11

import org.scalatest.{FlatSpec, Matchers}

class SolverSpec extends FlatSpec with Matchers {

  "The solver step 1" should "return 3 for ne,ne,ne" in {
    import Solver._
    val r = solveStep1("ne,ne,ne")
    r should be (3)
  }

  "The solver step 1" should "return 0 for ne,ne,sw,sw" in {
    import Solver._
    val r = solveStep1("ne,ne,sw,sw")
    r should be (0)
  }

  "The solver step 1" should "return 2 for ne,ne,s,s" in {
    import Solver._
    val r = solveStep1("ne,ne,s,s")
    r should be (2)
  }

  "The solver step 1" should "return 3 for se,sw,se,sw,sw" in {
    import Solver._
    val r = solveStep1("se,sw,se,sw,sw")
    r should be (3)
  }
}
