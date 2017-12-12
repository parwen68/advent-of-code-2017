package se.parwen.adoc.day4

import org.scalatest._

class SolverSpec extends FlatSpec with Matchers {

  import Solver._

  "The solver step 1" should "return valid for aa bb cc dd ee" in {
    solveStep1("aa bb cc dd ee") should be(1)
  }

  "The solver step 1" should "return invalid for aa bb cc dd aa" in {
    solveStep1("aa bb cc dd aa") should be(0)
  }

  "The solver step 1" should "return valid for aa bb cc dd aaa" in {
    solveStep1("aa bb cc dd aaa") should be(1)
  }
}
