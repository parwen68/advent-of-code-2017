package se.parwen.adoc.day8

import org.scalatest.{FlatSpec, Matchers}

class SolverSpec extends FlatSpec with Matchers {

  "The solver" should "return a and 1" in {
    import Solver._
    val (reg, v) = solveStep1("""b inc 5 if a > 1
                 |a inc 1 if b < 5
                 |c dec -10 if a >= 1
                 |c inc -20 if c == 10""".stripMargin)

    reg should be ("a")
    v should be (1)
  }

  "The solver" should "return max value 10" in {
    import Solver._
    val mx = solveStep2("""b inc 5 if a > 1
                 |a inc 1 if b < 5
                 |c dec -10 if a >= 1
                 |c inc -20 if c == 10""".stripMargin)

    mx should be (10)
  }
}
