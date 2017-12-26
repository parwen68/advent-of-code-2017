package se.parwen.adoc.day12

import org.scalatest.{FlatSpec, Matchers}

class SolverSpec extends FlatSpec with Matchers {

  "The solver step 1" should "return 6" in {
    import Solver._
    val r = solveStep1("""0 <-> 2
                         |1 <-> 1
                         |2 <-> 0, 3, 4
                         |3 <-> 2, 4
                         |4 <-> 2, 3, 6
                         |5 <-> 6
                         |6 <-> 4, 5""".stripMargin)

    r should be (6)
  }

  "The solver step 2" should "return 2" in {
    import Solver._
    val r = solveStep2("""0 <-> 2
                         |1 <-> 1
                         |2 <-> 0, 3, 4
                         |3 <-> 2, 4
                         |4 <-> 2, 3, 6
                         |5 <-> 6
                         |6 <-> 4, 5""".stripMargin)

    r should be (2)
  }
}
