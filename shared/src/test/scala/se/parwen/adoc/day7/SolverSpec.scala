package se.parwen.adoc.day7

import org.scalatest.{FlatSpec, Matchers}

class SolverSpec extends FlatSpec with Matchers {

  "The solver" should "return tknk" in {
    import Solver._
    val r = solveStep1("""pbga (66)
            |xhth (57)
            |ebii (61)
            |havc (66)
            |ktlj (57)
            |fwft (72) -> ktlj, cntj, xhth
            |qoyq (66)
            |padx (45) -> pbga, havc, qoyq
            |tknk (41) -> ugml, padx, fwft
            |jptl (61)
            |ugml (68) -> gyxo, ebii, jptl
            |gyxo (61)
            |cntj (57)""".stripMargin)

    r should be (List("tknk"))
  }
/*
  "The Solver" should "return stuff" in {
    import Solver._
    solveStep2("""pbga (66)
                 |xhth (57)
                 |ebii (61)
                 |havc (66)
                 |ktlj (57)
                 |fwft (72) -> ktlj, cntj, xhth
                 |qoyq (66)
                 |padx (45) -> pbga, havc, qoyq
                 |tknk (41) -> ugml, padx, fwft
                 |jptl (61)
                 |ugml (68) -> gyxo, ebii, jptl
                 |gyxo (61)
                 |cntj (57)""".stripMargin)
  }
  */
}

