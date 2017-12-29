package se.parwen.adoc.day19

import org.scalatest._

class SolverSpec extends FlatSpec with Matchers {

  "Solver parse" should "return 14x6 array" in {

    val a = Solver.parse("""     |
                           $     |  +--+
                           $     A  |  C
                           $ F---|----E|--+
                           $     |  |  |  D
                           $     +B-+  +--+
                           $""".stripMargin('$'))

    a(0,0) should be (' ')
    a(5,0) should be ('|')
  }

  "" should "" in {

    val a = Solver.solveStep1("""     |
                                $     |  +--+
                                $     A  |  C
                                $ F---|----E|--+
                                $     |  |  |  D
                                $     +B-+  +--+
                                $""".stripMargin('$'))

    a should be ("ABCDEF")
  }

}
