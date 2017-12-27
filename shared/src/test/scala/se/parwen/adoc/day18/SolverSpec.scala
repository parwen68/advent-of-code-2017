package se.parwen.adoc.day18

import org.scalatest._
import se.parwen.adoc.day18.Solver._

class SolverSpec extends FlatSpec with Matchers {

  "Solver parse" should "return" in {
    val a = Solver.parse("""set a 1
                   |add a 2
                   |mul a a
                   |mod a 5
                   |snd a
                   |set a 0
                   |rcv a
                   |jgz a -1
                   |set a 1
                   |jgz a -2""".stripMargin)

    a should be (Vector(Seti('a',1), Addi('a',2), Mulc('a','a'), Modi('a',5), Sndc('a'), Seti('a',0), Rcv('a'), Jgzi('a',-1), Seti('a',1), Jgzi('a',-2)))
  }
  "Solver solveStep1" should "return 4" in {
    val a = Solver.solveStep1("""set a 1
                   |add a 2
                   |mul a a
                   |mod a 5
                   |snd a
                   |set a 0
                   |rcv a
                   |jgz a -1
                   |set a 1
                   |jgz a -2""".stripMargin)

    a should be (4)
  }

  "" should "" in {
    val a = Solver.solveStep2("""snd 1
                                |snd 2
                                |snd p
                                |rcv a
                                |rcv b
                                |rcv c
                                |rcv d""".stripMargin)
    a should be (3)
  }
}
