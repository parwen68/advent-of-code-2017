package se.parwen.adoc.day9

import org.scalatest.{FlatSpec, Matchers}

class SolverSpec extends FlatSpec with Matchers {

  "The solvers step1" should "return 0 fo <>" in {
    import Solver._
    val r = solveStep1("<>")
    r should be (0)
  }

  "The solvers step1" should "return 1 for {}" in {
    import Solver._
    val r = solveStep1("{}")
    r should be (1)
  }

  "The solvers step1" should "return 6 for {{{}}}" in {
    import Solver._
    val r = solveStep1("{{{}}}")
    r should be (6)
  }

  "The solvers step1" should "return 5 for {{},{}}" in {
    import Solver._
    val r = solveStep1("{{},{}}")
    r should be (5)
  }

  "The solvers step1" should "return 16 for {{{},{},{{}}}}" in {
    import Solver._
    val r = solveStep1("{{{},{},{{}}}}")
    r should be (16)
  }

  "The solvers step1" should "return 1 for {<a>,<a>,<a>,<a>}" in {
    import Solver._
    val r = solveStep1("{<a>,<a>,<a>,<a>}")
    r should be (1)
  }

  "The solvers step1" should "return 9 for {{<ab>},{<ab>},{<ab>},{<ab>}}" in {
    import Solver._
    val r = solveStep1("{{<ab>},{<ab>},{<ab>},{<ab>}}")
    r should be (9)
  }

  "The solvers step1" should "return 9 for {{<!!>},{<!!>},{<!!>},{<!!>}}" in {
    import Solver._
    val r = solveStep1("{{<!!>},{<!!>},{<!!>},{<!!>}}")
    r should be (9)
  }

  "The solvers step1" should "return 3 for {{<a!>},{<a!>},{<a!>},{<ab>}}" in {
    import Solver._
    val r = solveStep1("{{<a!>},{<a!>},{<a!>},{<ab>}}")
    r should be (3)
  }

  "The solvers step1" should "return 0 for <>" in {
    import Solver._
    val r = solveStep1("<>")
    r should be (0)
  }

  "The solvers step2" should "return 0 for <>" in {
    import Solver._
    val r = solveStep2("<>")
    r should be (0)
  }

  "The solvers step2" should "return 17 for <random characters>" in {
    import Solver._
    val r = solveStep2("<random characters>")
    r should be (17)
  }

  "The solvers step2" should "return 3 for <<<<>" in {
    import Solver._
    val r = solveStep2("<<<<>")
    r should be (3)
  }

  "The solvers step2" should "return 2 for <{!>}>" in {
    import Solver._
    val r = solveStep2("<{!>}>")
    r should be (2)
  }

  "The solvers step2" should "return 0 for <!!>" in {
    import Solver._
    val r = solveStep2("<!!>")
    r should be (0)
  }

  "The solvers step2" should "return 0 for <!!!>>" in {
    import Solver._
    val r = solveStep2("<!!!>>")
    r should be (0)
  }

  "The solvers step2" should "return 10 for <{o\"i!a,<{i<a>" in {
    import Solver._
    val r = solveStep2("<{o\"i!a,<{i<a>")
    r should be (10)
  }
}
