package se.parwen.adoc.day10

import org.scalatest.{FlatSpec, Matchers}

class SolverSpec extends FlatSpec with Matchers {

  "The solver" should "handle 1" in {
    import Solver._
    val r = swap(List(0, 1, 2, 3, 4), 0, 3)

    r should be (List(2, 1, 0, 3, 4))
  }

  "The solver" should "handle 2" in {
    import Solver._
    val r = swap(List(2, 1, 0, 3, 4), 3, 4)

    r should be (List(4, 3, 0, 1, 2))

  }

  "The solver" should "handle 3" in {
    import Solver._
    val r = swap(List(4, 3, 0, 1, 2), 8, 1)

    r should be (List(4, 3, 0, 1, 2))

  }

  "The solver" should "handle 4" in {
    import Solver._
    val r = swap(List(4, 3, 0, 1, 2), 11, 5)

    r should be (List(3, 4, 2, 1, 0))
  }

  "The solver" should "return for list 0,1,2,3,4 and lengths 3,4,1,5" in {
    import Solver._
    val list = List(0,1,2,3,4)
    val lengths = List(3,4,1,5)
    val r = solveStep1(list, lengths)

    r should be (12)
  }

  "The solver" should "return for list 0,1,2,3,4 and lengths 3,4,0,5" in {
    import Solver._
    val list = List(0,1,2,3,4)
    val lengths = List(3,4,0,5)
    val r = solveStep1(list, lengths)

    r should be (2)
  }

  "" should "" in {
    import Solver._
    val r = toAscii("1,2,3")

    r should be (List(49,44,50,44,51))
  }

  "The solver step2" should "return a2582a3a0e66e6e86e3812dcb672a272 for empty string" in {
    import Solver._
    val r = solveStep2("")

    r should be ("a2582a3a0e66e6e86e3812dcb672a272")
  }

  "The solver step2" should "return 33efeb34ea91902bb2f59c9920caa6cd for AoC 2017g" in {
    import Solver._
    val r = solveStep2("AoC 2017")

    r should be ("33efeb34ea91902bb2f59c9920caa6cd")
  }

  "The solver step2" should "return 3efbe78a8d82f29979031a4aa0b16a9d for 1,2,3" in {
    import Solver._
    val r = solveStep2("1,2,3")

    r should be ("3efbe78a8d82f29979031a4aa0b16a9d")
  }

  "The solver step2" should "return 63960835bcdc130f0b66d7ff4f6a5a8e for 1,2,4" in {
    import Solver._
    val r = solveStep2("1,2,4")

    r should be ("63960835bcdc130f0b66d7ff4f6a5a8e")
  }
}
