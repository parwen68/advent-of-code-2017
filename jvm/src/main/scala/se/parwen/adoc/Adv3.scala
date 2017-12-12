package se.parwen.adoc

import se.parwen.adoc.day3.Solver

object Adv3 {

  def main(args: Array[String]): Unit = {
    val result1 = Solver.solveStep1(289326)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(289326)
    println(s"step 2 result is $result2")
  }

}