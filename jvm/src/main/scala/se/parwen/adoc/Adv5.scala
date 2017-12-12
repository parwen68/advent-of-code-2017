package se.parwen.adoc

import se.parwen.adoc.day5._

object Adv5 {
  def main(args: Array[String]): Unit = {
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}