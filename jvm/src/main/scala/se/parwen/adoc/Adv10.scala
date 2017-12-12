package se.parwen.adoc

import se.parwen.adoc.day10._

object Adv10 {
  def main(args: Array[String]): Unit = {
    import Solver._
    val result1 = solveStep1((0 to 255).toList, Input.input)
    println(s"step 1 result is $result1")
    val result2 = solveStep2(Input.inputAsStr)
    println(s"step 2 result is $result2")
  }
}