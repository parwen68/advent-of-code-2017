package se.parwen.adoc

import se.parwen.adoc.day9._

object Adv9 {
  def main(args: Array[String]): Unit = {
    import Solver._
    val result1 = solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}