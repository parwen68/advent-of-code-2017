package se.parwen.adoc

import se.parwen.adoc.day6._

object Adv6 {
  def main(args: Array[String]): Unit = {
    import Solver._
    val result1 = solveStep1(List(0,5,10,0,11,14,13,4,11,8,8,7,1,4,12,11))
    println(s"step 1 result is $result1")
    val result2 = solveStep2(List(0,5,10,0,11,14,13,4,11,8,8,7,1,4,12,11))
    println(s"step 2 result is $result2")
  }
}
