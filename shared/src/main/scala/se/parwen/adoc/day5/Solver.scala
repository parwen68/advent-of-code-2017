package se.parwen.adoc.day5

object Solver {

  def solveStep1(list: Array[Int]): Int = {
    var ptr = 0
    var prevPtr = ptr

    import scala.util.control.Breaks._

    var cnt = 0
    breakable {
      while (true) {
        cnt += 1
        val v = list(ptr)
        ptr += v
        list(prevPtr) = list(prevPtr) + 1
        prevPtr = ptr
        if (ptr < 0 || ptr >= list.length) {
          //println(s"$cnt $ptr ${list.toList}")
          break
        }
      }
    }
    cnt
  }

  def solveStep2(list: Array[Int]): Int = {
    var ptr = 0
    var prevPtr = ptr

    import scala.util.control.Breaks._

    var cnt = 0
    breakable {
      while (true) {
        cnt += 1
        val v = list(ptr)
        ptr += v
        list(prevPtr) = list(prevPtr) + (if (v >= 3) -1 else 1)
        prevPtr = ptr
        if (ptr < 0 || ptr >= list.length) {
          //println(s"$cnt $ptr ${list.toList}")
          break
        }
      }
    }
    cnt
  }
}

trait Solver5 {
  def solve5(): Unit = {
    println("Day 5:")
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}