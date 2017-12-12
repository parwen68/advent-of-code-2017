package se.parwen.adoc.day6

object Solver {

  def stream(value: Int, length: Int, count: Int = 0) : Stream[Int] = {
    if (count == length) Stream.empty
    else Math.ceil(value.asInstanceOf[Double] / length).asInstanceOf[Int] #:: stream(value-1, length, count + 1)
  }

  def solveStep1(input: List[Int]): Int = {

    def loop(acc: List[List[Int]], list: List[Int]) : Int = {
      val (max, idx) = (list.max, list.indexOf(list.max))

      val dist = stream(list(idx), list.length).toList
      val shifted = dist.takeRight(idx + 1) ++ dist.dropRight(idx + 1)

      val nextList = list.updated(idx, 0).zip(shifted).map {case (a,b) => a + b}

      if (acc contains nextList) acc.length
      else loop(nextList :: acc, nextList)
    }
    loop(List(input), input)
  }

  def solveStep2(input: List[Int]): Int = {

    def loop(acc: List[List[Int]], list: List[Int]) : Int = {
      val (max, idx) = (list.max, list.indexOf(list.max))

      val dist = stream(list(idx), list.length).toList
      val shifted = dist.takeRight(idx + 1) ++ dist.dropRight(idx + 1)

      val nextList = list.updated(idx, 0).zip(shifted).map {case (a,b) => a + b}

      if (acc contains nextList) {
        val firstIdx = acc.indexOf(nextList)
        firstIdx + 1
      }
      else loop(nextList :: acc, nextList)
    }
    loop(List(input), input)
  }
}

trait Solver6 {
  def solve6() {
    println("Day 6:")
    val result1 = Solver.solveStep1(List(0,5,10,0,11,14,13,4,11,8,8,7,1,4,12,11))
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(List(0,5,10,0,11,14,13,4,11,8,8,7,1,4,12,11))
    println(s"step 2 result is $result2")
  }
}