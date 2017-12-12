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