package se.parwen.adoc.day15

object Solver {

  def generator(start: Long, factor: Long) : () => Long = {
    var prev = start
    () => {
      val next = prev * factor % 2147483647
      prev = next
      next
    }
  }

  def doMatch(first: Long, second: Long): Boolean = (first & 0xffff) == (second & 0xffff)

  def solveStep1(aStart: Int, bStart: Int): Int = {
    val genA = generator(aStart, 16807)
    val genB = generator(bStart, 48271)

    def loop(cnt: Long, matched: Int = 0) : Int= {
      if (cnt == 0)
        matched
      else
        loop(cnt - 1, matched + (if (doMatch(genA(), genB())) 1 else 0))
    }
    loop(40000000L)
  }

  def multGenerator(gen: () => Long, multiplier: Long) : () => Long = {
    () => {
      def loop(next: Long): Long = {
        if(next % multiplier == 0) next else loop(gen())
      }
      loop(gen())
    }
  }

  def solveStep2(aStart: Int, bStart: Int): Int = {
    val genA = generator(aStart, 16807)
    val genB = generator(bStart, 48271)

    (0L until 5000000).toStream.count(_ => doMatch(multGenerator(genA, 4)(), multGenerator(genB, 8)()))
  }

}

trait Solver15 {
  def solve15(): Unit = {
    println("Day 15:")
    val result1 = Solver.solveStep1(Input.input('A'), Input.input('B'))
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input('A'), Input.input('B'))
    println(s"step 2 result is $result2")
  }
}

object Main extends Solver15 {
  def main(args: Array[String]): Unit = {
    solve15()
  }
}