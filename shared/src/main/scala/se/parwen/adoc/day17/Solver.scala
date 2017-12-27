package se.parwen.adoc.day17

object Solver {

  def move(currPos: Int, steps: Int, bufferSize: Int): Int = {
    val laps = steps / bufferSize
    val reminder = steps % bufferSize

    if(currPos + reminder >= bufferSize) {
      currPos + reminder - bufferSize
    } else {
      currPos + reminder
    }
  }

  def insert(pos: Int, value: Int, buffer: List[Int]) : List[Int] = {
    buffer.patch(pos, List(value), 0)
  }

  def solve(steps: Int, turns: Int) : (List[Int], Int) = {

    def loop(cnt: Int = 0, pos: Int = 0, buffer: List[Int] = List(0)): (List[Int], Int) = {
      if (cnt == turns) {
        (buffer, pos)
      } else {
        val newPos = move(pos, steps, buffer.size)
        loop(cnt + 1,  newPos + 1, insert(newPos + 1, cnt + 1, buffer))
      }
    }
    loop()
  }

  def solveStep1(steps: Int, turns: Int): Int = {
    val (buffer, pos) = solve(steps, turns)
    buffer(pos+1)
  }

  def solveStep2(steps: Int, turns: Int): Int = {

    def loop(cnt: Int = 0, pos: Int = 0, bufferSize: Int = 1, value: Int = -1): Int = {
      if (cnt == turns) {
        value
      } else {
        val newPos = move(pos, steps, bufferSize)
        loop(cnt + 1,  newPos + 1, bufferSize + 1, if(newPos == 0) cnt +1 else value)
      }
    }
    loop()
  }
}

trait Solver17 {
  def solve17(): Unit = {
    println("Day 17:")
    val result1 = Solver.solveStep1(371, 2017)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(371, 50000000)
    println(s"step 2 result is $result2")
  }
}

object Main extends Solver17 {
  def main(args: Array[String]): Unit = {
    solve17()
  }
}