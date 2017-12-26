package se.parwen.adoc.day1

object Solver {

  def solveStep1(input: String) = {
    val pairs = input.zip(input.drop(input.length / 2) + input.take(input.length / 2))
    pairs.foldLeft(0)((acc, v) => if(v._1 == v._2) acc + (v._1 - '0') else acc)
  }

  def solveStep2(input: String) = {
    val pairs = input.zip(input.drop(input.length / 2) + input.take(input.length / 2))
    pairs.foldLeft(0)((acc, v) => if(v._1 == v._2) acc + (v._1 - '0') else acc)
  }
}

trait Solver1 {
  def solve1(): Unit = {
    println("Day 1:")
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}

object Main extends Solver1 {
  def main(args: Array[String]): Unit = {
    solve1()
  }
}