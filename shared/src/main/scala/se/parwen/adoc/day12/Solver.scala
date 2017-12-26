package se.parwen.adoc.day12

import scala.annotation.tailrec

object Solver {

  case class Program(name: String, programs: List[Program])

  private def parse(input: String) = {
    val rows = input.split('\n').map { v =>
      val e = v.split("<->")
      (e(0).trim.toInt, e(1).split(',').map(s => s.trim.toInt).toList)
    }
    val c = rows.toList.flatMap { case (k, v) => v.map((k, _)) }
    val b = c.flatMap { case (k, v) => List((k, v), (v, k)) }
    b.toSet
  }

  private def getGroup(c: Int, m: Set[(Int, Int)]) = {
    @tailrec
    def loop(c: Set[Int], m: Set[(Int, Int)], acc: Set[Int] = Set()): Set[Int] = {
      val x = m
        .filter { case (k, v) => c.contains(k) || c.contains(v) }
        .flatMap { case (k, v) => Set(k, v) }
        .filter(s => !c.contains(s) && !acc.contains(s))

      if (x.nonEmpty) {
        loop(x, m, acc ++ c)
      } else
        acc ++ c
    }
    loop(Set(c), m)
  }

  def solveStep1(input: String): Int = {
    getGroup(0, parse(input)).size
  }

  def solveStep2(input: String): Int = {
    val inp = parse(input)
    val all = inp.map(_._1)

    @tailrec
    def loop(start: Int, acc: List[Set[Int]], remaning: Set[Int]): List[Set[Int]] = {
      val group = getGroup(start,inp)
      val rest = remaning.diff(group)
      if(rest.isEmpty) acc :+ group else loop(rest.head, acc :+ group, rest)
    }
    loop(0, List(), all).size
  }
}

trait Solver12 {
  def solve12(): Unit = {
    println("Day 12:")
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}