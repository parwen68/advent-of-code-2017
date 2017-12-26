package se.parwen.adoc.day14

import scala.annotation.tailrec

object Solver {

  def toAscii(s: String): List[Int] = {
    s.map(_.toInt).toList
  }

  def swap(l: List[Int], from: Int, length: Int) : List[Int] = {
    val nFrom = from % l.length
    val nTo = (from + length) % l.length

    if (length == 0) {
      l
    } else if (nFrom < nTo) {
      l.take(nFrom) ++ l.slice(nFrom, nTo).reverse ++ l.takeRight(l.length - nTo)
    } else {
      val a = (l.takeRight(l.length - nFrom) ++ l.take(nTo)).reverse
      a.takeRight(nTo) ++ l.slice(nTo, nFrom) ++ a.take(l.length - nFrom)
    }
  }

  def solveForList(list: List[Int], lengths: List[Int], cp : Int = 0, skip: Int = 0) : (List[Int], Int, Int) = {
    @tailrec
    def loop(innerList: List[Int], innerLengths: List[Int], cp : Int, skip: Int): (List[Int], Int, Int) = {
      innerLengths match {
        case Nil => (innerList, cp, skip)
        case length :: tail => loop(swap(innerList, cp, length), tail, cp + length + skip, skip + 1)
      }
    }
    loop(list, lengths, cp, skip)
  }

  def knotHash(input: String) : List[Int] = {
    val lengths = toAscii(input) ++ List(17, 31, 73, 47, 23)
    val list = (0 to 255).toList
    val cnt = 64

    def loop(innerList: List[Int], cp : Int, skip: Int, cnt: Int): List[Int] = {
      if (cnt == 0) innerList
      else {
        val (newList, newCp, newSkipskip) = solveForList(innerList, lengths, cp, skip)
        loop(newList, newCp, newSkipskip, cnt -1)
      }
    }
    loop(list, 0, 0, 64).grouped(16).map(l => l.fold(0)(_ ^ _)).toList //.map(v => f"$v%02x").mkString
  }

  def solveStep1(input: String): Int = {
    (0 to 127).map(v => s"$input-$v").map(knotHash).toList.flatten.flatMap(v => v.toBinaryString).count(_ == '1')
  }

  def solveStep2(input: String): Int = {
    val s = (0 to 127)
      .map(v => s"$input-$v")
      .map(knotHash)
      .map(v =>
        v.flatMap(v => f"${v.toBinaryString}%8s".replace(' ', '0')).toArray
      ).toArray

    val v = (for {
      x <- 0 to 127
      y <- 0 to 127
      if s(x)(y) == '1'
    } yield (x, y)).toSet


    def getNeighbors(c: (Int,Int), rest: Set[(Int,Int)]): List[(Int,Int)] = {
      val deltas = for {
        dx <- -1 to 1
        dy <- -1 to 1
        if Math.abs(dy + dx) == 1
      } yield (dx,dy)

      deltas.map(d => (c._1 + d._1, c._2 + d._2)).filter(rest(_)).toList
    }

    @tailrec
    def findGroup(next: List[(Int,Int)], cells: Set[(Int, Int)], acc: Set[(Int, Int)]) : (Set[(Int,Int)], Set[(Int,Int)])= {
      next match {
        case Nil => (cells, acc)
        case head :: tail =>
          val neighbors = getNeighbors(head, cells)
          findGroup(tail ++ neighbors, cells -- neighbors, acc ++ neighbors)
      }
    }

    @tailrec
    def loop(all: Set[(Int, Int)], cnt: Int): Int =
      if (all.isEmpty) cnt else loop(findGroup(List(all.head), all - all.head, Set())._1, cnt + 1)
    loop(v, 0)
  }

}

trait Solver14 {
  def solve14(): Unit = {
    println("Day 14:")
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}

object Main extends Solver14 {
  def main(args: Array[String]): Unit = {
    solve14()
  }
}