package se.parwen.adoc.day10

import se.parwen.adoc.day10.Solver.solveForList

import scala.annotation.tailrec

object Solver {

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

  def solveStep1(list: List[Int], lengths: List[Int]) : Int = {
    val (r, _, _) = solveForList(list, lengths)
    r.head * r(1)
  }

  def toAscii(s: String): List[Int] = {
    s.map(_.toInt).toList
  }

  def solveStep2(input: String) : String = {
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
    loop(list, 0, 0, 64).grouped(16).map(l => l.fold(0)(_ ^ _)).map(v => f"$v%02x").mkString
  }
}