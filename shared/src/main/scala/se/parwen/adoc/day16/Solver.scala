package se.parwen.adoc.day16

import scala.annotation.tailrec

object Solver {

  def spin(inp: Array[Char], steps: Int) : Array[Char] = {
    inp.takeRight(steps) ++ inp.dropRight(steps)
  }

  def exchange(inp: Array[Char], pos1: Int, pos2: Int) : Array[Char] = {
    inp.updated(pos1, inp(pos2)).updated(pos2, inp(pos1))
  }

  def partner(inp: Array[Char], n1: Char, n2: Char) : Array[Char] = {
    exchange(inp, inp.indexOf(n1), inp.indexOf(n2))
  }

  def parse(input: String): List[Command] = {
    input.split(',').map(s => (s(0), s.substring(1))).toList.map {
      case ('s', p) => Spin(p.toInt)
      case ('x', p) =>
        val s = p.split('/').map(_.toInt)
        Exchange(s(0), s(1))
      case ('p', p) =>
        val s = p.split('/').map(_ (0))
        Partner(s(0), s(1))
      case _ => throw new RuntimeException
    }
  }

  sealed trait Command
  case class Spin(steps: Int) extends Command
  case class Exchange(p1: Int, p2: Int) extends Command
  case class Partner(c1: Char, c2: Char) extends Command

  def solve(start: Array[Char], commands: List[Command]): Array[Char] = {
    @tailrec
    def loop(inp: Array[Char], commands: List[Command]) : Array[Char] = {
      commands match {
        case Nil => inp
        case Spin(steps) :: tail => loop(spin(inp, steps), tail)
        case Exchange(p1,p2) :: tail => loop(exchange(inp, p1,p2), tail)
        case Partner(c1,c2) :: tail => loop(partner(inp, c1, c2), tail)
      }
    }
    loop(start, commands)
  }

  def solveStep1(input: String, commandStr: String): String = {
    val start = input.toCharArray
    val commands = parse(commandStr)
    solve(start, commands).mkString
  }

  def solveStep2(input: String, commandStr: String) : String = {
    val commands = parse(commandStr)

    @tailrec
    def loop(s: Array[Char], cnt: Int = 0, acc: List[String] = List()) : List[String] = {
      if(acc contains s.mkString) {
        acc
      } else {
        loop(solve(s, commands), cnt + 1, acc :+ s.mkString)
      }
    }
    val cycle = loop(input.toCharArray)
    cycle(1000000000 % cycle.length)
  }
}

trait Solver16 {
  def solve16(): Unit = {
    println("Day 16:")
    val result1 = Solver.solveStep1("abcdefghijklmnop", Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2("abcdefghijklmnop", Input.input)
    println(s"step 2 result is $result2")
  }
}

object Main extends Solver16 {
  def main(args: Array[String]): Unit = {
    solve16()
  }
}
