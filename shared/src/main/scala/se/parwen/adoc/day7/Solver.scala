package se.parwen.adoc.day7

import se.parwen.adoc.day7.Solver.{solveStep1, solveStep2}

import scala.annotation.tailrec

object Solver {

  val pattern = raw"(\w+) \((\d+)\)[-> ]*([\w, ]*)".r

  case class Program(name: String, weight: Int = -1, ontopof: Option[Program] = None)

  def parse(str: String): (List[Program], Map[String, Program]) = {
    val rows = str.split('\n')

    val p = for(row <- rows) yield
      row match {
        case pattern(name, weight, "") => (Program(name, weight.toInt), Map[String, Program]())
        case pattern(name, weight, other) =>
          val p = Program(name, weight.toInt)
          val m = other.split(',').map(s => s.trim).zip(Stream.continually(p)).toMap
          (p, m)
      }
    p.foldLeft((List[Program](), Map[String, Program]())){ case ((programs, map), (pp,m)) => (programs :+ pp, map ++ m)}
  }

  def connect(programs: List[Program], map: Map[String, Program]): List[Program] = {
    def loop(p: List[Program], acc: List[Program] = Nil): List[Program] = p match {
      case Nil => acc
      case head :: tail => loop(tail, acc :+ head.copy(ontopof = map.get(head.name)))
    }
    loop(programs)
  }

  def solveStep1(input: String): List[String] = {
    (connect _ tupled parse(input)).filter(p => p.ontopof.isEmpty).map(_.name)
  }

  def solveStep2(input: String): Int = {
    val connected = connect _ tupled parse(input)

    case class P(name: String, weight: Int, ontop: List[P] = Nil) {
      def weights: List[Int] =  {
        if (ontop.isEmpty) List(weight)
        else weight :: ontop.flatMap(_.weights)
      }
      def weightsSum: Int = weights.sum
    }

    val bottom = connected.filter(_.ontopof.isEmpty).map(p => P(p.name, p.weight)).head

    def loop(c: P, connected: List[Program]) : P = {
      val ontop = connected.filter(_.ontopof.exists(e => e.name == c.name)).map(p => P(p.name, p.weight))
      if (ontop.isEmpty) c
      else c.copy(ontop = ontop.map(t => loop(t, connected)))
    }
    val b2 = loop(bottom, connected)

    //println(b2.ontop.map(_.weightsSum))
    //println(b2.ontop(1).ontop.map(_.weightsSum))
    //println(b2.ontop(1).ontop.head.ontop.map(_.weightsSum))
    //println(b2.ontop(1).ontop.head.ontop(2).ontop.map(_.weightsSum))
    //println(b2.ontop(1).ontop.head.ontop(2).weight)
    //println(b2.ontop(1).ontop.head.ontop(2).weight - 9)

    b2.ontop(1).ontop.head.ontop(2).weight - 9
  }
}

trait Solver7 {
  def solve7(): Unit = {
    println("Day 7:")
    val result1 = solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}