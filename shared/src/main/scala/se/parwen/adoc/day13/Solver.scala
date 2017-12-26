package se.parwen.adoc.day13

import scala.annotation.tailrec

object Solver {

  case class Layer(pos: Int, depth: Int) {
    val stream: List[Int] = (0 until depth-1).toList ++ (depth-1 until 0 by -1).toList
    def getForPicoSec(pico: Int): Int = {
      stream(pico % stream.length)
    }
  }

  object Layer {
    def parse(s: String): Layer = {
      val r = s.split(':')
      Layer(r(0).trim.toInt, r(1).trim.toInt)
    }
  }

  def parse(input: String): List[Layer] = {
    input.split('\n').map(Layer.parse).toList
  }

  case class Pico(layers: List[Layer]) {
    def getForPicoSec(pico: Int): Map[Int, (Int, Int)] = {
      layers.map(layer => (layer.pos, (layer.depth, layer.getForPicoSec(pico)))).toMap
    }
    def getMaxLayer: Int = layers.map(_.pos).max
  }

  def solveStep1(input: String): Int = {
    val picos = Pico(parse(input))

    @tailrec
    def loop(pico: Int, pos: Int, acc: Int): Int = {
      if (pos > picos.getMaxLayer)
        acc
      else {
        val state = picos.getForPicoSec(pico)
        val a = state.get(pos).exists(_._2 == 0)
        loop(pico+1, pos+1, acc + (if (a) state.get(pos).map(_._1 * pos).getOrElse(0) else 0))
      }
    }
    loop(0, 0, 0)
  }

  def solveStep2(input: String): Int = {
    val picos = Pico(parse(input))

    @tailrec
    def outerLoop(delay: Int): Int = {
      @tailrec
      def loop(pico: Int, pos: Int): Boolean = {
        if (pos > picos.getMaxLayer)
          true
        else {
          val state = picos.getForPicoSec(pico + delay)
          if (state.get(pos).exists(_._2 == 0)) false else loop(pico + 1, pos + 1)
        }
      }
      if(loop(0,0)) delay else outerLoop(delay + 1)
    }
    outerLoop(0)
  }
}

trait Solver13 {
  def solve13(): Unit = {
    println("Day 13:")
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    println(s"step 2 might take some time...")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}

object Main extends Solver13 {
  def main(args: Array[String]): Unit = {
    solve13()
  }
}