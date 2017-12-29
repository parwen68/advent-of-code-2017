package se.parwen.adoc.day19

object Solver {

  sealed trait Direction
  trait HDirection extends Direction
  trait VDirection extends Direction
  case object Up extends VDirection
  case object Down extends VDirection
  case object Left extends HDirection
  case object Right extends HDirection

  case class Path(arr: Array[Array[Char]]) {
    def apply(x: Int, y: Int): Char = arr(y)(x)
    def start: (Int, Int) = (arr(0).indexOf('|'),0)
  }

  def parse(input: String): Path = {
    val lines = input.split('\n')
    val maxLineLength = lines.map(_.length).max
    Path(lines.map(s => s.padTo(maxLineLength, ' ').toCharArray))
  }

  def solve(input: String): (String, Int) = {
    val path = parse(input)

    def contLeft(x: Int,y: Int) = path(x-1,y) != ' '
    def contRight(x: Int,y: Int) = path(x+1,y) != ' '
    def contUp(x: Int,y: Int) = path(x,y-1) != ' '
    def contDown(x: Int,y: Int) = path(x,y+1) != ' '

    def loop(x: Int, y: Int, d: Direction, acc: List[Char] = List(), cnt: Int = 0) : (String, Int) = {
      (path(x,y), d) match {
        case (' ', _) => (acc.filter(c => c.isLetter).mkString, cnt)
        case ('+', d : VDirection) if contLeft(x,y) => loop(x-1,y,Left, acc, cnt+1)
        case ('+', d : VDirection) if contRight(x,y) => loop(x+1,y,Right, acc, cnt+1)
        case ('+', d : HDirection) if contUp(x,y) => loop(x,y-1,Up, acc, cnt+1)
        case ('+', d : HDirection) if contDown(x,y) => loop(x,y+1,Down, acc, cnt+1)
        case (c ,Down) => loop(x, y + 1, Down, acc :+ c, cnt+1)
        case (c, Up) => loop(x, y - 1, Up, acc :+ c, cnt+1)
        case (c, Left) => loop(x - 1, y, Left, acc :+ c, cnt+1)
        case (c, Right) => loop(x + 1, y, Right, acc :+ c, cnt+1)
      }
    }
    val (x,y) = path.start
    loop(x,y,Down)
  }

  def solveStep1(input: String): String = {
    solve(input)._1
  }

  def solveStep2(input: String): Int = {
    solve(input)._2
  }
}

trait Solver19 {
  def solve19(): Unit = {
    println("Day 19:")
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}

object Main extends Solver19 {
  def main(args: Array[String]): Unit = {
    solve19()
  }
}