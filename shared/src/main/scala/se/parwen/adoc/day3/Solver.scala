package se.parwen.adoc.day3

object Solver {

  val directions: Stream[String] = "R" #:: "U" #:: "L" #:: "D" #:: directions

  val multipliers: Stream[Int] = Stream.from(1).flatMap(i => Stream(i, i))

  def moves: Stream[Char] = multipliers.zip(directions).flatMap { case (multiplier, direction) => direction * multiplier }

  case class Pos(x:Int, y:Int) {
    def left: Pos  = copy(x = x - 1)
    def up: Pos    = copy(y = y + 1)
    def right: Pos = copy(x = x + 1)
    def down: Pos  = copy(y = y - 1)
  }

  def nextPos(pos: Pos, c: Char): Pos = c match {
    case 'L' => pos.left
    case 'R' => pos.right
    case 'U' => pos.up
    case 'D' => pos.down
  }

  def solveStep1(target: Int): Int = {
    import java.lang.Math.abs

    val endPos = (moves take target - 1).toList.foldLeft(Pos(0, 0)){case (pos, c) => nextPos(pos, c) }

    abs(endPos.x) + abs(endPos.y)
  }
  
  def solveStep2(target: Int): Int = {

    def sum(pos: Pos, m: Map[Pos, Int]): Int = {
      List(pos.left, pos.right, pos.up, pos.down, pos.left.up, pos.right.up, pos.left.down, pos.right.down)
        .map(m.getOrElse(_, 0)).sum
    }

    def loop(map: Map[Pos, Int], p: Pos, v: Int, m: Stream[Char]) : Int = m match {
      case c #:: tail if v < target =>
        val np = nextPos(p, c)
        val s = sum(np, map)
        loop(map ++ Map{np -> s}, np, s, tail)
      case _ => v
    }
    loop(Map{Pos(0,0) -> 1}, Pos(0,0), 1, moves)
  }
}
