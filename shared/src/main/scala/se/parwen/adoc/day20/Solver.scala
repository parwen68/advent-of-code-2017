package se.parwen.adoc.day20

object Solver {

  case class Coord(x: Int, y: Int, z: Int) {
    def dist: Int = Math.abs(x) + Math.abs(y) + Math.abs(z)
  }
  object Coord {
    def apply(x: String, y: String, z: String) : Coord = {
      Coord(x.trim.toInt, y.trim.toInt, z.trim.toInt)
    }
  }
  case class Pixel(idx: Int, p: Coord, v: Coord, a: Coord) {
    def tick(): Pixel = {
      val s = this.copy(v = Coord(v.x + a.x, v.y + a.y, v.z + a.z))
      s.copy(p = Coord(p.x + s.v.x, p.y + s.v.y, p.z + s.v.z))
    }
  }

  def parse(input: String): Vector[Pixel] =  {
    val pp = "<([\\s-\\d]+),([\\s-\\d]+),([\\s-\\d]+)>"
    val pattern = s"p=$pp, v=$pp, a=$pp".r

    val lines = input.split('\n')

    lines.zipWithIndex.map{
      case (pattern(px,py,pz,vx,vy,vz,ax,ay,az),idx)=>
        Pixel(idx, Coord(px,py,pz), Coord(vx,vy,vz), Coord(ax, ay, az))
    }.toVector
  }

  def solveStep1(input: String): Int = {
    val pixles = parse(input)

    val minAcc = pixles.map(p => p.a.dist).min

    val minAccPixels = pixles.filter(p => p.a.dist == minAcc)
    if (minAccPixels.length == 1) {
      minAccPixels(0).idx
    } else {
      def loop(p: Vector[Pixel], dists: Vector[Int] = Vector()) : Int = {
        if (dists.nonEmpty && p.zip(dists).map { case (p, d) => p.p.dist - d }.forall(_ > 0)) {
          p.foldLeft(p(0))((p1, p2) => if (p1.p.dist < p2.p.dist) p1 else p2).idx
        } else {
          loop(p.map(p => p.tick()), p.map(p => p.p.dist))
        }
      }
      loop(minAccPixels)
    }
  }

  def solveStep2(input: String): Int = {
    val pixels = parse(input)

    def distance(p1: Pixel, p2: Pixel): Int = {
      Math.abs(p1.p.x - p2.p.x) + Math.abs(p1.p.y - p2.p.y) + Math.abs(p1.p.z - p2.p.z)
    }

    def distances(p: Vector[Pixel]) = {
      for {
        p1 <- p
        p2 <- p if p2 != p1
      } yield distance(p1, p2)
    }

    def loop(p: Vector[Pixel], left: Int, times: Int = 0, dists: Vector[Int] = Vector()) : Int = {
      if (dists.nonEmpty && distances(p).zip(dists).map{ case (d1, d2) => d1 - d2}.forall(_ > 0))
        left
      else {
        val collisions = p.map(_.p).groupBy(e => e).filter(e => e._2.length > 1)
        loop(
          p.filterNot(v => collisions.keys.toVector.contains(v.p)).map(_.tick()),
          p.length,
          if (p.length == left) times + 1 else 0,
          if (times > 1000) distances(p) else Vector()
        )
      }
    }
    loop(pixels, pixels.length)
  }
}

trait Solver20 {
  def solve20(): Unit = {
    println("Day 20:")
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}

object Main extends Solver20 {
  def main(args: Array[String]): Unit = {
    solve20()
  }
}