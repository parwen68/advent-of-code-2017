package se.parwen.adoc.day20

import org.scalatest._
import se.parwen.adoc.day20.Solver.{Coord, Pixel}

class SolverSpec extends FlatSpec with Matchers {

  "The Solver parser" should "return parsed result" in {
    val a = Solver.parse("""p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
                           |p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>""".stripMargin)

    println(a)
  }

  "Pixels" should "move after tick" in {
    val a = Solver.parse("""p=< 3,0,0>, v=< 2,0,0>, a=<-1,0,0>
                           |p=< 4,0,0>, v=< 0,0,0>, a=<-2,0,0>""".stripMargin)

    val r = a.map(_.tick())
    r should be (Vector(Pixel(0, Coord(4,0,0),Coord(1,0,0),Coord(-1,0,0)), Pixel(1, Coord(2,0,0),Coord(-2,0,0),Coord(-2,0,0))))
  }

}
