package se.parwen.adoc.day13

import org.scalatest.{FlatSpec, Matchers}
import Solver._

class SolverSpec extends FlatSpec with Matchers {

  "The layer" should "return 0,1,2,3,2,1,0,1,2,3 in 0,1,2,3,4,5,6,7,8,9" in {
    val layer = Layer.parse("0 : 4")
    List(0,1,2,3,4,5,6,7,8,9).map(layer.getForPicoSec) should be (List(0, 1, 2, 3, 2, 1, 0, 1, 2, 3))
  }

  "The layer" should "return 3 in pico 3" in {
    val layer = Layer.parse("0 : 6")
    layer.getForPicoSec(3) should be (3)
  }
  // 0,1,2,3,4,5,4,3,2,1,0,1,2,3,4,5
  "The layer" should "return 2 in pico 15" in {
    val layer = Layer.parse("0 : 6")
    layer.getForPicoSec(15) should be (5)
  }

  "Layers in pico 0" should "0,0,0,0" in {
    val r = Pico(parse("""0: 3
                         |1: 2
                         |4: 4
                         |6: 4""".stripMargin)).getForPicoSec(0)
    r should be (Map(0 -> (3,0), 1 -> (2,0), 4 -> (4,0), 6 -> (4,0)))
  }

  "Layers in pico 3" should "1,1,3,3" in {
    val r = Pico(parse("""0: 3
                         |1: 2
                         |4: 4
                         |6: 4""".stripMargin)).getForPicoSec(3)
    r should be (Map(0 -> (3,1), 1 -> (2,1), 4 -> (4,3), 6 -> (4,3)))
  }

  "The solver parse" should "return list" in {
   val r =  parse("""0: 3
                    |1: 2
                    |4: 4
                    |6: 4""".stripMargin)

    r should be (Layer(0,3) :: Layer(1,2) :: Layer(4,4) :: Layer(6,4) :: Nil)
  }

  "The solver solveStep1" should "return 24" in {
    val r =  solveStep1("""0: 3
                          |1: 2
                          |4: 4
                          |6: 4""".stripMargin)

    r should be (24)
  }

  "The solver solveStep2" should "return 10" in {
    val r =  solveStep2("""0: 3
                     |1: 2
                     |4: 4
                     |6: 4""".stripMargin)

    r should be (10)
  }
}
