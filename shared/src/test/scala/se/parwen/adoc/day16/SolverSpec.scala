package se.parwen.adoc.day16

import org.scalatest.{FlatSpec, Matchers}
import se.parwen.adoc.day16.Solver.{Exchange, Partner, Spin}

class SolverSpec extends FlatSpec with Matchers {

  "Solver spin 3" should "return cdeab for abcde" in {
    Solver.spin("abcde".toArray, 3) should be ("cdeab".toArray)
  }
  "Solver exchange 1,3" should "return adcbe for abcde" in {
    Solver.exchange("abcde".toArray, 1, 3) should be ("adcbe".toArray)
  }
  "Solver exchange 3,4" should "return abced for abcde" in {
    Solver.exchange("eabcd".toArray, 3, 4) should be ("eabdc".toArray)
  }
  "Solver partner b,d" should "return adcbe for abcde" in {
    Solver.partner("abcde".toArray, 'b', 'd') should be ("adcbe".toArray)
  }
  "Solver partner e,b" should "return baedc for eabdc" in {
    Solver.partner("eabdc".toArray, 'e', 'b') should be ("baedc".toArray)
  }
  "Solver parse" should "return list for x5/15,s15,x1/3,pn/f" in {
    Solver.parse("x5/15,s15,x1/3,pn/f") should be (List(Exchange(5,15), Spin(15), Exchange(1,3), Partner('n','f')))
  }
}
