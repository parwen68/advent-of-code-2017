package se.parwen.adoc

import se.parwen.adoc.day10.Solver10
import se.parwen.adoc.day11.Solver11
import se.parwen.adoc.day12.Solver12
import se.parwen.adoc.day13.Solver13
import se.parwen.adoc.day14.Solver14
import se.parwen.adoc.day3.Solver3
import se.parwen.adoc.day4.Solver4
import se.parwen.adoc.day5.Solver5
import se.parwen.adoc.day6.Solver6
import se.parwen.adoc.day7.Solver7
import se.parwen.adoc.day8.Solver8
import se.parwen.adoc.day9.Solver9

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}

object AdocJS extends Solver3
  with Solver4 with Solver5 with Solver6 with Solver7 with Solver8
  with Solver9 with Solver10 with Solver11 with Solver12 with Solver13
  with Solver14 {

  @JSExportTopLevel("se.parwen.adoc.AdocJS")
  protected def getInstance(): this.type = this

  @JSExport
  def main(): Unit = {
    solve3()
    solve4()
    solve5()
    solve6()
    solve7()
    solve8()
    solve9()
    solve10()
    solve11()
    solve12()
    solve13()
    solve14()
  }
}