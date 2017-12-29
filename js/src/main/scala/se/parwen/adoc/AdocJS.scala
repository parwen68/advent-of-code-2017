package se.parwen.adoc

import se.parwen.adoc.day10.Solver10
import se.parwen.adoc.day11.Solver11
import se.parwen.adoc.day12.Solver12
import se.parwen.adoc.day13.Solver13
import se.parwen.adoc.day14.Solver14
import se.parwen.adoc.day15.Solver15
import se.parwen.adoc.day16.Solver16
import se.parwen.adoc.day17.Solver17
import se.parwen.adoc.day18.Solver18
import se.parwen.adoc.day19.Solver19
import se.parwen.adoc.day20.Solver20
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
  with Solver14 with Solver15 with Solver16 with Solver17 with Solver18
  with Solver19 with Solver20 {

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
    solve15()
    solve16()
    solve17()
    solve18()
    solve19()
    solve20()
  }
}