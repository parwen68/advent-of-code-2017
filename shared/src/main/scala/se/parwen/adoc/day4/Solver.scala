package se.parwen.adoc.day4

object Solver {

  def isSame(a: Array[String]): Boolean = {
    val b = a.foldLeft(Map[String,Int]())((m,v) => m.updated(v, m.getOrElse(v,0) + 1))
    b.values.max == 1
  }

  def isAnagram(a: Array[String]): Boolean = {
    for (w1 <- a) {
      for (w2 <- a) {
        if (w1 != w2 && w1.sorted == w2.sorted) {
          return false
        }
      }
    }
    true
  }

  def solveStep1(input: String): Int = {
    def isValid(str: String) = {
      isSame(str.split("\\s+"))
    }
    input.split('\n').filter(s => !s.isEmpty).map(pp => (pp, isValid(pp))).count(p => p._2)
  }

  def solveStep2(input: String): Int = {
    def isValid(str: String) = {
      val a = str.split("\\s+")
      isSame(a) && isAnagram(a)
    }
    input.split('\n').filter(s => !s.isEmpty).map(pp => (pp, isValid(pp))).count(p => p._2)
  }
}
