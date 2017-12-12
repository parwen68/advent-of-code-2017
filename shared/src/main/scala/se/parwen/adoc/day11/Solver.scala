package se.parwen.adoc.day11

/*
 s + ne = se
 s + nw = sw
 n + se = ne
 n + sw = nw

---

 se + sw = s
 ne + nw = n

---

 s + n = 0
 sw + ne = 0
 se + nw = 0

---

 s,sw,se = s, s
 s,sw,nw = sw, sw
 s,ne,se = se, se
 s,ne,nw = s, n
 n,sw,se = n, s
 n,sw,nw = nw, nw
 n,ne,se = ne, ne
 n,ne,nw = n, n

*/

object Solver {

  def reduce1(g: Map[String,Int]): Map[String, Int] = {
    val ns = g("n") - g("s")
    val swne = g("sw") - g("ne")
    val senw = g("se") - g("nw")

    (
      (if (ns > 0) List(("n", ns), ("s", 0)) else List(("n", 0), ("s", -ns))) ++
      (if (swne > 0) List(("sw", swne), ("ne", 0)) else List(("sw", 0), ("ne", -swne))) ++
      (if (senw > 0) List(("se", senw), ("nw", 0)) else List(("se", 0), ("nw", -senw)))
    ).toMap
  }

  def p(v: Int) = if (v > 0) v else 0

  def reduce2(g: List[(String, Int)]): List[(String, Int)] = {
    import Math._
    g match {
      case List(("n",0), ("ne",0), ("nw",0), ("s",a), ("se",c), ("sw",b)) => List(("n",0), ("ne",0), ("nw",0), ("s",a + min(c,b)), ("se",p(c - b)), ("sw",p(b - c)))
      case List(("n",0), ("ne",0), ("nw",c), ("s",a), ("se",0), ("sw",b)) => List(("n",0), ("ne",0), ("nw",p(c - a)), ("s",p(a - c)), ("se",0), ("sw",b + min(a,c)))
      case List(("n",0), ("ne",b), ("nw",0), ("s",a), ("se",c), ("sw",0)) => List(("n",0), ("ne",p(b-a)), ("nw",0), ("s",p(a-b)), ("se",c + min(a, b)), ("sw",0))
      case List(("n",0), ("ne",b), ("nw",c), ("s",a), ("se",0), ("sw",0)) => List(("n",p(abs(b-c)-a)), ("ne",p(b-c)), ("nw",p(c-b)), ("s",p(a-abs(b-c))), ("se",0), ("sw",0))
      case List(("n",a), ("ne",0), ("nw",0), ("s",0), ("se",c), ("sw",b)) => List(("n",p(a-abs(b-c))), ("ne",0), ("nw",0), ("s",p(abs(b-c)-a)), ("se",p(c-b)), ("sw",p(b-c)))
      case List(("n",a), ("ne",0), ("nw",c), ("s",0), ("se",0), ("sw",b)) => List(("n",p(a-b)), ("ne",0), ("nw",c + min(a,b)), ("s",0), ("se",0), ("sw",p(b-a)))
      case List(("n",a), ("ne",b), ("nw",0), ("s",0), ("se",c), ("sw",0)) => List(("n",p(a-c)), ("ne",b + min(a,c)), ("nw",0), ("s",0), ("se",p(c-a)), ("sw",0))
      case List(("n",a), ("ne",b), ("nw",c), ("s",0), ("se",0), ("sw",0)) => List(("n",a + min(b,c)), ("ne",p(b-c)), ("nw",p(c-b)), ("s",0), ("se",0), ("sw",0))
    }
  }

  def solve(input: Array[String]): List[(String, Int)] = {
    val map1 = List("s", "n", "nw", "ne", "sw", "se").map(v => (v, 0)).toMap
    val map2 = input.toList.groupBy(identity).mapValues(_.size)
    val m = map1.map{ case (k,v) => k -> (v + map2.getOrElse(k,0)) }

    reduce2(reduce1(m).toList.sortWith(_._1 < _._1))
  }

  def solveStep1(input: String): Int = {
    solve(input.split(',')).map(_._2).sum
  }

  def solveStep2(input: String): Int = {
    val list: Array[String] = input.split(',')
    val len = list.length

    (1 to len).map(list.take(_)).foldLeft(0)((acc,v) => Math.max(acc,solve(v).map(_._2).sum))
  }
}

trait Solver11 {
  def solve11(): Unit = {
    println("Day 11:")
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}