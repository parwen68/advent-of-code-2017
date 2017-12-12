package se.parwen.adoc.day8

object Solver {

  val pattern = raw"(\w+) (\w+) ([-\d]+) if (\w+ [!><=]+ [-\d]+)".r
  val patternCond = raw"(\w+) ([!><=]+) ([-\d]+)".r


  case class Exp(reg: String, cmd: String, amount: Int, cond: String)

  def parse(input: String) = {
    val rows = input.split('\n').toList

    for (row <- rows) yield
      row match {
        case pattern(reg, cmd, amount, cond) => Exp(reg, cmd, amount.toInt, cond)
      }
  }

  def eval(cond: String, ctx: Map[String, Int]): Boolean = {
    cond match {
      case patternCond(reg, ">", v) => ctx.getOrElse(reg, 0) > v.toInt
      case patternCond(reg, "<", v) => ctx.getOrElse(reg, 0) < v.toInt
      case patternCond(reg, ">=", v) => ctx.getOrElse(reg, 0) >= v.toInt
      case patternCond(reg, "<=", v) => ctx.getOrElse(reg, 0) <= v.toInt
      case patternCond(reg, "==", v) => ctx.getOrElse(reg, 0) == v.toInt
      case patternCond(reg, "!=", v) => ctx.getOrElse(reg, 0) != v.toInt
    }
  }

  def add(reg: String, v: Int, ctx: Map[String, Int]) = {
    ctx.updated(reg, ctx.getOrElse(reg, 0) + v)
  }

  def sub(reg: String, v: Int, ctx: Map[String, Int]) = {
    ctx.updated(reg, ctx.getOrElse(reg, 0) - v)
  }

  def solveStep1(input: String): (String, Int) = {
    val exps = parse(input)

    def loop(exps: List[Exp], ctx: Map[String, Int] = Map()): Map[String, Int] = exps match {
      case Nil => ctx
      case Exp(reg, cmd, amount, cond) :: tail =>
        val newCtx = if (eval(cond, ctx)) if (cmd == "inc") add(reg, amount, ctx) else sub(reg, amount, ctx) else ctx
        loop(tail, newCtx)
    }
    loop(exps).max((x: (String, Int), y: (String, Int)) => x._2.compareTo(y._2))
  }

  def solveStep2(input: String): Int = {
    val exps = parse(input)
    def loop(exps: List[Exp], ctx: Map[String, Int] = Map(), mx: Int = Int.MinValue): (Map[String, Int], Int) = exps match {
      case Nil => (ctx, mx)
      case Exp(reg, cmd, amount, cond) :: tail =>
        val newCtx = if (eval(cond, ctx)) if (cmd == "inc") add(reg, amount, ctx) else sub(reg, amount, ctx) else ctx
        val newMax = if (newCtx.values.nonEmpty && newCtx.values.max > mx) newCtx.values.max else mx
        loop(tail, newCtx, newMax)
    }
    loop(exps)._2
  }

}