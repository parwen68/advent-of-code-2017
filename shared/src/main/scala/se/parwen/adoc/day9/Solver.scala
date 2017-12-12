package se.parwen.adoc.day9

object Solver {
  def solveStep1(input: String): Int = {

    case class Context(ingarbage: Boolean = false, score: Int = 0, started: Int = 0){
      def startGroup() = this.copy(started = started + 1)
      def endGroup() = this.copy(started = started - 1, score = score + started)
    }

    def loop(str: List[Char], ctx: Context = Context()): Context = {
      str match {
        case Nil => ctx
        case '<' :: tail => loop(tail, ctx.copy(ingarbage = true))
        case '>' :: tail => loop(tail, ctx.copy(ingarbage = false))
        case '!' :: _ :: tail if ctx.ingarbage => loop(tail, ctx)
        case '{' :: tail if !ctx.ingarbage => loop(tail, ctx.startGroup())
        case '}' :: tail if !ctx.ingarbage => loop(tail, ctx.endGroup())
        case _ :: tail => loop(tail, ctx)
      }
    }
    loop(input.toList).score
  }

  def solveStep2(input: String): Int = {

    case class Context(ingarbage: Boolean = false, score: Int = 0, started: Int = 0){
      def startGroup() = this.copy(started = started + 1)
      def endGroup() = this.copy(started = started - 1, score = score + started)
    }

    def loop(str: List[Char], count: Int, ctx: Context = Context()): Int = {
      str match {
        case Nil => count
        case '<' :: tail if !ctx.ingarbage => loop(tail, count, ctx.copy(ingarbage = true))
        case '>' :: tail if ctx.ingarbage => loop(tail, count, ctx.copy(ingarbage = false))
        case '!' :: _ :: tail if ctx.ingarbage => loop(tail, count, ctx)
        case '{' :: tail if !ctx.ingarbage => loop(tail, count,ctx.startGroup())
        case '}' :: tail if !ctx.ingarbage => loop(tail, count,ctx.endGroup())
        case _ :: tail if !ctx.ingarbage => loop(tail, count,ctx)
        case _ :: tail if ctx.ingarbage => loop(tail, count+1,ctx)
      }
    }
    loop(input.toList, 0)
  }
}