package se.parwen.adoc.day18

import scala.annotation.tailrec
import scala.util.matching.Regex

object Solver {

  case class ExecCtx(
    ptr: Long,
    regs: Map[Char, Long],
    lastPlayedSnd: Long,
    inQ: List[Long] = List(),
    outQ: List[Long] = List(),
    sends: Int = 0,
    stopped: Boolean = false
  )

  sealed trait Instruction {
    def exec(ctx: ExecCtx) : ExecCtx
  }
  case class Sndi(v: Long) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx =
      ctx.copy(
        ptr = ctx.ptr + 1,
        lastPlayedSnd = v,
        outQ = ctx.outQ :+ v,
        sends = ctx.sends + 1)
  }

  case class Sndc(r: Char) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx =
      ctx.copy(
        ptr = ctx.ptr + 1,
        lastPlayedSnd = ctx.regs(r),
        outQ = ctx.outQ :+ ctx.regs(r),
        sends = ctx.sends + 1)
  }

  case class Rcv(r: Char) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx = {
      ctx.inQ match {
        case head :: tail => ctx.copy(
          ptr = ctx.ptr + 1,
          regs = ctx.regs.updated(r, head),
          inQ = tail)
        case Nil => ctx.copy(stopped = true)
      }
    }
  }

  case class Seti(r: Char, v: Long) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx =
      ctx.copy(ptr = ctx.ptr + 1, regs = ctx.regs.updated(r, v))
  }
  case class Setc(r1: Char, r2: Char) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx =
      ctx.copy(ptr = ctx.ptr + 1, regs = ctx.regs.updated(r1, ctx.regs(r2)))
  }
  case class Addi(r: Char, v: Long) extends Instruction {
    override def exec(ctx: ExecCtx):
    ExecCtx = ctx.copy(ptr = ctx.ptr + 1, regs = ctx.regs.updated(r, ctx.regs(r) + v))
  }
  case class Addc(r1: Char, r2: Char) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx = ctx.copy(ptr = ctx.ptr + 1, regs = ctx.regs.updated(r1, ctx.regs(r1) + ctx.regs(r2)))
  }
  case class Muli(r: Char, v: Long) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx = ctx.copy(ptr = ctx.ptr + 1, regs = ctx.regs.updated(r, ctx.regs(r) * v))
  }
  case class Mulc(r1: Char, r2: Char) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx = ctx.copy(ptr = ctx.ptr + 1, regs = ctx.regs.updated(r1, ctx.regs(r1) * ctx.regs(r2)))
  }
  case class Modi(r: Char, v: Long) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx = ctx.copy(ptr = ctx.ptr + 1, regs = ctx.regs.updated(r, ctx.regs(r) % v))
  }
  case class Modc(r1: Char, r2: Char) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx = ctx.copy(ptr = ctx.ptr + 1, regs = ctx.regs.updated(r1, ctx.regs(r1) % ctx.regs(r2)))
  }
  case class Jgz(v1: Long, v2: Long) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx = if(v1 > 0) ctx.copy(ptr = ctx.ptr + v2) else ctx.copy(ptr = ctx.ptr + 1)
  }
  case class Jgzi(r: Char, v: Long) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx = if(ctx.regs(r) > 0) ctx.copy(ptr = ctx.ptr + v) else ctx.copy(ptr = ctx.ptr + 1)
  }
  case class Jgzc(r1: Char, r2: Char) extends Instruction {
    override def exec(ctx: ExecCtx): ExecCtx = if(ctx.regs(r1) > 0) ctx.copy(ptr = ctx.ptr + ctx.regs(r2)) else ctx.copy(ptr = ctx.ptr + 1)
  }

  val sndi: Regex = "snd ([-\\d]+)".r
  val sndc: Regex = "snd (\\w)".r
  val rcv: Regex = "rcv (\\w)".r

  val seti: Regex = "set (\\w) ([-\\d]+)".r
  val setc: Regex = "set (\\w) (\\w)".r
  val addi: Regex = "add (\\w) ([-\\d]+)".r
  val addc: Regex = "add (\\w) (\\w)".r
  val muli: Regex = "mul (\\w) ([-\\d]+)".r
  val mulc: Regex = "mul (\\w) (\\w)".r
  val modi: Regex = "mod (\\w) ([-\\d]+)".r
  val modc: Regex = "mod (\\w) (\\w)".r
  val jgz: Regex = "jgz ([-\\d]+) ([-\\d]+)".r
  val jgzi: Regex = "jgz (\\w) ([-\\d]+)".r
  val jgzc: Regex = "jgz (\\w) (\\w)".r

  def parse(input: String) : Vector[Instruction] = {
    input.split('\n').map {
      case sndi(v) => Sndi(v.toInt)
      case sndc(r) => Sndc(r(0))
      case seti(r, v) => Seti(r(0), v.toInt)
      case setc(r1, r2) => Setc(r1(0), r2(0))
      case addi(r, v) => Addi(r(0), v.toInt)
      case addc(r1, r2) => Addc(r1(0), r2(0))
      case muli(r, v) => Muli(r(0), v.toInt)
      case mulc(r1, r2) => Mulc(r1(0), r2(0))
      case modi(r, v) => Modi(r(0), v.toInt)
      case modc(r1, r2) => Modc(r1(0), r2(0))
      case rcv(r) => Rcv(r(0))
      case jgz(v1, v2) => Jgz(v1.toInt, v2.toInt)
      case jgzi(r, v) => Jgzi(r(0), v.toInt)
      case jgzc(r1, r2) => Jgzc(r1(0), r2(0))
      case e => throw new RuntimeException(e)
    }.toVector
  }

  def solveStep1(input: String): Long = {
    val prgm = parse(input)

    @tailrec
    def loop(ctx: ExecCtx): Long = {
      prgm(ctx.ptr.toInt) match {
        case Rcv(r) if ctx.regs(r) != 0 => ctx.lastPlayedSnd
        case Rcv(r) if ctx.regs(r) == 0 => loop(prgm(ctx.ptr.toInt + 1).exec(ctx))
        case _ => loop(prgm(ctx.ptr.toInt).exec(ctx))
      }
    }
    loop(ExecCtx(0,('a' to 'z').map((_,0L)).toMap, 0))
  }

  def solveStep2(input: String): Long = {
    val prgm = parse(input)

    def stopped(ctx: ExecCtx) = ctx.stopped || ctx.ptr < 0 || ctx.ptr >= prgm.length

    @tailrec
    def loop(ctx0: ExecCtx, ctx1: ExecCtx): Int = {
      if (stopped(ctx0) && stopped(ctx1)) ctx1.sends
      else
        loop(
          prgm(ctx0.ptr.toInt).exec(ctx0.copy(inQ = ctx0.inQ ++ ctx1.outQ, outQ = List())),
          prgm(ctx1.ptr.toInt).exec(ctx1.copy(inQ = ctx1.inQ ++ ctx0.outQ, outQ = List()))
        )
    }

    val ctx0 = ExecCtx(0, ('a' to 'z').map((_,0L)).toMap.updated('p', 0L), 0)
    val ctx1 = ExecCtx(0, ('a' to 'z').map((_,0L)).toMap.updated('p', 1L), 0)

    loop(ctx0, ctx1)
  }

}

trait Solver18 {
  def solve18(): Unit = {
    println("Day 18:")
    val result1 = Solver.solveStep1(Input.input)
    println(s"step 1 result is $result1")
    val result2 = Solver.solveStep2(Input.input)
    println(s"step 2 result is $result2")
  }
}

object Main extends Solver18 {
  def main(args: Array[String]): Unit = {
    solve18()
  }
}