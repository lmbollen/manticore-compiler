package manticore.compiler.assembly.levels.unconstrained

import manticore.compiler.UnitFixtureTest
import manticore.compiler.ManticorePasses
import manticore.compiler.assembly.parser.AssemblyParser
import manticore.compiler.assembly.levels.placed.PlacedIR
import manticore.compiler.assembly.levels.placed.interpreter.AtomicInterpreter
import manticore.compiler.AssemblyContext
import manticore.compiler.assembly.levels.placed.lowering.InterruptLoweringTransform
import org.scalatest.matchers.should.Matchers

/** Distributed scheduled stall wave (P1) — compiler heartbeat + deferred-precise
  * interpreter.
  *
  * Verifies that `--stall-wave`:
  *   - injects the countdown heartbeat on the privileged core and the synthetic
  *     STALL interrupt survives the whole pipeline with the reserved 0x7FFF eid;
  *   - makes the interpreter defer termination: the application FINISH is captured
  *     but the run keeps executing through the `stallMargin` quiesce window and
  *     only gates on the STALL interrupt (so the trace is the non-stall trace plus
  *     the extra window vcycles — deferred-precise, matching the RTL);
  *   - still reports a deferred assertion/stop failure as an error;
  *   - is a byte-identical no-op when off.
  */
class UnconstrainedStallWaveTransformTester extends UnitFixtureTest with Matchers {

  import PlacedIR._

  behavior of "stall-wave heartbeat"

  private val testProgram =
    s"""|.prog: .proc p0:
        |     .const one 1 1
        |     .const zero 1 0
        |     .reg cnt 32 .input cnt_curr 0 .output cnt_next
        |     .const max_cnt 32 16
        |     .reg r1 1 .input done_curr 0 .output done_next
        |     .reg e1 1 .input even_curr 1 .output even_next
        |     .reg o1 1 .input odd_curr 0 .output odd_next
        |
        |     SLICE odd_next, cnt_next, 0, 1;
        |     XOR   even_next, odd_next, one;
        |     SEQ done_next, cnt_next, max_cnt;
        |     (0) FINISH done_curr;
        |     (1) PUT cnt_curr, odd_curr;
        |     (2) FLUSH "#ODD#  %32d", odd_curr;
        |     (3) PUT cnt_curr, even_curr;
        |     (4) FLUSH "#EVEN# %32d", even_curr;
        |     ADD cnt_next, cnt_curr, one;
        |
        |""".stripMargin

  private val reference =
    """|#EVEN#          0
       |#ODD#           1
       |#EVEN#          2
       |#ODD#           3
       |#EVEN#          4
       |#ODD#           5
       |#EVEN#          6
       |#ODD#           7
       |#EVEN#          8
       |#ODD#           9
       |#EVEN#         10
       |#ODD#          11
       |#EVEN#         12
       |#ODD#          13
       |#EVEN#         14
       |#ODD#          15
       |""".stripMargin

  private def compileWith(ctx: AssemblyContext, src: String = testProgram): DefProgram = {
    val compiler =
      AssemblyParser andThen
        ManticorePasses.frontend andThen
        ManticorePasses.middleend andThen
        ManticorePasses.backend
    compiler(src)(ctx)
  }

  private def eids(program: DefProgram): Seq[Int] =
    program.processes.flatMap(_.body).collect { case Interrupt(d, _, _, _) => d.eid }

  private def run(program: DefProgram)(implicit ctx: AssemblyContext): (Boolean, Seq[String]) = {
    val serialOut = scala.collection.mutable.ArrayBuffer.empty[String]
    val interp = AtomicInterpreter.instance(
      program = program,
      serial = Some(ln => serialOut += ln)
    )
    val ok = interp.interpretCompletion()
    (ok, serialOut.toSeq)
  }

  it should "reserve exactly one 0x7FFF STALL interrupt that survives the pipeline" in { fixture =>
    implicit val ctx = AssemblyContext(
      dump_all = true,
      dump_dir = Some(fixture.test_dir.toFile),
      stallWave = true,
      stallMargin = 4
    )
    val allEids = eids(compileWith(ctx))
    withClue(s"interrupt eids = ${allEids}: ") {
      allEids.count(_ == InterruptLoweringTransform.StallEid) shouldEqual 1
    }
  }

  it should "defer termination through the quiesce window and gate on the STALL interrupt" in { fixture =>
    implicit val ctx = AssemblyContext(
      dump_all = true,
      dump_dir = Some(fixture.test_dir.toFile),
      stallWave = true,
      stallMargin = 4
    )
    val (ok, lines) = run(compileWith(ctx))
    val refLines = reference.linesIterator.toList

    ok shouldBe true // terminates cleanly via the STALL (FinishTrap)
    // The non-stall trace is an exact prefix...
    lines.take(refLines.size).toList shouldEqual refLines
    // ...and the stallMargin quiesce window keeps the body running, producing more.
    lines.size should be > refLines.size
  }

  it should "still report a deferred assertion failure as an error" in { fixture =>
    val failing =
      s"""|.prog: .proc p0:
          |     .const one 1 1
          |     .const zero 1 0
          |     .reg cnt 8 .input cnt_curr 0 .output cnt_next
          |     .const lim 8 3
          |     .reg r1 1 .input done_curr 0 .output done_next
          |     ADD cnt_next, cnt_curr, one;
          |     SEQ done_next, cnt_next, lim;
          |     (0) FINISH done_curr;
          |     (1) ASSERT zero;
          |""".stripMargin
    implicit val ctx = AssemblyContext(
      dump_all = true,
      dump_dir = Some(fixture.test_dir.toFile),
      stallWave = true,
      stallMargin = 4
    )
    val (ok, _) = run(compileWith(ctx, failing))
    ok shouldBe false // deferred ASSERT failure must still surface
  }

  it should "be a no-op when stallWave is off (no reserved eid)" in { fixture =>
    implicit val ctx = AssemblyContext(
      dump_all = true,
      dump_dir = Some(fixture.test_dir.toFile),
      stallWave = false
    )
    eids(compileWith(ctx)) should not contain InterruptLoweringTransform.StallEid
  }
}
