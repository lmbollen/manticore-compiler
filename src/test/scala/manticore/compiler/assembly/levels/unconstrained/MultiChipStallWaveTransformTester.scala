package manticore.compiler.assembly.levels.unconstrained

import manticore.compiler.UnitFixtureTest
import manticore.compiler.ManticorePasses
import manticore.compiler.assembly.parser.AssemblyParser
import manticore.compiler.assembly.levels.placed.PlacedIR
import manticore.compiler.assembly.levels.placed.interpreter.AtomicInterpreter
import manticore.compiler.AssemblyContext
import manticore.compiler.DefaultHardwareConfig
import manticore.compiler.assembly.levels.placed.lowering.InterruptLoweringTransform
import manticore.compiler.assembly.levels.codegen.CodeDump
import org.scalatest.matchers.should.Matchers

/** Distributed scheduled stall wave — MULTI-IC (per-chip privileged tiles).
  *
  * Increment 1 (foundation): verify that more than one privileged process can
  * coexist (one per chip), each carrying its own countdown heartbeat injected by
  * [[UnconstrainedStallWaveTransform]], and that the program compiles through the
  * full backend (the analytical placer no longer asserts a single privileged
  * process) and interprets to a clean, deferred-precise STALL.
  *
  * Each "chip" here is a single core counting independently to the same N then
  * FINISH; both chips' countdowns therefore zero-cross the same vcycle, so the run
  * terminates cleanly on the synchronized STALL.
  */
class MultiChipStallWaveTransformTester extends UnitFixtureTest with Matchers {

  import PlacedIR._

  behavior of "multi-chip stall-wave (per-chip privileged tiles)"

  // Two privileged tiles, explicitly placed on two different cores (use_loc), each
  // counting to 16 then FINISH. No inter-tile communication yet (Increment 1).
  private def tile(x: Int, y: Int): String =
    s"""|@LOC[x = $x, y = $y]
        |.proc p_${x}_${y}:
        |     .const one_${x}_${y} 1 1
        |     .const max_${x}_${y} 32 16
        |     .reg cnt_${x}_${y} 32 .input cnt_curr_${x}_${y} 0 .output cnt_next_${x}_${y}
        |     .reg done_${x}_${y} 1 .input done_curr_${x}_${y} 0 .output done_next_${x}_${y}
        |     ADD cnt_next_${x}_${y}, cnt_curr_${x}_${y}, one_${x}_${y};
        |     SEQ done_next_${x}_${y}, cnt_next_${x}_${y}, max_${x}_${y};
        |     (0) FINISH done_curr_${x}_${y};
        |""".stripMargin

  private val twoChipProgram =
    s""".prog:
       |${tile(0, 0)}
       |${tile(1, 0)}
       |""".stripMargin

  private def compileWith(ctx: AssemblyContext, src: String): DefProgram = {
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

  private def ctxFor(fixture: FixtureParam): AssemblyContext =
    AssemblyContext(
      dump_all = true,
      dump_dir = Some(fixture.test_dir.toFile),
      use_loc = true,
      hw_config = DefaultHardwareConfig(dimX = 2, dimY = 1),
      stallWave = true,
      stallMargin = 4,
      // single combined image (chipDimX = 0): each tile's own FINISH seeds its own
      // countdown via the unconstrained transform; deferred by stallMargin -> 16 + 4.
      expected_cycles = Some(20),
      max_cycles = 1000
    )

  it should "place two privileged tiles and inject one countdown per tile" in { fixture =>
    implicit val ctx = ctxFor(fixture)
    val allEids = eids(compileWith(ctx, twoChipProgram))
    withClue(s"interrupt eids = ${allEids}: ") {
      // one reserved STALL interrupt per privileged tile
      allEids.count(_ == InterruptLoweringTransform.StallEid) shouldEqual 2
    }
  }

  it should "interpret to a clean deferred-precise STALL across both tiles" in { fixture =>
    implicit val ctx = ctxFor(fixture)
    val (ok, _) = run(compileWith(ctx, twoChipProgram))
    ok shouldBe true // terminates cleanly via the synchronized STALL
  }

  // ---- Increment 2: cross-chip min-merge ------------------------------------------
  // Only chip A (the reporter, x=0) raises a FINISH; chip B (x=1) is compute-only and
  // has NO terminating interrupt. The post-placement HeartbeatTileInjectionTransform
  // must make chip B a tile whose countdown is seeded ONLY by the word received from
  // chip A, so both chips gate on the same vcycle.

  private val reporterPlusComputeProgram =
    s""".prog:
       |@LOC[x = 0, y = 0]
       |.proc p_0_0:
       |     .const one_a 1 1
       |     .const max_a 32 16
       |     .reg cnt_a 32 .input cnt_curr_a 0 .output cnt_next_a
       |     .reg done_a 1 .input done_curr_a 0 .output done_next_a
       |     ADD cnt_next_a, cnt_curr_a, one_a;
       |     SEQ done_next_a, cnt_next_a, max_a;
       |     (0) FINISH done_curr_a;
       |
       |@LOC[x = 1, y = 0]
       |.proc p_1_0:
       |     .const one_b 1 1
       |     .reg cnt_b 32 .input cnt_curr_b 0 .output cnt_next_b
       |     ADD cnt_next_b, cnt_curr_b, one_b;
       |""".stripMargin

  private def multiChipCtx(fixture: FixtureParam): AssemblyContext =
    AssemblyContext(
      dump_all = true,
      dump_dir = Some(fixture.test_dir.toFile),
      use_loc = true,
      hw_config = DefaultHardwareConfig(dimX = 2, dimY = 1),
      chipDimX = 1, // two 1x1 chips -> post-placement heartbeat transform engages
      stallWave = true,
      stallMargin = 4,
      // cross-chip: reporter exception at vcycle 16, seed = stallMargin + diameter(1)
      // = 5, so every chip gates on vcycle 16 + 5 = 21 (deferred-precise, synchronized).
      expected_cycles = Some(21),
      max_cycles = 1000
    )

  it should "create a tile + STALL on the compute-only chip via the post-placement transform" in { fixture =>
    implicit val ctx = multiChipCtx(fixture)
    val allEids = eids(compileWith(ctx, reporterPlusComputeProgram))
    withClue(s"interrupt eids = ${allEids}: ") {
      // one reserved STALL interrupt per chip (reporter chip + compute chip)
      allEids.count(_ == InterruptLoweringTransform.StallEid) shouldEqual 2
    }
  }

  it should "propagate the stall from the reporter chip to the compute-only chip" in { fixture =>
    implicit val ctx = multiChipCtx(fixture)
    val (ok, _) = run(compileWith(ctx, reporterPlusComputeProgram))
    // The compute chip has no local exception; it can only terminate if chip A's
    // countdown reached it over the NoC min-merge. A clean finish proves propagation.
    ok shouldBe true
  }

  it should "emit a per-chip manifest with each chip's own STALL exception (CodeDump)" in { fixture =>
    implicit val ctx = AssemblyContext(
      output_dir = Some(fixture.test_dir.toFile),
      dump_all = false,
      use_loc = true,
      hw_config = DefaultHardwareConfig(dimX = 2, dimY = 1),
      chipDimX = 1,
      stallWave = true,
      stallMargin = 4
    )
    val compiled = compileWith(ctx, reporterPlusComputeProgram)
    CodeDump(compiled) // must not fail on the two per-chip privileged tiles
    val manifest =
      new String(java.nio.file.Files.readAllBytes(fixture.test_dir.resolve("manifest.json")))
    withClue(s"manifest:\n${manifest}\n") {
      manifest should include("\"chips\"")
      // each of the two chips carries its own reserved STALL eid (0x7FFF = 32767)
      InterruptLoweringTransform.StallEid shouldEqual 0x7FFF
      "\"eid\": 32767".r.findAllIn(manifest).length shouldEqual 2
    }
  }
}
