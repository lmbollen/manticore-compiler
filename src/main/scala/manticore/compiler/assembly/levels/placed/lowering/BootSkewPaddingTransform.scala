package manticore.compiler.assembly.levels.placed.lowering

import manticore.compiler.AssemblyContext
import manticore.compiler.assembly.levels.placed.PlacedIR
import manticore.compiler.assembly.levels.placed.PlacedIRTransformer

/** Boot-skew compensation for non-uniform link latencies (multi-chip seams).
  *
  * The hardware Programmer's countdown sweep assumes every NoC hop costs one cycle;
  * when some links carry extra fixed latency (`--hop-latencies`), countdown packets
  * to cores behind those links arrive later, so those cores enter execution with a
  * known positive skew — permanently, since all periods are equal. The hardware
  * cannot know the topology (seams are a runtime property), so the COMPILER absorbs
  * the skew in the schedule:
  *
  *   skew(core) = sum of (linkLatency - 1) over the countdown packet's path
  *                (east along row 0 to the core's column, then north along it)
  *   pad(core)  = maxSkew - skew(core)   NOPs prepended to the core's body
  *
  * Every core then begins its real schedule on the same global cycle
  * (T0 + maxSkew), restoring the aligned-start assumption the whole static schedule
  * rests on. The virtual-cycle length and sleep lengths are derived from the padded
  * bodies later in MachineCodeGenerator, so periods stay equal automatically.
  *
  * Runs immediately after ProgramSchedulingTransform: all later passes (jump
  * targets, lifetimes/register allocation, codegen) operate on the padded bodies.
  * With no hop-latency map configured every skew is zero and this is a no-op.
  */
object BootSkewPaddingTransform extends PlacedIRTransformer {
  import PlacedIR._

  override def transform(program: DefProgram)(implicit ctx: AssemblyContext): DefProgram = {

    // Per-chip boot (the image is split per chip, ctx.chipDimX > 0): each chip's
    // bootloader boots only its own cores over INTRA-CHIP, uniform 1-cycle hops (no
    // seam during boot), so the Programmer's own start-countdown sweep already aligns
    // them; and Bittide releases every IC from reset at a coordinated time, so there is
    // no inter-IC boot offset to compensate either. The seam-aware skew below applies
    // ONLY to the single-master boot-over-seam model (whole-torus boot).
    if (ctx.chipDimX > 0) {
      return program
    }

    val skews =
      program.processes.map(p => p.id -> ctx.hw_config.bootCountdownSkew(p.id.x, p.id.y)).toMap
    val maxSkew = if (skews.isEmpty) 0 else skews.values.max

    if (maxSkew == 0) {
      program // uniform links (or no latency map): nothing to compensate
    } else {
      ctx.logger.info(
        s"compensating boot skew of up to ${maxSkew} cycles (non-uniform link latencies)"
      )
      program.copy(processes = program.processes.map { p =>
        val pad = maxSkew - skews(p.id)
        if (pad == 0) p
        else {
          ctx.logger.debug(s"prepending ${pad} Nops to ${p.id} (boot skew ${skews(p.id)})")
          p.copy(body = Seq.fill(pad)(Nop) ++ p.body)
        }
      })
    }
  }
}
