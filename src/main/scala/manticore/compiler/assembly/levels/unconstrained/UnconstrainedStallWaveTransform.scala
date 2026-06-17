package manticore.compiler.assembly.levels.unconstrained

import manticore.compiler.AssemblyContext
import manticore.compiler.assembly.BinaryOperator
import manticore.compiler.assembly.{
  AssertionInterrupt,
  FinishInterrupt,
  StopInterrupt
}
import manticore.compiler.assembly.levels.{ConstType, InputType, OutputType, WireType}
import manticore.compiler.assembly.annotations.{
  AnnotationValue,
  AssemblyAnnotation,
  AssemblyAnnotationFields,
  StallWaveMarker,
  StringValue,
  Reg => RegAnnotation
}

/** Distributed scheduled stall wave — compiler half (single-IC heartbeat).
  *
  * In `--stall-wave` mode a terminating exception (FINISH/STOP/ASSERT) must NOT
  * gate the local compute clock immediately. Instead the privileged core runs a
  * per-virtual-cycle countdown "heartbeat": the first terminating exception seeds
  * the countdown to `stallMargin`; each subsequent vcycle the countdown decrements
  * (saturating at 0); when it reaches 1 the core raises a reserved STALL interrupt
  * (eid 0x7FFF, the hardware `Management.STALL_EID`) whose Expect — and only whose
  * Expect — gates the clock. The application exception's eid is still captured by
  * Management, but execution continues through the `stallMargin` quiesce window so
  * that, across independently-clocked ICs, every core can be made to zero-cross the
  * same vcycle (the seed/decrement is travel-adjusted across seams in the multi-IC
  * extension; on a single IC the wave degenerates to a fixed `stallMargin` defer).
  *
  * This pass injects that dataflow on the privileged process (the one carrying the
  * interrupts) using the standard `.reg` current/next state-register mechanism, so
  * the rest of the pipeline (width conversion, scheduling, register allocation,
  * interrupt lowering) handles it with no special casing — except that
  * `InterruptLoweringTransform` assigns the reserved 0x7FFF eid to the interrupt
  * carrying the [[StallWaveMarker]] this pass attaches.
  *
  * Gated by `ctx.stallWave`; a no-op (byte-identical) when off.
  */
object UnconstrainedStallWaveTransform extends UnconstrainedIRTransformer {
  import UnconstrainedIR._

  override def transform(program: DefProgram)(implicit ctx: AssemblyContext): DefProgram = {
    // Multi-IC builds (a chip split is requested) route the heartbeat through the
    // post-placement HeartbeatTileInjectionTransform instead, which adds the cross
    // chip min-merge; injecting here too would double-count the countdown.
    if (!ctx.stallWave || ctx.chipDimX > 0) {
      program
    } else {
      program.copy(processes = program.processes.map(transformProcess))
    }
  }

  private def transformProcess(process: DefProcess)(implicit ctx: AssemblyContext): DefProcess = {
    // Terminating interrupts only: $display (SerialInterrupt) recurs every vcycle
    // and must not seed the stall.
    val terminating = process.body.collect {
      case i @ Interrupt(InterruptDescription(FinishInterrupt), c, _, _)    => (i, c, 0)
      case i @ Interrupt(InterruptDescription(StopInterrupt), c, _, _)      => (i, c, 0)
      case i @ Interrupt(InterruptDescription(AssertionInterrupt), c, _, _) => (i, c, 1)
    }
    if (terminating.isEmpty) {
      // Not the privileged process (or nothing that can terminate) — leave it be.
      return process
    }

    val newRegs   = scala.collection.mutable.ArrayBuffer.empty[DefReg]
    val newBody   = scala.collection.mutable.ArrayBuffer.empty[Instruction]
    val constPool = scala.collection.mutable.Map.empty[(BigInt, Int), Name]

    // The countdown is a normal native (16-bit) word; every comparison/boolean result
    // is 1-bit — matching how the rest of the IR types SEQ/predicate outputs (e.g. the
    // `.reg r1 1` done flags) — so the hardware Mux selects and the Expect conditions
    // see a genuine 1-bit boolean rather than a 16-bit value.
    val CW = 16

    def fresh(prefix: String): Name = s"%${prefix}_sw${ctx.uniqueNumber()}"

    def wire(prefix: String, width: Int): Name = {
      val n = fresh(prefix)
      newRegs += DefReg(LogicVariable(n, width, WireType), None)
      n
    }
    def const(value: BigInt, width: Int): Name = constPool.getOrElseUpdate(
      (value, width), {
        val n = fresh(s"c${value}w${width}")
        newRegs += DefReg(LogicVariable(n, width, ConstType), Some(value))
        n
      }
    )
    def emit(rd: Name, op: BinaryOperator.BinaryOperator, rs1: Name, rs2: Name): Name = {
      newBody += BinaryArithmetic(op, rd, rs1, rs2)
      rd
    }
    def regAnnon(id: String, tpe: StringValue): AssemblyAnnotation =
      RegAnnotation(
        Map(
          AssemblyAnnotationFields.Id.name   -> StringValue(id),
          AssemblyAnnotationFields.Type.name -> tpe
        )
      )

    // operand widths, so comparison constants are built at the condition's own width
    val widthOf = process.registers.map(r => r.variable.name -> r.variable.width).toMap

    // Persistent countdown state register pair (init 0 == disarmed), native word width.
    val stateId  = s"stall_cd${ctx.uniqueNumber()}"
    val cdCurr   = s"%${stateId}_curr"
    val cdNext   = s"%${stateId}_next"
    newRegs += DefReg(
      LogicVariable(cdCurr, CW, InputType),
      Some(BigInt(0)),
      Seq(regAnnon(stateId, RegAnnotation.Current))
    )
    newRegs += DefReg(
      LogicVariable(cdNext, CW, OutputType),
      None,
      Seq(regAnnon(stateId, RegAnnotation.Next))
    )

    val zeroW = const(0, CW)
    val oneW  = const(1, CW)
    val seed  = const(BigInt(ctx.stallMargin), CW)
    val one1  = const(1, 1)

    // seed_trigger = OR over terminating interrupts of "fired this vcycle" (1-bit).
    // Expect semantics: exception fires when condition != rs2 (rs2 = 1 for ASSERT,
    // else 0). fired = (cond != rs2) = NOT (cond SEQ rs2), compared at cond's width.
    val firedBits = terminating.map { case (_, cond, rs2) =>
      val w  = widthOf.getOrElse(cond, 1)
      val eq = emit(wire("eq", 1), BinaryOperator.SEQ, cond, const(BigInt(rs2), w))
      emit(wire("fired", 1), BinaryOperator.XOR, eq, one1)
    }
    val seedTrigger = firedBits.reduce { (a, b) =>
      emit(wire("anyfired", 1), BinaryOperator.OR, a, b)
    }

    // a0 = (cd == 0) == "disarmed"; armed = NOT a0; seed only on the rising edge
    // (disarmed AND a terminating exception fired) so a persistently-true FINISH
    // does not re-seed every vcycle.
    val a0       = emit(wire("disarmed", 1), BinaryOperator.SEQ, cdCurr, zeroW)
    val armed    = emit(wire("armed", 1), BinaryOperator.XOR, a0, one1)
    val seedNow  = emit(wire("seednow", 1), BinaryOperator.AND, seedTrigger, a0)

    // countdown_dec = armed ? (cd - 1) : 0 ; cd_next = seed_now ? SEED : countdown_dec
    val dec      = emit(wire("dec", CW), BinaryOperator.SUB, cdCurr, oneW)
    val cdDec    = wire("cddec", CW)
    newBody += Mux(cdDec, armed, zeroW, dec)
    newBody += Mux(cdNext, seedNow, cdDec, seed)

    // STALL fires when cd == 1 (this is the last running vcycle). FinishInterrupt
    // action so it encodes/gates like a terminator; the marker routes it to the
    // reserved 0x7FFF eid in InterruptLoweringTransform.
    val stallFire = emit(wire("stallfire", 1), BinaryOperator.SEQ, cdCurr, oneW)
    val maxOrder = process.body.collect { case i: Interrupt => i.order.value }.maxOption.getOrElse(-1)
    newBody += Interrupt(
      InterruptDescription(FinishInterrupt),
      stallFire,
      SystemCallOrder(maxOrder + 1),
      Seq(StallWaveMarker())
    )

    process.copy(
      registers = process.registers ++ newRegs.toSeq,
      body = process.body ++ newBody.toSeq
    )
  }
}
