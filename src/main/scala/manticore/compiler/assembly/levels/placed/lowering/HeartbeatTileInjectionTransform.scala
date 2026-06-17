package manticore.compiler.assembly.levels.placed.lowering

import manticore.compiler.AssemblyContext
import manticore.compiler.assembly.BinaryOperator
import manticore.compiler.assembly.{AssertionInterrupt, FinishInterrupt, StopInterrupt}
import manticore.compiler.assembly.levels.{ConstType, InputType, OutputType, WireType, UInt16}
import manticore.compiler.assembly.levels.placed.PlacedIR
import manticore.compiler.assembly.levels.placed.PlacedIRTransformer
import manticore.compiler.assembly.annotations.{
  AssemblyAnnotation,
  AssemblyAnnotationFields,
  StallWaveMarker,
  StringValue,
  Reg => RegAnnotation
}

/** Distributed scheduled stall wave — multi-IC heartbeat (post-placement).
  *
  * The single-IC half ([[manticore.compiler.assembly.levels.unconstrained.UnconstrainedStallWaveTransform]])
  * injects a per-vcycle countdown on the privileged process so a terminating
  * exception gates the local clock `stallMargin` vcycles later (deferred-precise).
  * On a multi-IC torus that is not enough: a `$display`/FINISH happens on ONE
  * chip's privileged core, but EVERY chip must gate on the SAME vcycle.
  *
  * This pass runs AFTER placement (coordinates are concrete) and BEFORE
  * `InterruptLoweringTransform` (so the synthetic STALL interrupt it emits picks up
  * the reserved 0x7FFF eid) and `ProgramSchedulingTransform` (so the explicit cross
  * chip `Send`s it emits are scheduled into timed `Recv`s). On each chip it augments
  * one privileged "tile" with:
  *
  *   - a persistent countdown register (`@REG` curr/next, init = sentinel "no stall");
  *   - a local seed: a terminating exception offers `seed = stallMargin + diameter`;
  *   - a min-merge of the (travel-adjusted) countdowns received from neighbour chips;
  *   - a saturating decrement;
  *   - a STALL interrupt (FINISH + [[StallWaveMarker]]) when the countdown hits 1;
  *   - one `Send` of its countdown to each neighbour chip's tile every vcycle.
  *
  * The countdown is encoded so that MIN is the natural merge: a large positive
  * `SENTINEL` means "no stall scheduled" (loses every min); an armed countdown is a
  * small value that every chip drives toward the same global zero-crossing. Each
  * received word is travel-adjusted by the directed link's per-hop vcycle latency so
  * that, despite the send delay, all chips reach the STALL threshold on the same
  * vcycle. Seeding with `stallMargin + diameter` guarantees the wave reaches the
  * farthest chip before its adjusted countdown would underflow the margin floor.
  *
  * Gated by `ctx.stallWave && ctx.chipDimX > 0`; a no-op otherwise (the single-chip
  * path uses the unconstrained transform).
  */
object HeartbeatTileInjectionTransform extends PlacedIRTransformer {
  import PlacedIR._

  /** A large positive (signed-16-bit) value meaning "no stall scheduled". Must be
    * bigger than any real countdown (`stallMargin + diameter`) yet stay positive so
    * the signed SLT min-merge orders it as the maximum. */
  private val SENTINEL = 16384

  override def transform(program: DefProgram)(implicit ctx: AssemblyContext): DefProgram = {
    if (!ctx.stallWave || ctx.chipDimX <= 0) {
      program
    } else {
      inject(program)
    }
  }

  private def inject(program: DefProgram)(implicit ctx: AssemblyContext): DefProgram = {
    val chipDimX = ctx.chipDimX
    val chipDimY = if (ctx.chipDimY > 0) ctx.chipDimY else ctx.hw_config.dimY
    val chipCols = ctx.hw_config.dimX / chipDimX
    val chipRows = ctx.hw_config.dimY / chipDimY
    val diameter = (chipCols - 1) + (chipRows - 1)
    val seedVal  = ctx.stallMargin + diameter

    // Per-hop travel adjustment, in vcycles. A neighbour's countdown arrives stale by
    // the directed link's vcycle latency; subtracting it makes every chip zero-cross
    // the same vcycle. The inherent cross-process state delay is one vcycle; seam
    // crossings on real hardware are longer (sourced from --hop-latencies), but the
    // interpreter models only the one-vcycle hop, so we use 1 here.
    val hopLatency = 1

    def chipOf(p: DefProcess): (Int, Int) = (p.id.x / chipDimX, p.id.y / chipDimY)

    val byChip = program.processes.groupBy(chipOf)
    def isPrivileged(p: DefProcess): Boolean = p.body.exists(_.isInstanceOf[PrivilegedInstruction])
    def cornerOf(chip: (Int, Int)): (Int, Int) = (chip._1 * chipDimX, chip._2 * chipDimY)

    // EVERY chip in the grid gets exactly one heartbeat tile, on its MASTER core
    // (chip-local (0,0) = the global corner) — only the master core's exception reaches
    // that chip's Management. Selection per chip:
    //   - if the chip hosts the application reporter (a privileged process), that IS the
    //     tile (a second privileged process would break CodeDump; the reporter is placed
    //     on the chip master);
    //   - else augment the process already at the corner;
    //   - else synthesize a minimal tile process at the corner (covers chips the placer
    //     left empty — without this they would never receive a STALL and never gate).
    val allChips: Seq[(Int, Int)] =
      for (cx <- 0 until chipCols; cy <- 0 until chipRows) yield (cx, cy)
    val syntheticTiles = scala.collection.mutable.ArrayBuffer.empty[DefProcess]
    val tileOf: Map[(Int, Int), DefProcess] = allChips.map { chip =>
      val (cornerX, cornerY) = cornerOf(chip)
      val picked = byChip
        .getOrElse(chip, Seq.empty)
        .find(isPrivileged)
        .orElse(program.processes.find(p => p.id.x == cornerX && p.id.y == cornerY))
        .getOrElse {
          val syn = DefProcess(
            id = ProcessIdImpl(s"hb_tile_${chip._1}_${chip._2}", cornerX, cornerY),
            registers = Seq.empty,
            functions = Seq.empty,
            body = Seq.empty
          )
          syntheticTiles += syn
          syn
        }
      chip -> picked
    }.toMap
    val tileId: Map[(Int, Int), ProcessId] = tileOf.map { case (chip, p) => chip -> p.id }

    def neighbours(chip: (Int, Int)): Seq[(Int, Int)] = {
      val (cx, cy) = chip
      Seq((cx - 1, cy), (cx + 1, cy), (cx, cy - 1), (cx, cy + 1)).filter { case (x, y) =>
        x >= 0 && x < chipCols && y >= 0 && y < chipRows
      }
    }

    // rx register on `receiver` chip holding the countdown received from `sender` chip
    def rxName(receiver: (Int, Int), sender: (Int, Int)): Name =
      s"%hb_rx_${receiver._1}_${receiver._2}_from_${sender._1}_${sender._2}"

    ctx.logger.info(
      s"Stall-wave heartbeat: ${chipCols}x${chipRows} chips, diameter ${diameter}, seed ${seedVal}, " +
        s"${syntheticTiles.length} synthetic tile(s) on empty chips"
    )

    val rewritten: Map[ProcessId, DefProcess] = tileOf.map { case (chip, proc) =>
      proc.id -> augmentTile(chip, proc, neighbours(chip), tileId, seedVal, hopLatency, rxName)
    }

    // replace augmented existing processes in place; append the synthetic corner tiles
    val existing  = program.processes.map { p => rewritten.getOrElse(p.id, p) }
    val synthetic = syntheticTiles.toSeq.map { syn => rewritten(syn.id) }
    program.copy(processes = existing ++ synthetic)
  }

  private def augmentTile(
      chip: (Int, Int),
      proc: DefProcess,
      neigh: Seq[(Int, Int)],
      tileId: Map[(Int, Int), ProcessId],
      seedVal: Int,
      hopLatency: Int,
      rxName: ((Int, Int), (Int, Int)) => Name
  )(implicit ctx: AssemblyContext): DefProcess = {

    val newRegs   = scala.collection.mutable.ArrayBuffer.empty[DefReg]
    val newBody   = scala.collection.mutable.ArrayBuffer.empty[Instruction]
    val constPool = scala.collection.mutable.Map.empty[Int, Name]

    def fresh(p: String): Name = s"%hb_${p}_${ctx.uniqueNumber()}"
    def wire(p: String): Name = {
      val n = fresh(p); newRegs += DefReg(ValueVariable(n, -1, WireType), None); n
    }
    // Reuse an existing constant of the same value rather than minting a duplicate:
    // InterruptLoweringTransform (which runs right after this pass) dedups constants by
    // value and assumes names are unique per value (true after CSE). Introducing a
    // second constant with an already-present value would make that dedup drop one
    // declaration while leaving its name referenced in the body.
    val existingConsts: Map[Int, Name] = proc.registers.collect {
      case DefReg(ValueVariable(n, _, ConstType), Some(v), _) => v.toInt -> n
    }.toMap
    def const(v: Int): Name = constPool.getOrElseUpdate(
      v,
      existingConsts.getOrElse(
        v, {
          val n = fresh(s"c$v"); newRegs += DefReg(ValueVariable(n, -1, ConstType), Some(UInt16(v))); n
        }
      )
    )
    def emit(op: BinaryOperator.BinaryOperator, a: Name, b: Name, p: String): Name = {
      val rd = wire(p); newBody += BinaryArithmetic(op, rd, a, b); rd
    }
    def regAnnon(id: String, tpe: StringValue): AssemblyAnnotation =
      RegAnnotation(
        Map(
          AssemblyAnnotationFields.Id.name   -> StringValue(id),
          AssemblyAnnotationFields.Type.name -> tpe
        )
      )
    // min(a, b) into rd: rd = (a < b) ? a : b
    def minInto(a: Name, b: Name, rd: Name): Unit = {
      val lt = emit(BinaryOperator.SLT, a, b, "lt")
      newBody += Mux(rd, lt, b, a) // sel ? rtrue : rfalse  ==  (a<b) ? a : b
    }

    val cSent = const(SENTINEL)
    val cOne  = const(1)
    val cSeed = const(seedVal)

    // persistent countdown state (init sentinel = disarmed)
    val stateId = s"hb_cd_${chip._1}_${chip._2}"
    val cdCurr  = s"%${stateId}_curr"
    val cdNext  = s"%${stateId}_next"
    newRegs += DefReg(ValueVariable(cdCurr, -1, InputType), Some(UInt16(SENTINEL)), Seq(regAnnon(stateId, RegAnnotation.Current)))
    newRegs += DefReg(ValueVariable(cdNext, -1, OutputType), None, Seq(regAnnon(stateId, RegAnnotation.Next)))

    // local seed trigger from this tile's terminating interrupts (Expect semantics:
    // fires when condition != rs2; rs2 = 1 for ASSERT else 0).
    val terminating = proc.body.collect {
      case Interrupt(SimpleInterruptDescription(FinishInterrupt, _, _), c, _, _)    => (c, 0)
      case Interrupt(SimpleInterruptDescription(StopInterrupt, _, _), c, _, _)      => (c, 0)
      case Interrupt(SimpleInterruptDescription(AssertionInterrupt, _, _), c, _, _) => (c, 1)
    }
    val seedTrigger: Name =
      if (terminating.isEmpty) const(0)
      else
        terminating
          .map { case (c, rs2) =>
            val eq = emit(BinaryOperator.SEQ, c, const(rs2), "eq")
            emit(BinaryOperator.XOR, eq, cOne, "fired")
          }
          .reduce { (a, b) => emit(BinaryOperator.OR, a, b, "anyfired") }

    // localSeed = seedTrigger ? SEED : SENTINEL
    val localSeed = wire("localseed")
    newBody += Mux(localSeed, seedTrigger, cSent, cSeed)

    // decremented = (cd == SENTINEL) ? SENTINEL : cd - 1   (sentinel is stable)
    val isSent      = emit(BinaryOperator.SEQ, cdCurr, cSent, "issent")
    val cdMinus     = emit(BinaryOperator.SUB, cdCurr, cOne, "cdminus")
    val decremented = wire("dec")
    newBody += Mux(decremented, isSent, cdMinus, cSent)

    // per-neighbour received word, travel-adjusted: adj = (rx == SENTINEL) ? SENTINEL : rx - L
    val cL = const(hopLatency)
    val adjs: Seq[Name] = neigh.map { n =>
      val rx = rxName(chip, n)
      newRegs += DefReg(
        ValueVariable(rx, -1, InputType),
        Some(UInt16(SENTINEL)),
        Seq(regAnnon(s"hb_rxid_${chip._1}_${chip._2}_from_${n._1}_${n._2}", RegAnnotation.Current))
      )
      val rxIsSent = emit(BinaryOperator.SEQ, rx, cSent, "rxsent")
      val rxMinus  = emit(BinaryOperator.SUB, rx, cL, "rxminus")
      val adj      = wire("adj")
      newBody += Mux(adj, rxIsSent, rxMinus, cSent)
      adj
    }

    // cdNext = min(localSeed, decremented, adj...) — fold so the last min writes cdNext
    val terms = Seq(localSeed, decremented) ++ adjs
    var acc   = terms.head
    for (i <- 1 until terms.size) {
      val rd = if (i == terms.size - 1) cdNext else wire("min")
      minInto(acc, terms(i), rd)
      acc = rd
    }

    // STALL when cd == 1 (last running vcycle). FINISH action + marker -> eid 0x7FFF.
    val stallFire = emit(BinaryOperator.SEQ, cdCurr, cOne, "stallfire")
    val maxOrder  = proc.body.collect { case i: Interrupt => i.order.value }.maxOption.getOrElse(-1)
    newBody += Interrupt(
      SimpleInterruptDescription(FinishInterrupt, None, -1),
      stallFire,
      SystemCallOrder(maxOrder + 1),
      Seq(StallWaveMarker())
    )

    // send this tile's countdown to each neighbour tile's rx register every vcycle
    neigh.foreach { n =>
      newBody += Send(rxName(n, chip), cdNext, tileId(n))
    }

    proc.copy(
      registers = proc.registers ++ newRegs.toSeq,
      body = proc.body ++ newBody.toSeq
    )
  }
}
