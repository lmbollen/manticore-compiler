package manticore.compiler.assembly.levels.codegen

import manticore.compiler.assembly.levels.AssemblyTransformer
import manticore.compiler.assembly.levels.CompilerTransformation
import manticore.compiler.assembly.levels.placed.PlacedIR._
import manticore.compiler.FunctionalTransformation
import manticore.compiler.AssemblyContext
import manticore.compiler.LoggerId
import manticore.compiler.assembly.{SerialInterrupt, FinishInterrupt, StopInterrupt, AssertionInterrupt}
import manticore.compiler.FormatString.FmtBin
import manticore.compiler.FormatString.FmtHex
import manticore.compiler.FormatString.FmtDec
import manticore.compiler.FormatString.FmtConcat
import manticore.compiler.FormatString.FmtLit
import java.io.PrintWriter

object CodeDump extends FunctionalTransformation[DefProgram, Unit] {
  private implicit val loggerId = LoggerId("CodeDump")
  override def apply(program: DefProgram)(implicit ctx: AssemblyContext): Unit = {

    if (ctx.output_dir.isEmpty) {
      ctx.logger.error("No output dir specified!")
      ctx.logger.fail("Could not continue")
    }

    val outDir = ctx.output_dir.get

    // assemble the initializer phases + the main program once
    ctx.logger.info("Dumping initializers")
    val initializers  = InitializerProgram.makeInitializer(program).map(MachineCodeGenerator.assembleProgram)
    val mainAssembled = MachineCodeGenerator.assembleProgram(program)

    // Emit either ONE combined image (single-chip / whole-torus boot, the default) or,
    // when a per-chip split is requested (ctx.chipDimX > 0), one image PER PHYSICAL CHIP.
    // The split is purely a programming concern: it partitions the per-core blocks by
    // node location and re-bases each block's boot hops to that chip's local origin, so
    // each chip's bootloader programs only its own cores. The compilation itself is
    // unchanged (one global torus; the only multi-chip input is latencies.csv).
    //
    // chipImages: per chip, (cx, cy, initializer exec.bin paths, main exec.bin path).
    val chipImages: Seq[(Int, Int, Seq[String], String)] =
      if (ctx.chipDimX > 0) {
        val chipDimX = ctx.chipDimX
        val chipDimY = if (ctx.chipDimY > 0) ctx.chipDimY else ctx.hw_config.dimY
        val chipCols = ctx.hw_config.dimX / chipDimX
        val chipRows = ctx.hw_config.dimY / chipDimY
        ctx.logger.info(
          s"Per-chip image split: ${chipCols}x${chipRows} chips of ${chipDimX}x${chipDimY}"
        )
        for {
          cy <- 0 until chipRows
          cx <- 0 until chipCols
        } yield {
          // Membership and boot addressing follow the RTL's FOLDED double-cut torus
          // (Fold), NOT a contiguous tile: chip (cx, cy) owns the global ring
          // positions Fold.chip(x)==cx && Fold.chip(y)==cy, which is non-contiguous
          // for a multi-chip dimension. A contiguous split (x / chipDimX) would load
          // each core into the wrong physical chip and misroute. MachineCodeGenerator
          // addresses each core's chip-local slot during the standalone boot.
          def inChip(p: MachineCodeGenerator.AssembledProcess): Boolean =
            Fold.chip(p.place._1, chipCols, chipDimX) == cx &&
              Fold.chip(p.place._2, chipRows, chipDimY) == cy
          val chipName = s"chip_${cx}_${cy}"
          val chipDir  = outDir.toPath.resolve(chipName)
          val initPaths = initializers.zipWithIndex.map { case (init, ix) =>
            val dir = chipDir.resolve(s"init_${ix}")
            MachineCodeGenerator.generateCode(init.filter(inChip), dir, chipDimX, chipDimY, chipCols, chipRows)
            dir.resolve("exec.bin").toAbsolutePath().toString()
          }
          val mainDir = chipDir.resolve("main")
          MachineCodeGenerator.generateCode(mainAssembled.filter(inChip), mainDir, chipDimX, chipDimY, chipCols, chipRows)
          (cx, cy, initPaths, mainDir.resolve("exec.bin").toAbsolutePath().toString())
        }
      } else {
        val initDirs = Range(0, initializers.length).map { index => outDir.toPath.resolve(s"init_${index}") }
        initializers.zip(initDirs).foreach { case (init, dir) => MachineCodeGenerator.generateCode(init, dir) }
        ctx.logger.info("Dumping program")
        val programDir = outDir.toPath.resolve("main")
        MachineCodeGenerator.generateCode(mainAssembled, programDir)
        Seq(
          (
            0,
            0,
            initDirs.map(_.resolve("exec.bin").toAbsolutePath().toString()).toSeq,
            programDir.resolve("exec.bin").toAbsolutePath().toString()
          )
        )
      }

    ctx.logger.info("Creating manifest")

    // Find the privileged process(es). A single global privileged process is the
    // common case; a per-chip split (the multi-IC stall wave) has AT MOST ONE per
    // chip, each owning that chip's exceptions + global memories.
    val cdX = ctx.chipDimX
    val cdY = if (ctx.chipDimY > 0) ctx.chipDimY else ctx.hw_config.dimY
    // Group by the FOLDED chip (matching inChip and HeartbeatTileInjectionTransform);
    // a contiguous (p.id.x / cdX) grouping would put fold-distinct chips' tiles in the
    // same bucket and spuriously report >1 privileged process per chip.
    def chipOfProc(p: DefProcess): (Int, Int) =
      if (cdX > 0)
        (Fold.chip(p.id.x, ctx.hw_config.dimX / cdX, cdX), Fold.chip(p.id.y, ctx.hw_config.dimY / cdY, cdY))
      else (0, 0)
    val privilegedByChip: Map[(Int, Int), Seq[DefProcess]] =
      program.processes.filter(_.body.exists(_.isInstanceOf[PrivilegedInstruction])).groupBy(chipOfProc)
    if (cdX > 0) {
      privilegedByChip.foreach { case ((cx, cy), ps) =>
        if (ps.size > 1) ctx.logger.fail(s"Chip (${cx}, ${cy}) has more than one privileged process")
      }
    } else {
      val n = privilegedByChip.values.flatten.size
      if (n == 0) ctx.logger.fail("Could not find a privileged process")
      else if (n > 1) ctx.logger.fail("Can not handle more than one privileged process")
    }
    def interruptsOf(chip: (Int, Int)): Seq[Interrupt] =
      privilegedByChip.getOrElse(chip, Nil).flatMap(_.body.collect { case x: Interrupt => x })
    def globalMemoriesOf(chip: (Int, Int)): Seq[DefGlobalMemory] =
      privilegedByChip.getOrElse(chip, Nil).flatMap(_.globalMemories)
    // single-chip view (the common case): the lone privileged process's data
    val globalMemories = privilegedByChip.values.flatten.flatMap(_.globalMemories).toSeq
    val interrupts     = privilegedByChip.values.flatten.flatMap(_.body.collect { case x: Interrupt => x }).toSeq

    def jsonValue[V](v: V): String = {
      if (v.isInstanceOf[String]) {
        "\"" + v + "\""
      } else if (v.isInstanceOf[Iterable[_]]) {
        "[" + v.asInstanceOf[Iterable[_]].map(jsonValue).mkString(", ") + "]"
      } else {
        v.toString()
      }
    }
    def asRecord[V](tabs: Int, fields: (String, V)*): String = {

      val record = fields
        .map { case (k, v) =>
          s"    \"${k}\": ${jsonValue(v)}"
        }
        .map(" " * tabs + _)
        .mkString(",\n")
      record
    }
    def asJson[V](tabs: Int, fields: (String, V)*): String = {
      val record = asRecord(tabs * 2, fields: _*)
      " " * tabs + "{\n" + record + "\n" + " " * tabs + "}"
    }
    def interruptEntries(interrupts: Seq[Interrupt]): String = interrupts
      .map {
        case Interrupt(SimpleInterruptDescription(action, info, eid), _, _, _) =>
          val tpe = (action: @unchecked) match {
            case AssertionInterrupt => "ASSERT"
            case FinishInterrupt    => "FINISH"
            case StopInterrupt      => "STOP"
          }
          val str = asJson(
            12,
            "type" -> tpe,
            "eid"  -> eid,
            "info" -> info.map(_.getFile()).getOrElse("")
          )
          str
        case Interrupt(SerialInterruptDescription(SerialInterrupt(fmt), info, eid, pointers), _, _, _) =>
          val ptrQ = scala.collection.mutable.Queue.from(pointers)

          val fmtStr = fmt.parts
            .map {
              case FmtLit(chars) =>
                asJson(20, "type" -> "string", "value" -> chars)
              case FmtHex(w) =>
                asJson(20, "type" -> "hex", "offsets" -> Seq(ptrQ.dequeue()), "bitwidth" -> w)
              case FmtBin(w) =>
                asJson(20, "type" -> "bin", "offsets" -> Seq(ptrQ.dequeue()), "bitwidth" -> w)
              case d @ FmtDec(w, fillZero) =>
                asJson(
                  20,
                  "type"     -> "dec",
                  "offsets"  -> Seq(ptrQ.dequeue()),
                  "bitwidth" -> w,
                  "digits"   -> d.decWidth,
                  "zeros"    -> fillZero
                )
              case FmtConcat(atoms, width) =>
                val offsets = atoms.map(_ => ptrQ.dequeue())
                atoms.head match {
                  case t: FmtBin =>
                    asJson(20, "type" -> "bin", "bitwidth" -> width, "digits" -> width, "offsets" -> offsets)
                  case t: FmtDec =>
                    asJson(
                      20,
                      "type"     -> "dec",
                      "bitwidth" -> width,
                      "digits"   -> t.withWidth(width).decWidth,
                      "offsets"  -> offsets,
                      "zeros"    -> t.fillZero
                    )
                  case t: FmtHex =>
                    asJson(20, "type" -> "hex", "bitwidth" -> width, "digits" -> width, "offsets" -> offsets)
                }

            }
            .mkString(",\n")
          val fmtField = " " * 16 + "\"fmt\": [\n" + fmtStr + "\n" + " " * 16 + "]"
          " " * 12 + "{\n" +
            Seq(
              s"\"type\": \"FLUSH\"",
              s"\"eid\": ${eid}",
              s"\"info\": \"${info.map(_.getFile()).getOrElse("")}\""
            ).map(" " * 16 + _).mkString(",\n") + ",\n" + fmtField + "\n" + " " * 12 + "}"

      }
      .mkString(",\n")
    def memoryEntries(globalMemories: Seq[DefGlobalMemory]): String = globalMemories
      .map { case DefGlobalMemory(_, size, base, _, _) =>
        asJson(12, "size" -> size, "base" -> base)
      }
      .mkString(",\n")
    val userMemoryBase  = asRecord(4, "base" -> ctx.hw_config.userGlobalMemoryBase)
    val gridJson        = asRecord(4, "dimx" -> ctx.hw_config.dimX, "dimy" -> ctx.hw_config.dimY)
    // image section: single combined image (initializers + program), or per-chip "chips"
    val imageSection: String =
      if (ctx.chipDimX > 0) {
        val chipDimYEff = if (ctx.chipDimY > 0) ctx.chipDimY else ctx.hw_config.dimY
        val chipsArr = chipImages
          .map { case (cx, cy, initPaths, mainPath) =>
            // each chip carries its OWN privileged process's exceptions + memories
            " " * 12 + "{\n" +
              asRecord(8, "cx" -> cx, "cy" -> cy, "initializers" -> initPaths, "program" -> mainPath) + ",\n" +
              " " * 16 + "\"exceptions\": [\n" + interruptEntries(interruptsOf((cx, cy))) + "\n" + " " * 16 + "],\n" +
              " " * 16 + "\"memories\": [\n" + memoryEntries(globalMemoriesOf((cx, cy))) + "\n" + " " * 16 + "]\n" +
              " " * 12 + "}"
          }
          .mkString(",\n")
        asRecord(4, "chip_dim_x" -> ctx.chipDimX, "chip_dim_y" -> chipDimYEff) + ",\n" +
          " " * 8 + "\"chips\": [\n" + chipsArr + "\n" + " " * 8 + "]"
      } else {
        val (_, _, initPaths, mainPath) = chipImages.head
        asRecord(4, "initializers" -> initPaths) + ",\n" + asRecord(4, "program" -> mainPath)
      }
    val exceptionsJson  = " " * 8 + "\"exceptions\": [\n" + interruptEntries(interrupts) + "\n" + " " * 8 + "]"
    val globalMemoriesJson = " " * 8 + "\"memories\": [\n" + memoryEntries(globalMemories) + "\n" + " " * 8 + "]"
    val manifest =
      if (ctx.chipDimX > 0) {
        // per-chip mode: each chip's exceptions + memories live inside its chips[] entry
        "{\n" + Seq(gridJson, imageSection, userMemoryBase).mkString(",\n") + "\n}"
      } else {
        "{\n" + Seq(gridJson, imageSection, exceptionsJson, userMemoryBase, globalMemoriesJson).mkString(
          ",\n"
        ) + "\n}"
      }
    val manifestWriter = new PrintWriter(outDir.toPath.resolve("manifest.json").toFile)
    manifestWriter.write(manifest)
    manifestWriter.close()
  }

}
