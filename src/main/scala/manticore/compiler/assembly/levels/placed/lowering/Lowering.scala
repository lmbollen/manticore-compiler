package manticore.compiler.assembly.levels.placed.lowering

import manticore.compiler.assembly.levels.placed.PlacedIRTransformer
import manticore.compiler.assembly.levels.placed.PlacedIR
import manticore.compiler.AssemblyContext
import manticore.compiler.assembly.levels.placed.JumpTableNormalizationTransform
import manticore.compiler.assembly.levels.placed.JumpLabelAssignmentTransform

object Lowering {

  val Transformation =
    HeartbeatTileInjectionTransform andThen // multi-IC stall wave: per-chip tiles + cross-chip min-merge (no-op unless --stall-wave with a chip split)
      InterruptLoweringTransform andThen
      JumpTableNormalizationTransform andThen
      ProgramSchedulingTransform andThen
      BootSkewPaddingTransform andThen // multi-chip: align core start times (no-op for uniform links)
      SetJumpTargetsTransform andThen
      LocalMemoryAllocation andThen
      RegisterAllocationTransform

}
