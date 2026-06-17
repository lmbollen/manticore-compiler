package manticore.compiler.assembly.annotations

/** Marker annotation placed by [[manticore.compiler.assembly.levels.unconstrained.UnconstrainedStallWaveTransform]]
  * on the synthetic STALL interrupt of the distributed scheduled stall wave.
  *
  * It carries no fields; its presence on an `Interrupt`'s `annons` tells
  * `InterruptLoweringTransform` to assign the reserved STALL exception id
  * (0x7FFF, matching the hardware `Management.STALL_EID`) instead of a regular
  * success eid. The marker lives only in-memory between the frontend heartbeat
  * pass and interrupt lowering (it serializes to nothing, like any field-less
  * annotation), so it never needs a parser entry.
  */
final class StallWaveMarker extends AssemblyAnnotation {
  override val name: String = StallWaveMarker.name
  override val fields: Map[AssemblyAnnotationFields.FieldName, AnnotationValue] =
    Map.empty
}

object StallWaveMarker {
  val name: String = "STALLWAVE"
  def apply(): StallWaveMarker = new StallWaveMarker
}
