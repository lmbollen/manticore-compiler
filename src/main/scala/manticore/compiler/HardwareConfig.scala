package manticore.compiler

import manticore.compiler.assembly.ManticoreAssemblyIR
import manticore.compiler.assembly.levels.placed.PlacedIR

sealed trait HardwareConfig {
  import PlacedIR._

  // number of machine registers
  val nRegisters: Int
  // instruction memory capacity in instructions
  val nInstructions: Int
  // scratch pad capacity in 16-bit words
  val nScratchPad: Int

  // number of custom functions
  val nCustomFunctions: Int
  // number of inputs to the custom function unit
  val nCfuInputs: Int


  val dimX: Int
  val dimY: Int

  val maxLatency: Int

  // number of pipes in the recv path
  val recvPipes: Int
  // number of pipes in the send path after decode
  val sendPipes: Int
  // latency of decoding
  val decodeLatency: Int

  protected val nHops: Int = 1

  def latency(inst: Instruction): Int = inst match {
    case _: Predicate => 0
    // sink instructions can only have anti-dependencies which are independent of
    // the time it takes to commit them. For an anti dependence LST -> LLD only
    // mandates placing LST first, but no Nop is needed in between them.
    case _ @(_: Interrupt | _: PutSerial | _: GlobalStore | _: LocalStore) => 0
    case Nop                                   => 0
    case JumpTable(_, _, blocks, delaySlot, _) =>
      // this is a conservative estimation
      val delaySlotLatency = 1 + delaySlot.length
      delaySlotLatency + blocks.map { case JumpCase(_, blk) =>
        blk.length + maxLatency + 1
      }.max

    case _ => maxLatency
  }

  // Signed shortest-path hop counts for bidirectional routing.
  // Positive = forward (+X/+Y); negative = backward (-X/-Y).
  // Ties (exactly half the torus) are broken in favour of the forward direction.
  def xHops(source: ProcessId, target: ProcessId): Int = {
    val forward  = if (source.x <= target.x) target.x - source.x
                   else dimX - source.x + target.x
    val backward = dimX - forward
    (if (forward <= backward) forward else -backward) * nHops
  }

  def yHops(source: ProcessId, target: ProcessId): Int = {
    val forward  = if (source.y <= target.y) target.y - source.y
                   else dimY - source.y + target.y
    val backward = dimY - forward
    (if (forward <= backward) forward else -backward) * nHops
  }

  def xyHops(source: ProcessId, target: ProcessId): (Int, Int) = {
    (xHops(source, target), yHops(source, target))
  }

  def manhattan(source: ProcessId, target: ProcessId): Int = {
    // Absolute values because hops are now signed.
    math.abs(xHops(source, target)) + math.abs(yHops(source, target))
  }

  val userGlobalMemoryBase = 0x00004000L // the first 16Ki shorts are reserved
  // to the system

}

case class DefaultHardwareConfig(
    dimX: Int,
    dimY: Int,
    maxLatency: Int = 10,
    nRegisters: Int = 2048,
    nCarries: Int = 64,
    nScratchPad: Int = (1 << 14),
    nInstructions: Int = 4096,
    nCustomFunctions: Int = 32,
    nCfuInputs: Int = 4,
    decodeLatency: Int = 5,
    recvPipes: Int = 7,

    sendPipes: Int = 7
) extends HardwareConfig
