package manticore.compiler

import manticore.compiler.assembly.ManticoreAssemblyIR
import manticore.compiler.assembly.levels.placed.PlacedIR

/** Per-directed-link NoC hop latencies, for multi-chip topologies where some links
  * (the chip-to-chip crossings) have long, statically-known latencies.
  *
  * A hop's latency is the number of cycles from occupying one switch's channel
  * register to occupying the next switch's channel register along a directed link:
  * 1 = a plain on-chip hop (the default for every link not listed); an inter-chip
  * crossing routed through K constant-latency pipeline stages is 1+K.
  *
  * IMPORTANT: these latencies affect ONLY the scheduler's travel-time model (when
  * values arrive, where Recvs are placed, link-slot reservations). The packet
  * addressing — signed hop counts and the min-|hops| direction choice — is
  * deliberately unchanged.
  *
  * CSV format (loaded via masm `--hop-latencies <file>`), one directed link per row:
  *   src_x,src_y,dir,latency
  * where dir ∈ {east,west,north,south} names the link FROM (src_x,src_y) toward its
  * neighbour in that direction (with torus wraparound), e.g. on an 8x4 made of two
  * chained 4x4 chips the four eastbound boundary crossings of the first seam are
  * `3,0,east,K` … `3,3,east,K`. Lines starting with `#`, blank lines, and one
  * optional header row are ignored.
  */
case class HopLatencyMap(
    east: Map[(Int, Int), Int],
    west: Map[(Int, Int), Int],
    north: Map[(Int, Int), Int],
    south: Map[(Int, Int), Int]
) {
  def xLink(x: Int, y: Int, eastbound: Boolean): Int =
    (if (eastbound) east else west).getOrElse((x, y), 1)
  def yLink(x: Int, y: Int, northbound: Boolean): Int =
    (if (northbound) north else south).getOrElse((x, y), 1)
}

object HopLatencyMap {

  def fromCsv(text: String, dimX: Int, dimY: Int): Either[String, HopLatencyMap] = {
    val east, west, north, south = scala.collection.mutable.Map.empty[(Int, Int), Int]
    val errors = scala.collection.mutable.ArrayBuffer.empty[String]
    text.linesIterator.zipWithIndex.foreach { case (raw, ix) =>
      val line = raw.trim
      val isHeader = ix == 0 && line.toLowerCase.startsWith("src_x")
      if (line.nonEmpty && !line.startsWith("#") && !isHeader) {
        line.split(",").map(_.trim) match {
          case Array(xs, ys, dir, ls) =>
            (xs.toIntOption, ys.toIntOption, ls.toIntOption) match {
              case (Some(x), Some(y), Some(l)) =>
                if (x < 0 || x >= dimX || y < 0 || y >= dimY)
                  errors += s"line ${ix + 1}: node ($x,$y) outside ${dimX}x${dimY}"
                else if (l < 1)
                  errors += s"line ${ix + 1}: latency $l < 1"
                else
                  dir.toLowerCase match {
                    case "east"  => east((x, y)) = l
                    case "west"  => west((x, y)) = l
                    case "north" => north((x, y)) = l
                    case "south" => south((x, y)) = l
                    case other   => errors += s"line ${ix + 1}: unknown direction '$other' (east|west|north|south)"
                  }
              case _ => errors += s"line ${ix + 1}: malformed numbers in '$line'"
            }
          case _ => errors += s"line ${ix + 1}: expected 'src_x,src_y,dir,latency' but got '$line'"
        }
      }
    }
    if (errors.nonEmpty) Left(errors.mkString("; "))
    else Right(HopLatencyMap(east.toMap, west.toMap, north.toMap, south.toMap))
  }

  def fromCsvFile(path: java.io.File, dimX: Int, dimY: Int): Either[String, HopLatencyMap] =
    scala.util.Try(scala.io.Source.fromFile(path)) match {
      case scala.util.Failure(e) => Left(s"could not read ${path}: ${e.getMessage}")
      case scala.util.Success(src) =>
        try fromCsv(src.mkString, dimX, dimY)
        finally src.close()
    }
}

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

  // ---- per-link travel-time model (multi-chip) -------------------------------------
  // Optional per-directed-link latencies (1 = plain on-chip hop). These affect ONLY
  // how long the scheduler believes a hop takes — packet addressing (the signed hop
  // counts above and their min-|hops| direction choice) is unchanged.
  def hopLatencies: Option[HopLatencyMap] = None

  /** cycles to traverse the directed X link leaving switch (x,y) east- or westward */
  final def xLinkLatency(x: Int, y: Int, eastbound: Boolean): Int =
    hopLatencies.fold(1)(_.xLink(x, y, eastbound))

  /** cycles to traverse the directed Y link leaving switch (x,y) north- or southward */
  final def yLinkLatency(x: Int, y: Int, northbound: Boolean): Int =
    hopLatencies.fold(1)(_.yLink(x, y, northbound))

  private def mod(v: Int, m: Int): Int = ((v % m) + m) % m

  /** total link latency of the X leg of the (dimension-ordered) route, at row source.y */
  final def xPathLatency(source: ProcessId, target: ProcessId): Int = {
    val n    = xHops(source, target) // signed count — the route addressing actually uses
    val east = n >= 0
    (0 until math.abs(n)).map { i =>
      xLinkLatency(mod(source.x + (if (east) i else -i), dimX), source.y, east)
    }.sum
  }

  /** total link latency of the Y leg, at column target.x (X routes first) */
  final def yPathLatency(source: ProcessId, target: ProcessId): Int = {
    val n     = yHops(source, target)
    val north = n >= 0
    (0 until math.abs(n)).map { i =>
      yLinkLatency(target.x, mod(source.y + (if (north) i else -i), dimY), north)
    }.sum
  }

  def manhattan(source: ProcessId, target: ProcessId): Int = {
    // Travel-time of the dimension-ordered route. With no hopLatencies configured every
    // link costs 1 and this equals |xHops| + |yHops| (the previous definition).
    xPathLatency(source, target) + yPathLatency(source, target)
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

    sendPipes: Int = 7,
    override val hopLatencies: Option[HopLatencyMap] = None
) extends HardwareConfig
