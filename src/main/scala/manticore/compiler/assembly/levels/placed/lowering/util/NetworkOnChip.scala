package manticore.compiler.assembly.levels.placed.lowering.util
import manticore.compiler.assembly.levels.placed.PlacedIR._
import java.io.PrintWriter
import manticore.compiler.AssemblyContext
import manticore.compiler.HardwareConfig

private[lowering] case class RecvEvent(recv: Recv, cycle: Int)

/**
  * Modeling the bidirectional Manticore network on chip.
  *
  * xHops/yHops from HardwareConfig are signed:
  *   positive = forward (+X / +Y),  negative = backward (-X / -Y).
  *
  * Link-occupancy arrays are split by direction so that a +X and a -X packet
  * can share the same switch in the same cycle without collision (they use
  * independent physical channels).
  *
  *   linksXPos(x)(y)  eastbound  link entering switch (x, y)
  *   linksXNeg(x)(y)  westbound  link entering switch (x, y)
  *   linksYPos(x)(y)  northbound link entering switch (x, y)  — also models
  *                    terminal delivery (the shared y_reg in the Switch)
  *   linksYNeg(x)(y)  southbound link entering switch (x, y)
  *
  * @author Mahyar emami <mahyar.emami@epfl.ch>  (original)
  *         extended for bidirectional routing
  */
private[lowering] class NetworkOnChip(val cfg: HardwareConfig) {

  sealed abstract trait Response
  case object Denied               extends Response
  case class Granted(arrival: Int) extends Response

  case class Step(loc: Int, t: Int)

  class Path private[NetworkOnChip] (
      val from: ProcessId,
      val send: Send,
      val scheduleCycle: Int
  ) {
    val to    = send.dest_id
    val xDist = cfg.xHops(from, to)   // signed
    val yDist = cfg.yHops(from, to)   // signed

    /* The packet is enqueued to the NoC after cfg.decodeLatency + cfg.sendPipes cycles,
     * plus 1 for the register at the switch input.
     */
    val enqueueTime: Int = cfg.decodeLatency + cfg.sendPipes + scheduleCycle + 1

    private def mod(v: Int, m: Int): Int = ((v % m) + m) % m

    // Sequence of switches visited while routing in the X dimension.
    // Steps occupy linksXPos (xDist > 0) or linksXNeg (xDist < 0).
    // Each step's time advances by the (per-directed-link) hop latency: 1 for a plain
    // on-chip hop, more for a configured slow link (e.g. an inter-chip crossing). With
    // no hopLatencies configured this reduces exactly to the previous +1-per-hop model.
    val xHops: Seq[Step] = {
      val east = xDist >= 0
      var t    = enqueueTime - 1 // the source switch's channel register
      Seq.tabulate(math.abs(xDist)) { i =>
        val fromX = mod(from.x + (if (east) i else -i), cfg.dimX)
        val toX   = mod(from.x + (if (east) i + 1 else -(i + 1)), cfg.dimX)
        t += cfg.xLinkLatency(fromX, from.y, east)
        Step(toX, t)
      }
    }

    // Time at which the packet leaves the last X hop (or enqueueTime if no X hops).
    private val xDoneTime: Int = if (xHops.nonEmpty) xHops.last.t else enqueueTime - 1

    // Sequence of switches visited while routing in the Y dimension (at column to.x).
    // The LAST entry (lastHop) models the y_reg write in the destination switch;
    // it always occupies linksYPos because terminal delivery uses yOutput regardless
    // of the arrival direction.
    val yHops: Seq[Step] = {
      val north = yDist >= 0
      val transit: Seq[Step] = {
        var t = xDoneTime
        Seq.tabulate(math.abs(yDist)) { i =>
          val fromY = mod(from.y + (if (north) i else -i), cfg.dimY)
          val toY   = mod(from.y + (if (north) i + 1 else -(i + 1)), cfg.dimY)
          t += cfg.yLinkLatency(to.x, fromY, north)
          Step(toY, t)
        }
      }

      // lastHop: the destination switch's y_reg at delivery time.
      // For northbound: the slot is at (to.y + 1) % dimY.
      // For southbound: delivery also goes through y_reg → same model, so
      // lastHop location is (to.y + 1) % dimY regardless of direction.
      val lastHopTime = if (transit.nonEmpty) transit.last.t + 1 else xDoneTime + 1
      assert(
        xHops.nonEmpty || yDist != 0,
        s"Can not have self messages: send ${send.serialized} from $from with xDist=$xDist yDist=$yDist"
      )
      transit :+ Step((to.y + 1) % cfg.dimY, lastHopTime)
    }
  }

  private type LinkOccupancy = scala.collection.mutable.Set[Int]
  private val linksXPos = Array.ofDim[LinkOccupancy](cfg.dimX, cfg.dimY)
  private val linksXNeg = Array.ofDim[LinkOccupancy](cfg.dimX, cfg.dimY)
  private val linksYPos = Array.ofDim[LinkOccupancy](cfg.dimX, cfg.dimY)
  private val linksYNeg = Array.ofDim[LinkOccupancy](cfg.dimX, cfg.dimY)

  private val usedPaths = scala.collection.mutable.ArrayBuffer.empty[Path]

  for (x <- 0 until cfg.dimX; y <- 0 until cfg.dimY) {
    linksXPos(x)(y) = scala.collection.mutable.Set.empty[Int]
    linksXNeg(x)(y) = scala.collection.mutable.Set.empty[Int]
    linksYPos(x)(y) = scala.collection.mutable.Set.empty[Int]
    linksYNeg(x)(y) = scala.collection.mutable.Set.empty[Int]
  }

  def draw(): String = {
    def renderLine(y: Int): String = {
      val ln = new StringBuilder
      ln ++= "\n"
      for (x <- 0 until cfg.dimX) {
        ln ++= f"${"|"}%12s"
      }
      ln ++= "\n"
      for (x <- 0 until cfg.dimX) {
        ln ++= f"    ${linksYPos(x)(y).size}%8d"
      }
      ln ++= "\n"
      for (x <- 0 until cfg.dimX) {
        ln ++= f"${"v"}%12s"
      }
      ln ++= "\n"
      for (x <- 0 until cfg.dimX) {
        ln ++= f"${linksXPos(x)(y).size}%7d->[ ]".replace(" ", "-")
      }
      ln ++= "\n"
      ln.toString()
    }
    val str = new StringBuilder
    str ++= "\n"
    for (y <- 0 until cfg.dimY) { str ++= renderLine(y) }
    str.toString()
  }

  def getPaths(): Iterable[Path] = usedPaths

  /** Try to reserve the links for a Send scheduled at scheduleCycle.
    * Returns Some(path) if all required links are free, None otherwise.
    */
  def tryReserve(from: ProcessId, send: Send, scheduleCycle: Int): Option[Path] = {
    val path = new Path(from, send, scheduleCycle)
    val xLinks = if (path.xDist >= 0) linksXPos else linksXNeg
    val canRouteX = path.xHops.forall { case Step(x, t) =>
      !xLinks(x)(from.y).contains(t)
    }
    // Y routing uses the directional channel for in-transit hops and the shared y_reg
    // (always linksYPos) for the terminal delivery:
    //   yTransit  — the in-transit Y hops. Northbound (yDist>=0) occupy linksYPos,
    //               southbound (yDist<0) occupy linksYNeg. These are physically distinct
    //               channels (y_reg vs y_neg_reg), so they never collide with each other.
    //   yTerm     — the lastHop: the destination switch's y_reg write. Terminal delivery
    //               goes through yOutput/y_reg regardless of arrival direction, so it is
    //               ALWAYS reserved in linksYPos (shared with northbound arrivals).
    val yLinks   = if (path.yDist >= 0) linksYPos else linksYNeg
    val yTransit = path.yHops.init
    val yTerm    = path.yHops.last

    val canRouteYTransit = yTransit.forall { case Step(y, t) =>
      !yLinks(path.to.x)(y).contains(t)
    }
    val canRouteYTerm = !linksYPos(path.to.x)(yTerm.loc).contains(yTerm.t)
    if (canRouteX && canRouteYTransit && canRouteYTerm) Some(path) else None
  }

  /** Reserve the links for the given path and return the RecvEvent. */
  def request(path: Path): RecvEvent = {
    assert(tryReserve(path.from, path.send, path.scheduleCycle).nonEmpty)
    usedPaths += path

    val xLinks = if (path.xDist >= 0) linksXPos else linksXNeg
    for (Step(x, t) <- path.xHops) xLinks(x)(path.from.y) += t

    // Mirror tryReserve: in-transit Y hops occupy the directional channel (linksYPos for
    // northbound, linksYNeg for southbound); the terminal y_reg write is always linksYPos.
    val yLinks   = if (path.yDist >= 0) linksYPos else linksYNeg
    val yTransit = path.yHops.init
    val yTerm    = path.yHops.last

    for (Step(y, t) <- yTransit) yLinks(path.to.x)(y) += t
    linksYPos(path.to.x)(yTerm.loc) += yTerm.t

    RecvEvent(
      Recv(path.send.rd, path.send.rs, path.from),
      path.yHops.last.t + cfg.recvPipes
    )
  }
}

object NetworkOnChip {

  def jsonDump(network: NetworkOnChip): String = {
    val printer = new StringBuilder
    printer ++= (s"{\n\"topology\": [${network.cfg.dimX}, ${network.cfg.dimY}], \n\"paths\": [")
    network.getPaths().foreach { p =>
      val ln = s"\"source\": [${p.from.x}, ${p.from.y}],\n" +
        s"\"cycle\": ${p.scheduleCycle},\n" +
        s"\"xHops\": [${p.xHops.map(_.loc).mkString(", ")}],\n" +
        s"\"yHops\": [${p.yHops.map(_.loc).mkString(", ")}],\n" +
        s"\"target\": [${p.to.x}, ${p.to.y}]\n"
      printer ++= "{"
      printer ++= ln
      if (p != network.getPaths().last)
        printer ++= "},"
      else
        printer ++= "}"
    }
    printer ++= "]}"
    printer.toString()
  }

  /** Human-traceable CSV of every NoC transaction in one virtual cycle, sorted per source
    * node then by schedule time. One row per Send. Columns:
    *   send_x,send_y   : source core
    *   recv_x,recv_y   : destination core
    *   xDist,yDist     : signed hop counts (>0 forward/+X+Y, <0 backward/-X-Y)
    *   scheduleCycle   : cycle the SEND is issued (within the vcycle schedule)
    *   enqueueCycle    : cycle the packet enters the NoC (decodeLatency+sendPipes+1 later)
    *   expectedRecv    : cycle the destination is modeled to receive it (yHops.last + recvPipes)
    *   send_reg/recv_reg : source/destination register names (for cross-checking the .masm)
    * Lets the schedule's per-transaction timing be traced by hand against the RTL.
    */
  def csvDump(network: NetworkOnChip): String = {
    val cfg = network.cfg
    val sb  = new StringBuilder
    sb ++= "send_x,send_y,recv_x,recv_y,xDist,yDist,scheduleCycle,enqueueCycle,expectedRecv,send_reg,recv_reg\n"
    network
      .getPaths()
      .toSeq
      .sortBy(p => (p.from.x, p.from.y, p.scheduleCycle))
      .foreach { p =>
        val expectedRecv = p.yHops.last.t + cfg.recvPipes
        sb ++= s"${p.from.x},${p.from.y},${p.to.x},${p.to.y},${p.xDist},${p.yDist}," +
          s"${p.scheduleCycle},${p.enqueueTime},${expectedRecv},${p.send.rs},${p.send.rd}\n"
      }
    sb.toString()
  }

  def apply(cfg: HardwareConfig) = new NetworkOnChip(cfg)
}
