package manticore.compiler.assembly.levels.codegen

/** Folded double-cut torus mapping between a global ring coordinate and the
  * (chip, chip-local-slot) position the RTL physically places it at.
  *
  * Each `ManticoreFlatArray` cuts every ring TWICE (the East/North boundary owns
  * the MIDDLE cut at col `h-1 | h` with `h = w/2`; the West/South boundary owns
  * the WRAP cut at col `w-1 | 0`). Chaining `n` chips folds the global ring
  * through them: the out-chain (local slots `0..h-1`) ascends, the far end
  * U-turns, and the back-chain (local slots `h..w-1`) descends. So chip `k` of an
  * `n`-chip dimension of width `w` owns the global ring positions `{k*h .. k*h+h-1}`
  * (out-chain) plus the matching back-chain run — NOT a contiguous block. A lone
  * chip (`n == 1`, all edges `extend=false`) folds to the identity.
  *
  * This is why a CONTIGUOUS per-chip split (`g / w`) misroutes on a multi-chip
  * (`extend=true`) build: a core's global ring coordinate is not the chip it would
  * land in under integer division. [[CodeDump]] uses [[chip]] for the per-chip
  * partition and [[MachineCodeGenerator.makeBinaryStream]] uses [[local]]/[[localHop]]
  * to address each core's chip-local slot during the standalone (extend=false) boot.
  * The mapping reproduces the RTL wiring (`ManticoreFlatArray` / `TorusBoundary`)
  * and the working seam latencies exactly.
  */
object Fold {

  /** Half-width of an EVEN chip dimension. The double-cut fold cuts each ring into
    * two `w/2` halves, so `w` must be even (and the RTL requires even chip dims
    * anyway) — an odd width is rejected outright rather than silently mishandled.
    */
  private def half(w: Int): Int = {
    require(w % 2 == 0, s"Fold: chip dimension must be even (the RTL double-cut requires even dims), got w=$w")
    w / 2
  }

  /** Which chip (`0..n-1`) of an `n`-chip dimension of width `w` owns global ring
    * position `g`. A single-chip dimension (`n <= 1`) does not fold — it owns the
    * whole dimension contiguously — so it imposes no even-width requirement.
    */
  def chip(g: Int, n: Int, w: Int): Int =
    if (n <= 1) 0
    else {
      val h = half(w)
      if (g < n * h) g / h
      else n - 1 - (g - n * h) / h
    }

  /** The chip-local slot (`0..w-1`) that global ring position `g` maps to within
    * its chip.
    */
  def local(g: Int, n: Int, w: Int): Int =
    if (n <= 1) g
    else {
      val h = half(w)
      if (g < n * h) g % h
      else h + ((g - n * h) % h)
    }

  /** Inverse of [[chip]]/[[local]]: the global ring position of chip-local slot
    * `loc` in chip `c`.
    */
  def ring(c: Int, loc: Int, n: Int, w: Int): Int =
    if (n <= 1) loc
    else {
      val h = half(w)
      if (loc < h) c * h + loc
      else n * h + (n - 1 - c) * h + (loc - h)
    }

  /** Minimal signed hop from a chip's bootloader (local slot 0) to local slot
    * `loc`, routed on the standalone (extend=false) width-`w` chip torus. Ties to
    * the forward (+) direction, matching the global hop convention.
    */
  def localHop(loc: Int, w: Int): Int =
    if (loc * 2 <= w) loc else loc - w
}
