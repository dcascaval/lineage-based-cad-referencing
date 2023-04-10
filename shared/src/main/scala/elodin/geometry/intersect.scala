package elodin.geometry

object IntersectionResult:
  def unapply(ir: IntersectionResult) = (ir.point, ir.paramA, ir.paramB)

class IntersectionResult(val point: Point, var paramA: Double, var paramB: Double):
  def flip() =
    val temp = paramA; paramA = paramB; paramB = temp; this
  override def toString() =
    s"Intersection($point, $paramA, $paramB)"
