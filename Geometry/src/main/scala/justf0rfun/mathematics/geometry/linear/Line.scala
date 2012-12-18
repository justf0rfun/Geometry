package justf0rfun.mathematics.geometry.linear
import justf0rfun.mathematics.geometry.Point

class Line(val gradient: Double, val offsetY: Double) {

  def y(x: Double) = gradient * x + offsetY

  def x(y: Double) = (y - offsetY) / gradient

  def point(x: Double) = new Point(x, y(x))

  def intersection(line: Line) = (line.offsetY - offsetY) / (gradient - line.gradient)

  def intersectionPoint(line: Line): Option[Point] = {
    val x = intersection(line)
    new Some(new Point(x, y(x)))
  }

  def intersectionPoint(lineSegment: LineSegment): Option[Point] = {
    intersectionPoint(lineSegment.line) match {
      case Some(intersectionPoint) if (Math.min(lineSegment.pointA.x, lineSegment.pointB.x) <= intersectionPoint.x && intersectionPoint.x <= Math.max(lineSegment.pointA.x, lineSegment.pointB.x)) => {
        {
          new Some(intersectionPoint)
        }
      }
      case None => None
    }
  }
  
  def lineSegment(x0: Double, x1: Double) = new LineSegment(new Point(x0, y(x0)), new Point(x1, y(x1)))

}