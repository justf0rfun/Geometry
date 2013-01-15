package justf0rfun.mathematics.geometry.linear
import scala.math.max
import scala.math.min

import justf0rfun.mathematics.geometry.Point

class Line(val gradient: Double, val offsetY: Double) {

	def this(lineSegment: LineSegment) = this(lineSegment.vector.gradient, lineSegment.pointA.y - lineSegment.vector.gradient * lineSegment.pointA.x)
	
	def this(vector: Vector, referencePoint: Point) = this(new LineSegment(referencePoint, vector))
	
	def this(vector: Vector, offsetY: Double) = this(vector.gradient, offsetY)

	def y(x: Double) = gradient * x + offsetY

	def x(y: Double) = (y - offsetY) / gradient

	def point(x: Double) = new Point(x, y(x))

	def intersection(line: Line) = (line.offsetY - offsetY) / (gradient - line.gradient)

	def intersectionPoint(line: Line): Option[Point] = {
		val x = intersection(line)
		//TODO is this correct? if no intersection None is right but with identical lines there are infinity intersection points and not None
		if (x == Double.NaN) {
			return None
		}
		return new Some(new Point(x, y(x)))
	}

	def intersectionPoint(lineSegment: LineSegment): Option[Point] = {
		//		println(intersectionPoint(lineSegment.line))
		intersectionPoint(new Line(lineSegment)) match {
			case Some(intersectionPoint) if (min(lineSegment.pointA.x, lineSegment.pointB.x) <= intersectionPoint.x && intersectionPoint.x <= max(lineSegment.pointA.x, lineSegment.pointB.x)) => new Some(intersectionPoint)
			case _ => None
		}
	}

	def lineSegment(x0: Double, x1: Double) = new LineSegment(new Point(x0, y(x0)), new Point(x1, y(x1)))

}