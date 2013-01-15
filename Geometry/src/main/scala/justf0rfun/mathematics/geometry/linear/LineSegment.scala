package justf0rfun.mathematics.geometry.linear
import scala.math._
import justf0rfun.mathematics.geometry.Point
import justf0rfun.mathematics.geometry.Angle

class LineSegment(val pointA: Point, val pointB: Point) {

	def this(pointA: Point, vector: Vector) = {
//		this(pointA, new Point(pointA.x + cos(angle.radian) * distance, pointA.y + sin(angle.radian) * distance))
		this(pointA, vector.point(pointA))
	}

	def distance = pointA.distance(pointB)

//	def gradient = height / width

	def angle = vector.angle
//	def angle = new Angle(atan2(height, width))

//	def offset = pointA.y - pointB.y
//	def offset = line.y(0)
	
	def vector = new Vector(pointB.x - pointA.x, pointB.y - pointA.y)

//	def line = new Line(gradient, new Line(gradient, 0).y(pointA.x) - pointA.y)
//	def line = new Line(vector.gradient, pointA.y - vector.gradient * pointA.x)

	def width = pointB.x - pointA.x

	def height = pointB.y - pointA.y

	def middle = new Point((pointA.x + pointB.x) / 2, (pointA.y + pointB.y) / 2)

	def intersectionPoint(lineSegment: LineSegment): Option[Point] = {
		if (!isVertical && lineSegment.isVertical) {
			return LineSegment.intersectionPointWithVertical(this, lineSegment)
		} else 	if (isVertical && !lineSegment.isVertical) {
			return LineSegment.intersectionPointWithVertical(lineSegment, this)
		} else 	if (isVertical && lineSegment.isVertical) {
			if (pointA.x == lineSegment.pointA.x) {
				if (isYBetweenAandB(lineSegment.pointA.y) || isYBetweenAandB(lineSegment.pointB.y)) {
					//TODO number of intersection points == infitiy because both segments are overlapping. Find the right result type to express this
					return Some(lineSegment.pointB)
				}
			}
			return None
		} else {
			new Line(this).intersectionPoint(lineSegment) match {
			case Some(intersectionPoint) if (isXBetweenAandB(intersectionPoint.x)) => Some(intersectionPoint)
			case _ => return None
			}
		}
	}

	def isXBetweenAandB(x: Double) = (minX <= x && x <= maxX)

	def isYBetweenAandB(y: Double) = (minY <= y && y <= maxY)

	def isHorizontal = pointA.y == pointB.y

	def isVertical = pointA.x == pointB.x

	def minX = min(pointA.x, pointB.x)

	def minY = min(pointA.y, pointB.y)

	def maxX = max(pointA.x, pointB.x)

	def maxY = max(pointA.y, pointB.y)
	
	override def toString = "LineSegment(%s, %s)".format(pointA, pointB)

}

object LineSegment {

	def intersectionPointWithVertical(lineSegment: LineSegment, verticalLineSegment: LineSegment): Option[Point] = {
		//		val y = 1 / ((verticalLineSegment.width / verticalLineSegment.height) / verticalLineSegment.minX)
		if (lineSegment.isXBetweenAandB(verticalLineSegment.pointA.x)) {
			return new Line(lineSegment).y(lineSegment.pointA.x) match {
				case y if (verticalLineSegment.isYBetweenAandB(y)) => Some(new Point(verticalLineSegment.pointA.x, y))
				case _ => None
			}
		}
		return None
	}

}