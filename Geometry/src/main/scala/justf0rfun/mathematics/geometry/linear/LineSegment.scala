package justf0rfun.mathematics.geometry.linear
import scala.math._
import justf0rfun.mathematics.geometry.Point

class LineSegment(val pointA: Point, val pointB: Point) {

  def this(pointA: Point, angle: Double, distance: Double) = {
    this(pointA, new Point(acos(angle) * distance, asin(angle) * distance))
  }
  
  def distance = pointA.distance(pointB)

  def gradient = height / width

  def angle = Math.tan(distance)
  
  def offset = pointA.y - pointB.y
  
  def line = new Line(gradient, new Line(gradient, 0).y(pointA.x) - pointA.y)
  
  def width = pointA.x - pointB.x
  
  def height = pointA.y - pointB.y
  
}