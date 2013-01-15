package justf0rfun.mathematics.geometry.circular
import scala.math._
import justf0rfun.mathematics.geometry.Point
import justf0rfun.mathematics.geometry.linear.Line
import justf0rfun.mathematics.geometry.linear.LineSegment
import justf0rfun.mathematics.geometry.Offspring

class Circle(val radius: Double, val location: Point) {

  def this(radius: Double) = {
    this(radius, Offspring)
  }

  def circumference = 2 * Pi * radius

  def area = scala.math.pow(Pi * radius, 2)

  def diameter = 2 * radius

  def intersectionPoints(line: Line): Seq[Point] = {
    val x1 = (-2 * line.offsetY * line.gradient + sqrt(4 * pow(line.offsetY * line.gradient, 2) - 4 * (1 + pow(line.gradient, 2)) * (pow(line.offsetY, 2) - pow(radius, 2)))) / (2 * (1 + pow(line.gradient, 2)))
    val x2 = (-2 * line.offsetY * line.gradient - sqrt(4 * pow(line.offsetY * line.gradient, 2) - 4 * (1 + pow(line.gradient, 2)) * (pow(line.offsetY, 2) - pow(radius, 2)))) / (2 * (1 + pow(line.gradient, 2)))
    var intersectionPoints = List[Point]()
    if(x1 != Double.NaN) new Point(x1, line.y(x1)) :: intersectionPoints
    if(x1 != Double.NaN) new Point(x2, line.y(x2)) :: intersectionPoints
    intersectionPoints
  }
  
//  def intersections(lineSegment: LineSegment): Seq[Point] = {
//  	var intersections: Seq[Point] = intersectionPoints(lineSegment.line)
//  	intersections = intersections.filter(intersectionPoint => lineSegment.isXBetweenAandB(intersectionPoint.x) && lineSegment.isXBetweenAandB(intersectionPoint.y))
//  	intersections
//  }

}