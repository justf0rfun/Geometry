package justf0rfun.mathematics.geometry.circular
import scala.math._
import justf0rfun.mathematics.geometry.Point
import justf0rfun.mathematics.geometry.linear.Line

class Circle(val radius: Double, val location: Point) {

  def this(radius: Double) = {
    this(radius, new Point(0, 0))
  }

  def circumference = 2 * Pi * radius

  def area = scala.math.pow(Pi * radius, 2)

  def diameter = radius * 2

  def intersections(line: Line): (Point, Point) = {
    val x1 = (-2 * line.offsetY * line.gradient + sqrt(4 * pow(line.offsetY * line.gradient, 2) - 4 * (1 + pow(line.gradient, 2)) * (pow(line.offsetY, 2) - pow(radius, 2)))) / (2 * (1 + pow(line.gradient, 2)))
    val x2 = (-2 * line.offsetY * line.gradient - sqrt(4 * pow(line.offsetY * line.gradient, 2) - 4 * (1 + pow(line.gradient, 2)) * (pow(line.offsetY, 2) - pow(radius, 2)))) / (2 * (1 + pow(line.gradient, 2)))
    (new Point(x1, line.y(x1)), new Point(x2, line.y(x2)))
  }

}