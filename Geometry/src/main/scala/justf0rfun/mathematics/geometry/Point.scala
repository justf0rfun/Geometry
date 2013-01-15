package justf0rfun.mathematics.geometry

import scala.math._

class Point(val x: Double, val y: Double) {

  def distance(point: Point) = sqrt(pow(point.x - x, 2) + pow(point.y - y, 2))
  
//  def +(point: Point) = new Point(x + point.x, y + point.y)
  
  override def toString = "(%+f, %+f)".format(x, y)
  
}

object Point {
  
  def apply(x: Double, y: Double) = new Point(x, y)
  
}

object Offspring extends Point(0, 0)