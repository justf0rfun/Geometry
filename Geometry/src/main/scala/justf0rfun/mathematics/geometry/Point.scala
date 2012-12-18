package justf0rfun.mathematics.geometry

class Point(val x: Double, val y: Double) {

  def distance(point: Point) = Math.sqrt(Math.pow(x - point.x, 2) + Math.pow(y - point.y, 2))
  
}

object Point {
  
  def apply(x: Double, y: Double) = new Point(x, y)
  
}