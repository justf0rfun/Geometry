package justf0rfun.mathematics.geometry.linear
import justf0rfun.mathematics.geometry.Angle
import scala.math.cos
import scala.math.sin
import scala.math.atan2
import scala.math.min
import scala.math.max
import scala.math.sqrt
import scala.math.pow
import justf0rfun.mathematics.geometry.Point

//class Vector(val angle: Angle, val distance: Double) {
class Vector(val width: Double, val height: Double) {

//	def this(pointA: Point, pointB: Point) = this(max(pointA.x, pointB.x) - min(pointA.x, pointB.x), max(pointA.y, pointB.y) - min(pointA.y, pointB.y))
	
	def angle = new Angle(atan2(height, width))
	
//	def angle(vector: Vector) = new Angle(arcos(this * vector))

//	def width = distance * cos(angle.radian)
//
//	def height = distance * sin(angle.radian)

	def +(vector: Vector) = new Vector(width + vector.width, height + vector.height)
	
	def -(vector: Vector) = new Vector(width - vector.width, height - vector.height)
	
	def *(vector: Vector) = new Vector(width * vector.width, height * vector.height)
	
	def *(scalar: Double) = new Vector(width * scalar, height * scalar)
	
	def inverse = new Vector(width * -1, height * -1)
	
	def gradient = height / width
	
	def point(referencePoint: Point) = new Point(referencePoint.x + width, referencePoint.y + height)
	
	def unitVector = if(distance == 1) this else Vector.createPolarVector(angle, 1)
	
	def distance = sqrt(pow(width, 2) + pow(height, 2))
	
	override def toString = "Vector(%f, %f)".format(width, height)

}

object Vector {
	
	lazy val zeroVector = new Vector(0, 0)
	
	def createPolarVector(angle: Angle, distance: Double) = new Vector(distance * cos(angle.radian), distance * sin(angle.radian))
	
}