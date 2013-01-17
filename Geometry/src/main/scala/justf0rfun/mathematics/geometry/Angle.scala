package justf0rfun.mathematics.geometry

import scala.math._

class Angle(val radian: Double) extends AnyVal {

	def degree = toDegrees(radian)

	override def toString = f"$degree%f°"

	def inverse = this + Angle.degree180

//	def +(angle: Angle) = Angle.createByDegree(abs((degree + angle.degree)) % 360)
//	
//	def -(angle: Angle) = Angle.createByDegree(360 - abs((degree - angle.degree)))
	
	def +(angle: Angle) = new Angle(radian + angle.radian) 
	
	def -(angle: Angle) = new Angle(radian - angle.radian)
	
	def sin = scala.math.sin(radian)

	def cos = scala.math.cos(radian)

	def tan = scala.math.tan(radian)

	def asin = scala.math.asin(radian)

	def acos = scala.math.acos(radian)

	def atan = scala.math.atan(radian)

}

object Angle {

	lazy val degree45 = new Angle(toRadians(45))
	lazy val degree90 = new Angle(toRadians(90))
	lazy val degree180 = new Angle(toRadians(180))
	lazy val degree360 = new Angle(toRadians(360))

	def createByDegree(degree: Double): Angle = degree match {
		case 45 => degree45
		case 90 => degree90
		case 180 => degree180
		case 360 => degree360
		case _ => new Angle(toRadians(degree))
	}

}