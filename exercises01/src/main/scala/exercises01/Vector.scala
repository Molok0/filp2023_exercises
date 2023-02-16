package exercises01

import scala.Byte.MaxValue.^
import scala.math.sqrt
import scala.math.pow

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(x + other.x, y + other.y);

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y);

  def *(scalar: Double): Vector = new Vector(x + scalar, y + scalar);

  def unary_- : Vector = new Vector(-x, -y);

  def euclideanLength: Double = sqrt(pow(x,2) + pow(y,2));

  def normalized: Vector = ???

  //override def equals(other: Any): Boolean = this.toString == other.toString;

  // Vector(x, y)
  //override def toString: String = "Vector(" + x + ", "+ y + ")";
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = ???

  def sum(list: List[Vector]): Vector = ???

  def unapply(arg: Vector): Option[(Double, Double)] = ???
}
