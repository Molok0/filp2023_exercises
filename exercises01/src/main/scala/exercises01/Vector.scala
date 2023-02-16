package exercises01

import scala.math.sqrt
import scala.math.pow

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(x + other.x, y + other.y);

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y);

  def *(scalar: Double): Vector = new Vector(x * scalar, y * scalar);

  def unary_- : Vector = new Vector(-x, -y);

  def euclideanLength: Double = sqrt(pow(x, 2) + pow(y, 2));

  def normalized: Vector =
    if (euclideanLength == 0) (new Vector(0, 0)) else (new Vector(x / euclideanLength, y / euclideanLength))

  override def equals(other: Any): Boolean = other.equals(x, y)

  // Vector(x, y)
  override def toString: String = "Vector(" + x + ", " + y + ")";
}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = new Vector(length * Math.cos(angle), length * Math.sin(angle))

  def sum(list: List[Vector]): Vector = {
    var a = new Vector(0, 0);
    for (i <- list) {
      a = i + a
    }
    a
  }
  def unapply(arg: Vector): Option[(Double, Double)] = Some(arg.x, arg.y)
}
