package exercises01

import exercises01.Vector.zeroVector

import scala.math.sqrt
import scala.math.pow

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(x + other.x, y + other.y);

  def -(other: Vector): Vector = new Vector(x - other.x, y - other.y);

  def *(scalar: Double): Vector = new Vector(x * scalar, y * scalar);

  def unary_- : Vector = new Vector(-x, -y);

  def euclideanLength: Double = sqrt(pow(x, 2) + pow(y, 2));

  def normalized: Vector = {
    val vectorLen = euclideanLength
    if (vectorLen == 0) (zeroVector) else (new Vector(x / vectorLen, y / vectorLen))
  }

  override def equals(other: Any): Boolean = other match {
    case Vector(this.x, this.y) => true
    case _                      => false
  };

  // Vector(x, y)
  override def toString: String = s"Vector($x, $y)";
}

object Vector {
  val zeroVector                                       = new Vector(0, 0)
  def fromAngle(angle: Double, length: Double): Vector = new Vector(length * Math.cos(angle), length * Math.sin(angle))

  def sum(list: List[Vector]): Vector = list.foldLeft(zeroVector)(_ + _)

  def unapply(arg: Vector): Option[(Double, Double)] = Some(arg.x, arg.y)
}
