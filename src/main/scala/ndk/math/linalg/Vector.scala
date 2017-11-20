package ndk.math.linalg

import ndk.math.Gaussian

import scala.util.Random

class Vector (val values: Array[Double]) {
  val length = values.length;

  def apply(n: Int): Double = {
    values(n)
  }

  def update(n: Int, v: Double): Unit = {
    values(n) = v
  }

  def +(that: Vector): Vector = {
    assert(this.values.length == that.values.length)
    new Vector((this.values zip that.values).map { case (a, b) => a + b })
  }

  // Add the given vector to this one changing this one
  def addInPlace (that: Vector): Vector = {
    assert(this.values.length == that.values.length)
    for (i <- this.values.indices) {
      this.values(i) = this.values(i) + that.values(i)
    }
    this
  }

  def -(that: Vector): Vector = {
    assert(this.values.length == that.values.length)
    new Vector((this.values zip that.values).map { case (a, b) => a - b })
  }

  def * (that: Vector): Vector = {
    assert(this.values.length == that.values.length)
    new Vector((this.values zip that.values).map { case (a, b) => a * b })
  }

  def dot (that: Vector): Double = {
    assert(this.values.length == that.values.length)
    (this.values zip that.values).map { case (a, b) => a * b }.sum
  }

  def *(that: Double): Vector = {
    new Vector(this.values.map(_ * that))
  }

  def multiplyInPlace (that: Double): Vector = {
    for (i <- values.indices) values(i) = values(i) * that
    this
  }

  def map (fcn: Double => Double): Vector = {
    new Vector(this.values.map(fcn))
  }

  override def equals (other: Any): Boolean = other match {
    case that: Vector => Vector.vectorsEqual(1E-12)(this, that)
    case _ => false
  }
  override def toString: String = values.mkString("[", ", ", "]")
}

object Vector {
  def apply (values: Double*): Vector = new Vector(values.toArray)
  def vectorsEqual (tolerance: Double)(a: Vector, b: Vector): Boolean = {
    val c = a - b

    (c dot c) < (tolerance * tolerance)
  }

  def random (length: Int, random: Random = new Random()): Vector = {
    new Vector(Array.fill(length)(Gaussian.random(random)))
  }

  def zero (length: Int): Vector = {
    new Vector(Array.fill(length)(0.0))
  }
}
