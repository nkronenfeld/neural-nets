package ndk.math.linalg

import ndk.math.Gaussian

import scala.util.Random

// A matrix, a listing of C columns of length R (i.e., column-major order
class Matrix (val C: Int, val R: Int, val values: Array[Double]) {
  assert(R * C == values.length)
  def apply (c: Int, r: Int): Double = {
    assert(0 <= c && c < C)
    assert(0 <= r && r < R)
    values(c * R + r)
  }

  def update(c: Int, r: Int, v: Double): Unit = {
    assert(0 <= c && c < C)
    assert(0 <= r && r < R)
    values(c * R + r) = v
  }

  def + (that: Matrix): Matrix = {
    assert(this.C == that.C)
    assert(this.R == that.R)
    new Matrix(C, R,
      (this.values zip that.values).map { case (a, b) => a + b })
  }

  // Add the given matrix to this one, changing this one
  def addInPlace (that: Matrix): Matrix = {
    assert(this.C == that.C)
    assert(this.R == that.R)
    for (i <- 0 until C * R) {
      this.values(i) = this.values(i) + that.values(i)
    }
    this
  }

  def * (that: Matrix): Matrix = {
    assert(this.C == that.R)
    new Matrix(that.C, this.R,
      (for (r <- 0 until this.R; c <- 0 until that.C) yield {
        (0 until this.C).map(n => this(n, r) * that(c, n)).sum
      }).toArray)
  }

  def * (that: Vector): Vector = {
    assert(this.C == that.length)
    new Vector((for (r <- 0 until this.R) yield {
      (0 until this.C).map(n => this(n, r) * that(n)).sum
    }).toArray)
  }

  def * (that: Double): Matrix = {
    new Matrix(C, R, values.map(_ * that))
  }

  def multiplyInPlace (that: Double): Matrix = {
    for (i <- values.indices) values(i) = values(i) * that
    this
  }

  def transpose (): Matrix = {
    new Matrix(R, C,
      values.sliding(R, R).toArray.transpose.reduce(_ ++ _)
    )
  }

  override def equals (other: Any): Boolean = other match {
    case that: Matrix =>
      this.C == that.C &&
        this.R == that.R &&
        (0 until (C * R)).map(n => this.values(n) == that.values(n)).reduce(_ && _)
    case _ => false
  }

  override def toString: String = s"Matrix[C=${C} x R=${R}]"
}

object Matrix {
  def fromArrays (columns: Array[Double]*): Matrix = {
    val C = columns.length
    val R = columns(0).length
    columns.foreach(c => assert(c.length == R))

    new Matrix(C, R, columns.reduce(_ ++ _))
  }
  def fromVectors (columns: Vector*): Matrix = {
    val C = columns.length
    val R = columns(0).length
    columns.foreach(c => assert(c.length == R))

    new Matrix(C, R, columns.map(_.values).reduce(_ ++ _))
  }

  def random (c: Int, r: Int, random: Random = new Random()): Matrix = {
    new Matrix(c, r, Array.fill(c * r)(Gaussian.random(random)))
  }

  def zero (c: Int, r: Int): Matrix = {
    new Matrix(c, r, Array.fill(c * r)(0.0))
  }
}
