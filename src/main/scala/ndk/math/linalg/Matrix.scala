package ndk.math.linalg

import ndk.math.Gaussian

import scala.util.Random

// A matrix, a listing of C columns of length R (i.e., column-major order
class Matrix (val C: Int, val R: Int, val values: Array[Double]) {
  assert(R * C == values.length)
  def row (r: Int): Vector =
    new Vector(values.grouped(R).map(_(r)).toArray)

  def column(c: Int): Vector =
    new Vector(values.slice(c * R, (c + 1) * R))

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
  def approximatelyEquals (epsilon: Double)(other: Any): Boolean = other match {
    case that: Matrix =>
      this.C == that.C && this.R == that.R &&
        (0 until (C * R)).map { n =>
          val a = this.values(n)
          val b = that.values(n)
          if (a.abs < epsilon) b.abs < epsilon
          else (1.0 - epsilon < a / b && a / b < 1.0 + epsilon)
        }.reduce(_ && _)
    case _ => false
  }

  override def toString: String = s"Matrix[C=${C} x R=${R}]: " + toStringRows.mkString("")
  def toStringRows: Seq[String] =
    (0 until R).map(r => row(r).values.map(n => f"${n}%2.2f").mkString("| ", " ", " |"))
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

  def one (n: Int): Matrix = {
    val rowPlusOne = 1.0 +: Array.fill(n)(0.0)
    new Matrix(n, n, Array.fill(n-1)(rowPlusOne).flatten :+ 1.0)
  }

  def invertSquareMatrix (M: Matrix): Matrix = {
    assert(M.C == M.R)
    val N = M.C
    val epsilon = 1E-6

    // Get the MI pair on which to perform the inversion
    val inversionSpace = new Array[Array[Double]](N)
    inversionSpace.indices.foreach { n =>
      inversionSpace(n) = M.row(n).values ++ Array.fill(N)(0.0)
      inversionSpace(n)(N + n) = 1.0
    }

    def swapRows (a: Int, b: Int): Unit = {
      val tmp = inversionSpace(a)
      inversionSpace(a) = inversionSpace(b)
      inversionSpace(b) = tmp
    }
    def scaleRow (a: Int, scale: Double): Unit = {
      (0 until N*2).foreach(n => inversionSpace(a)(n) = inversionSpace(a)(n) * scale)
    }
    def addScaledRow (a: Int, b: Int, scale: Double): Unit = {
      (0 until N*2).foreach(n =>
        inversionSpace(a)(n) = inversionSpace(a)(n) + inversionSpace(b)(n) * scale
      )
    }

    (0 until N).foreach { n =>
      // Find a row with a non-0 nth entry
      val r = (n until N).find(nn => inversionSpace(nn)(n).abs > epsilon)
      // Swap this row to the right position
      r.map(rr => if (n != rr) swapRows(n, rr)).orElse(throw new Exception("Uninvertable matrix"))
      // Scale it so that the nth entry is 1.0
      scaleRow(n, 1.0 / inversionSpace(n)(n))
      // Remove the nth entry from all other rows
      (0 until N).map { nn =>
        if (n != nn) {
          addScaledRow(nn, n, -inversionSpace(nn)(n))
        }
      }
    }

    new Matrix(N, N, inversionSpace.map(_.drop(N)).reduce(_ ++ _)).transpose()
  }
}
