package ndk.math.linalg

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
}
