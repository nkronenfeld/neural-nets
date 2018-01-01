package ndk.math.linalg

import scala.collection.TraversableLike
import scala.collection.generic.CanBuildFrom

object GradientDescentUtilities {
  private val epsilon = 1.0E-32

  /**
    * Normalize a set of data.
    *
    * Normalized here is taken to mean, for each entry, the number of standard deviations from the
    * mean of that entry
    *
    * @param data A bunch of N-vectors (for constant N)
    * @return (The normalized data, a vector of means (by entry), a vector of standard deviations
    *         (by entry))
    */
  def scaleByStandardDeviation[S1, S2] (data: TraversableLike[Vector, S1])
                                       (implicit bf: CanBuildFrom[S1, Vector, S2]): (S2, Vector, Vector) = {
    // quick in-place sum, no extra allocations
    val m = data.head.length
    var x0 = 0
    var x1 = Array.fill(m)(0.0)
    var x2 = Array.fill(m)(0.0)
    data.foreach { v =>
      assert(m == v.length)
      x0 = x0 + 1
      for (i <- 0 until m) {
        val vi = v(i)
        x1(i) += vi
        x2(i) += vi * vi
      }
    }

    val n = x0.toDouble
    val mu = new Vector(x1.map(_ / n))
    val sigma = new Vector((x1 zip x2).map { case (xx1, xx2) =>
      val variance = xx2 / x0 - (xx1 * xx1) / n / n
        if (variance < epsilon) 1.0
        else math.sqrt(variance)
    })

    (
      data.map { v =>
        (v - mu) / sigma
      }, mu, sigma
    )
  }


  def addOnesColumn (M: Matrix): Matrix = {
    new Matrix(M.C+1, M.R, Array.fill(M.R)(1.0) ++ M.values)
  }
}
