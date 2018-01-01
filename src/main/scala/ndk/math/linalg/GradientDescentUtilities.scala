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


  /**
    * Add an initial ones column to a matrix
    *
    * This is used with data matrices to add in intercept column to established data
    *
    * @param M The matrix to alter
    * @return A new matrix with an initial column of all 1's, but otherwise identical to M
    */
  def addOnesColumn (M: Matrix): Matrix = {
    new Matrix(M.C+1, M.R, Array.fill(M.R)(1.0) ++ M.values)
  }


  /**
    * Generic convergence function
    *
    * @param initialConvergenceValue The initial convergence value on which we are trying to improve
    * @param requiredConvergenceValue The required value of the convergence calculation for acceptable
    *                                 convergence
    * @param initialAlpha The initial convergence parameter, input into the function
    * @param initialValue The initial data value
    * @param fcn A function, taking the convergence parameter (alpha) and the value being analyzed,
    *            and outputting the convergence calculation and the new value
    * @tparam T The type of value being analyzed
    * @return The best value of type T found by convergence using the given function
    */
  def converge[T] (initialConvergenceValue: Double,
                   requiredConvergenceValue: Double,
                   initialAlpha: Double,
                   initialValue: T,
                   fcn: (Double, T) => (Double, T)): T  = {
    var c = initialConvergenceValue
    assert(c > 0.0)

    var alpha = initialAlpha
    var v = initialValue
    while (c > requiredConvergenceValue) {
      val (newC, newV) = fcn(alpha, v)
      assert(newC > 0.0)

      if (newC < c) {
        if (newC / c > 0.9) {
          alpha = alpha * 2.0
        }
        c = newC
        v = newV
      } else {
        alpha = alpha / 2.0
      }
    }

    v
  }
}
