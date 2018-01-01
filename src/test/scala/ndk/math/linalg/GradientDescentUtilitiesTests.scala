package ndk.math.linalg

import org.scalatest.FunSpec

class GradientDescentUtilitiesTests extends FunSpec {
  describe("Addition of intercept column") {
    it("should add a first column of all 1's to any matrix") {
      val M = GradientDescentUtilities.addOnesColumn(Matrix.fromArrays(
        Array(2.0, 3.0, 4.0),
        Array(3.0, 2.0, 5.0),
        Array(2.5, 2.5, -4.0)
      ))
      assert(Vector(1.0, 1.0, 1.0) === M.column(0))
      assert(Vector(2.0, 3.0, 4.0) === M.column(1))
      assert(Vector(3.0, 2.0, 5.0) === M.column(2))
      assert(Vector(2.5, 2.5, -4.0) === M.column(3))
    }
  }

  describe("Data scaling by standard deviation") {
    it("should throw an error when given data of non-uniform size") {
      intercept[AssertionError](GradientDescentUtilities.scaleByStandardDeviation(Seq(
        Vector(0.0, 1.0),
        Vector(0.0, 1.0, 2.0)
      )))
    }
    it("should find the correct values, when given simple, good data") {
      val (norms, mu, sigma) = GradientDescentUtilities.scaleByStandardDeviation(Seq(
        Vector( 9.0, 12.0, 20.0),
        Vector(15.0, 15.0, 20.0),
        Vector(21.0,  9.0, 50.0)
      ))
      assert(mu === Vector(15.0, 12.0, 30.0))
      val r24 = math.sqrt(24.0)
      val r6 = math.sqrt(6.0)
      val r200 = math.sqrt(200.0)
      assert(sigma === Vector(r24, r6, r200))
      assert(norms(0) === Vector(-6.0 / r24, 0.0, -10.0 / r200))
      assert(norms(1) === Vector(0.0, 3.0 / r6, -10.0 / r200))
      assert(norms(2) === Vector(6.0 / r24, -3.0 / r6, 20.0 / r200))
    }
    it("should not throw an error when all values of a given entry are the same") {
      GradientDescentUtilities.scaleByStandardDeviation(Seq(
        Vector(1.0, 10.0),
        Vector(2.0, 10.0)
      ))
    }
    it("should produce a sequence when given a sequence") {
      val (norms, _, _): (Seq[Vector], Vector, Vector) =
        GradientDescentUtilities.scaleByStandardDeviation(Seq(
          Vector(9.0, 12.0, 20.0),
          Vector(15.0, 15.0, 20.0),
          Vector(21.0, 9.0, 50.0)
        ))
      assert(norms.isInstanceOf[Seq[Vector]])
    }
    it("should produce a set when given a set") {
      val (norms, _, _): (Set[Vector], Vector, Vector) =
        GradientDescentUtilities.scaleByStandardDeviation(Set(
          Vector(9.0, 12.0, 20.0),
          Vector(15.0, 15.0, 20.0),
          Vector(21.0, 9.0, 50.0)
        ))
      assert(norms.isInstanceOf[Set[Vector]])
    }
  }

}
