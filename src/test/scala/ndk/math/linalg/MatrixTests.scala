package ndk.math.linalg

import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.FunSpec

class MatrixTests extends FunSpec {
  implicit val matricesEqual = new TolerantMatrixEquality(1E-12)

  describe("Multiplication") {
    it("should multiply a vector properly") {
      val A = Matrix.fromArrays(Array(1.0, 4.0, 2.0), Array(-3.0, 5.0, -4.0))
      val V = Vector(7, 9)
      assert(A * V === Vector(-20, 73, -22))
    }
    it("should multiply two matrices properly") {
      val A = Matrix.fromArrays(Array(1.0, 4.0), Array(2.0, 3.0))
      val B = Matrix.fromArrays(Array(2.0, -1.0), Array(-1.0, 1.0))
      assert(A * B === Matrix.fromArrays(Array(0.0, 1.0), Array(5.0, -1.0)))
    }
    it("should multiply by a vector properly") {
      val m = Matrix.fromVectors(Vector(-1.0, 1.0), Vector(1.0, -1.0), Vector(-1.0, 1.0))
      val v1 = Vector(1.0, 1.0, 1.0)
      val v2 = m * v1
      assert(Vector(-1.0, 1.0) ===  v2)
    }
  }
  describe("Transpose") {
    it("should transpose a matrix properly") {
      val a = Matrix.fromVectors(Vector(1.0, 2.0), Vector(3.0, 4.0), Vector(5.0, 6.0))
      val b = Matrix.fromVectors(Vector(1.0, 3.0, 5.0), Vector(2.0, 4.0, 6.0))

      assert(a === b.transpose())
      assert(b === a.transpose())
    }
  }
  describe("Standard matrices") {
    it("should generate a proper 2x2 zero matrix") {
      val M = Matrix.zero(2, 2)
      assert(0.0 === M(0, 0))
      assert(0.0 === M(0, 1))
      assert(0.0 === M(1, 0))
      assert(0.0 === M(1, 1))
    }
    it("should generate a proper 2x2 unit matrix") {
      val M = Matrix.one(2)
      assert(1.0 === M(0, 0))
      assert(0.0 === M(0, 1))
      assert(0.0 === M(1, 0))
      assert(1.0 === M(1, 1))
    }
    it("should generate a proper 3x3 unit matrix") {
      val M = Matrix.one(3)
      for (i <- 0 until 3; j <- 0 until 3) {
        if (i == j) assert(1.0 === M(i, j))
        else assert(0.0 === M(i, j))
      }
    }
    it("should generate a proper 4x4 unit matrix") {
      val M = Matrix.one(4)
      for (i <- 0 until 4; j <- 0 until 4) {
        if (i == j) assert(1.0 === M(i, j))
        else assert(0.0 === M(i, j))
      }
    }
  }
  describe("Inversion") {
    it("should invert a simple 2x2 matrix") {
      val M = Matrix.fromArrays(Array(1.0, 3.0), Array(-2.0, 2.5))
      val Mi = Matrix.invertSquareMatrix(M)
      assert(Matrix.one(2) === M * Mi)
      assert(Matrix.one(2) === Mi * M)
    }
    it("should throw an exception when trying to invert a non-square matrix") {
      val M = Matrix.fromArrays(Array(1.0, 2.0, 3.0), Array(1.0, 3.0, 7.0))
      intercept[AssertionError](Matrix.invertSquareMatrix(M))
    }
    it("should throw an exception when trying to invert an uninvertable matrix") {
      val M = Matrix.fromArrays(Array(1.0, 2.0), Array(1.0, 2.0))
      val thrown = intercept[Exception](Matrix.invertSquareMatrix(M))
      assert("Uninvertable matrix" === thrown.getMessage)
    }
  }
}

class TolerantMatrixEquality (epsilon: Double) extends Equality[Matrix] {
  override def areEqual(a: Matrix, b: Any) = a.approximatelyEquals(epsilon)(b)
}