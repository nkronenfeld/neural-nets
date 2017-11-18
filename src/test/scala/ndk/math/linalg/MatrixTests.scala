package ndk.math.linalg

import org.scalatest.FunSpec

class MatrixTests extends FunSpec {
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
  }
}