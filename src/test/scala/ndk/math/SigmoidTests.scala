package ndk.math

import org.scalactic.TolerantNumerics
import org.scalatest.FunSpec

class SigmoidTests extends FunSpec {
  private val epsilon = 1E-12
  implicit val doubleEquality = TolerantNumerics.tolerantDoubleEquality(epsilon)

  describe("Sigmoid") {
    it("should be 0 at large negative numbers") {
      assert(Sigmoid(-100.0) === 0.0)
    }
    it("should be 1/2 at 0") {
      assert(Sigmoid(0.0) === 0.5)
    }
    it("should be 1 at large positive numbers") {
      assert(Sigmoid(100.0) === 1.0)
    }
  }
}
