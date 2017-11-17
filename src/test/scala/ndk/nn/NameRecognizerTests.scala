package software.uncharted.focalpoint.networksources.names

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

class NameRecognizerTests extends FunSpec {
  describe("Fixed-length sequences") {
    it("should have the right length if given a shorter sequence") {
      assert(NameRecognizer.fixedLengthSequence(Seq("abc", "def"), 3, "ghi") === Seq("abc", "def", "ghi"))
    }
    it("should have the right length if given an equal-length sequence") {
      assert(NameRecognizer.fixedLengthSequence(Seq("abc", "def", "ghi"), 3, "jkl") === Seq("abc", "def", "ghi"))
    }
    it("should have the right length if given a longer sequence") {
      assert(NameRecognizer.fixedLengthSequence(Seq("abc", "def", "ghi", "jkl", "mno"), 3, "pqr") === Seq("abc", "def", "ghi"))
    }
  }

  describe("Featurization") {
    describe("with two words") {
      it("should always have 40 features") {
        assert(42 === NameRecognizer.stringToFeatures(2)("").values.length)
        assert(42 === NameRecognizer.stringToFeatures(2)("well enough").values.length)
        assert(42 === NameRecognizer.stringToFeatures(2)("this is a cheap trick").values.length)
      }
    }
    describe("with three words") {
      it("should always have 60 features") {
        assert(63 === NameRecognizer.stringToFeatures(3)("").values.length)
        assert(63 === NameRecognizer.stringToFeatures(3)("ronald wilson reagan").values.length)
        assert(63 === NameRecognizer.stringToFeatures(3)("the quick brown fox jumps over the lazy dog").values.length)
      }
    }
  }
}

class MatrixTests extends FunSpec {
  describe("Multiplication") {
    it("should multiply a vector properly") {
      val A = FeatureMatrix(Array(1.0, 4.0, 2.0), Array(-3.0, 5.0, -4.0))
      val V = FeatureVector(7, 9)
      assert(A * V === FeatureVector(-20, 73, -22))
    }
    it("should multiply two matrices properly") {
      val A = FeatureMatrix(Array(1.0, 4.0), Array(2.0, 3.0))
      val B = FeatureMatrix(Array(2.0, -1.0), Array(-1.0, 1.0))
      assert(A * B === FeatureMatrix(Array(0.0, 1.0), Array(5.0, -1.0)))
    }
  }
}