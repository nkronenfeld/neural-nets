package ndk.nn

import org.scalactic.TolerantNumerics
import org.scalatest.FunSpec



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
