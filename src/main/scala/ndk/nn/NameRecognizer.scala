package ndk.nn



import ndk.math.linalg.Vector




object NameRecognizer {
  private[nn] def fixedLengthSequence[T] (input: Seq[T], length: Int, defaultValue: T): Seq[T] = {
    if (input.length < length) {
      input ++ Seq.fill(length - input.length)(defaultValue)
    } else if (input.length == length) {
      input
    } else {
      input.take(length)
    }
  }
  def stringToFeatures (words: Int, lettersPerWord: Int = 20)(input: String): Vector = {
    new Vector(fixedLengthSequence(input.split("\\s+"), words, "").map { word =>
      (word.length.toDouble / lettersPerWord) +: fixedLengthSequence(word.map(_.toDouble / 256.0), lettersPerWord, 0.0)
    }.flatten.toArray)
  }
}

class NameRecognizer {
}





