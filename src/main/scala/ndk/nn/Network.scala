package ndk.nn

import ndk.math.Gaussian

class Network (layerSizes: Int*) {
  val layers = layerSizes.length
  val biases = layerSizes.drop(1).map(n => Array.fill(n)(Gaussian.random))
  val weights = layerSizes.sliding(2).map { neighbors =>
    val lowerSize = neighbors(0)
    val upperSize = neighbors(1)
    Array.fill(lowerSize * upperSize)(Gaussian.random)
  }.toSeq
}
