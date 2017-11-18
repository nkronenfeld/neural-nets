package ndk.nn

import ndk.math.{Gaussian, Sigmoid}
import ndk.math.linalg.{Matrix, Vector}

import scala.util.Random

class Network (val layers: Int,
               val biases: Array[Vector],
               val weights: Array[Matrix]) {
  assert(biases.length == layers - 1)
  assert(weights.length == layers - 1)
  for (i <- 0 until layers - 1) {
    assert(weights(i).R == biases(i).length)
    if (i > 0) assert(weights(i).C == biases(i - 1).length)
  }

  def this(random: Random)(layerSizes: Int*) = this(
    layerSizes.length,
    layerSizes.drop(1).map(n => Vector.random(n)).toArray,
    layerSizes.sliding(2).map { neighbors =>
      Matrix.random(neighbors(0), neighbors(1))
    }.toArray
  )
  def this(layerSizes: Int*) = this(new Random)(layerSizes:_*)

  def feedForward (a: Vector): Vector = {
    (biases zip weights).foldLeft(a) { case (input, (bias, weight)) =>
      val a = weight * input
      val b = a + bias
      val c = b.map(Sigmoid.apply)
      (weight * input + bias).map(Sigmoid.apply)
    }
  }
}
