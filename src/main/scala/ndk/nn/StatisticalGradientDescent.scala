package ndk.nn



import scala.util.Random

import ndk.math.linalg.Vector


// Taken from http://neuralnetworksanddeeplearning.com/
object StatisticalGradiendDescent {
  def apply (data: Seq[(Vector, Vector)], epochs: Int, batches: Int, mu: Double): Unit = {
    val r = new Random()
    val n = data.length
    val batchSize = n.toDouble / batches
    for (epoch <- 1 to epochs) {
      val epochData = r.shuffle(data)
      var batchStart = 0.0
      for (batch <- 1 to batches) {
        val batchEnd = batchStart + batchSize
        val batchData = epochData.slice(batchStart.ceil.toInt, batchEnd.ceil.toInt)
        //        updateBatch (batchData, mu)
      }
    }
  }
}