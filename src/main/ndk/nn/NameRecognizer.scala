package software.uncharted.focalpoint.networksources.names

import scala.util.Random


object Sigmoid {
  def apply (z: Double): Double = {
    1.0 / (1 + math.exp(-z))
  }
  def derivative (z: Double): Double = {
    val s = Sigmoid(z)
    s * (1.0 - s)
  }
}

object Gaussian {
  private val r = new Random
  def random: Double = {
    math.sqrt(-2 * math.log(r.nextDouble())) * math.cos(2 * math.Pi * r.nextDouble())
  }
}

object NameRecognizer {
  private[names] def fixedLengthSequence[T] (input: Seq[T], length: Int, defaultValue: T): Seq[T] = {
    if (input.length < length) {
      input ++ Seq.fill(length - input.length)(defaultValue)
    } else if (input.length == length) {
      input
    } else {
      input.take(length)
    }
  }
  def stringToFeatures (words: Int, lettersPerWord: Int = 20)(input: String): FeatureVector = {
    new FeatureVector(fixedLengthSequence(input.split("\\s+"), words, "").map { word =>
      (word.length.toDouble / lettersPerWord) +: fixedLengthSequence(word.map(_.toDouble / 256.0), lettersPerWord, 0.0)
    }.flatten.toArray)
  }
}

class NameRecognizer {
}

class Network (layerSizes: Int*) {
  val layers = layerSizes.length
  val biases = layerSizes.drop(1).map(n => Array.fill(n)(Gaussian.random))
  val weights = layerSizes.sliding(2).map { neighbors =>
    val lowerSize = neighbors(0)
    val upperSize = neighbors(1)
    Array.fill(lowerSize * upperSize)(Gaussian.random)
  }.toSeq
}

class FeatureVector (val values: Array[Double]) {
  val length = values.length;

  def apply(n: Int): Double = {
    values(n)
  }

  def update(n: Int, v: Double): Unit = {
    values(n) = v
  }

  def +(that: FeatureVector): FeatureVector = {
    assert(this.values.length == that.values.length)
    new FeatureVector((this.values zip that.values).map { case (a, b) => a + b })
  }

  def -(that: FeatureVector): FeatureVector = {
    assert(this.values.length == that.values.length)
    new FeatureVector((this.values zip that.values).map { case (a, b) => a - b })
  }

  def * (that: FeatureVector): FeatureVector = {
    assert(this.values.length == that.values.length)
    new FeatureVector((this.values zip that.values).map { case (a, b) => a * b })
  }

  def dot (that: FeatureVector): Double = {
    assert(this.values.length == that.values.length)
    (this.values zip that.values).map { case (a, b) => a * b }.sum
  }

  def *(that: Double): FeatureVector = {
    new FeatureVector(this.values.map(_ * that))
  }

  override def equals (other: Any): Boolean = other match {
    case that: FeatureVector => FeatureVector.vectorsEqual(1E-12)(this, that)
    case _ => false
  }
  override def toString: String = values.mkString("[", ", ", "]")
}
object FeatureVector {
  def apply (values: Double*): FeatureVector = new FeatureVector(values.toArray)
  def vectorsEqual (tolerance: Double)(a: FeatureVector, b: FeatureVector): Boolean = {
    val c = a - b

    (c dot c) < (tolerance * tolerance)
  }
}


// A matrix, a listing of C columns of length R (i.e., column-major order
class FeatureMatrix (val C: Int, val R: Int, val values: Array[Double]) {
  assert(R * C == values.length)
  def apply (c: Int, r: Int): Double = {
    assert(0 <= c && c < C)
    assert(0 <= r && r < R)
    values(c * R + r)
  }

  def update(c: Int, r: Int, v: Double): Unit = {
    assert(0 <= c && c < C)
    assert(0 <= r && r < R)
    values(c * R + r) = v
  }

  def * (that: FeatureMatrix): FeatureMatrix = {
    assert(this.C == that.R)
    new FeatureMatrix(that.C, this.R,
      (for (r <- 0 until this.R; c <- 0 until that.C) yield {
        (0 until this.C).map(n => this(n, r) * that(c, n)).sum
      }).toArray)
  }
  def * (that: FeatureVector): FeatureVector = {
    assert(this.C == that.length)
    new FeatureVector((for (r <- 0 until this.R) yield {
      (0 until this.C).map(n => this(n, r) * that(n)).sum
    }).toArray)
  }

  override def equals (other: Any): Boolean = other match {
    case that: FeatureMatrix =>
      this.C == that.C &&
      this.R == that.R &&
        (0 until (C * R)).map(n => this.values(n) == that.values(n)).reduce(_ && _)
    case _ => false
  }
}
object FeatureMatrix {
  def apply (columns: Array[Double]*): FeatureMatrix = {
    val C = columns.length
    val R = columns(0).length
    columns.foreach(c => assert(c.length == R))

    new FeatureMatrix(C, R, columns.reduce(_ ++ _))
  }
}

object StatisticalGradiendDescent {
  def apply (data: Seq[(FeatureVector, FeatureVector)], epochs: Int, batches: Int, mu: Double): Unit = {
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