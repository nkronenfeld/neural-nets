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

  private def shuffle[T] (input: Seq[T]): Seq[T] = {
    new Random().shuffle(input)
  }
  // Train using stochastic gradient descent
  /**
    * Train the network using stochastic gradient descent
    *
    * @param data The input data, and expected results; the first vector should, of course, be the
    *             size of our input layer, the second, our output layer
    * @param epochs The number of epochs over which to train
    * @param miniBatchSize number of elements of data to take at a time
    * @param learningRate The learning rate (eta in the original) at which to train
    * @param testData Test data to evaluate after each epoch to show our progress
    * @param random A random number generator, passed in so that tests can use a consistent seed
    */
  def train (data: Seq[(Vector, Vector)],
             epochs: Int, miniBatchSize: Int, learningRate: Double,
             testData: Seq[(Vector, Vector)] = Seq(),
             random: Random = new Random()): Unit = {
    val n = data.length
    val nTest = testData.length
    for (j <- 0 until epochs) {
      var epochData = random.shuffle(data)
      while (epochData.length > 0) {
        updateMiniBatch(epochData.take(miniBatchSize), learningRate)
        epochData = epochData.drop(miniBatchSize)
        if (nTest > 0) {
          val (x, y) = evaluate(testData)
          println(s"Epoch ${j}: ${x} / ${y}")
        } else {
          println(s"Epoch ${j} complete.")
        }
      }
    }
  }

  /*
   * Train on a small batch of data
   *
   * Update the network's weights and biases by applying gradient descent using backpropagation
   * in a single batch (essentially full gradient descent, one epoch)
   *
   * @param data The input data, and expected results; the first vector should, of course, be the
   *             size of our input layer, the second, our output layer
   * @param learningRate The learning rate (eta in the original) at which to train
   */
  private def updateMiniBatch (data: Seq[(Vector, Vector)], learningRate: Double): Unit = {
    val gradBiases = biases.map(v => Vector.zero(v.length))
    val gradWeights = weights.map(w => Matrix.zero(w.C, w.R))

    for ((input, output) <- data) {
      val (deltaGradBiases, deltaGradWeights) = backProp(input, output)
      for (i <- gradBiases.indices) {
        gradBiases(i).addInPlace(deltaGradBiases(i))
      }
      for (i <- gradWeights.indices) {
        gradWeights(i).addInPlace(deltaGradWeights(i))
      }
    }

    for (i <- weights.indices) {
      weights(i).addInPlace(gradWeights(i).multiplyInPlace(- learningRate / data.length))
    }
    for (i <- biases.indices) {
      biases(i).addInPlace(gradBiases(i).multiplyInPlace(- learningRate / data.length))
    }
  }

  /*
   * Train for a single datum
   *
   * @param input The input training vector
   * @param output The expected output vector for the given input
   * @return (gradBiases, gradWeights) representing the gradient of the cost function,
   *         parallel with biases and weights
   */
  private def backProp (input: Vector, output: Vector): (Seq[Vector], Seq[Matrix]) = {
    val gradBiases = biases.map(b => Vector.zero(b.length))
    val gradWeights = weights.map(w => Matrix.zero(w.C, w.R))

    // Feed forward
    null
  }

  /*
   * See how well we represent the given data
   */
  private def evaluate (data: Seq[(Vector, Vector)]): (Double, Double) = {
    (0.0, 0.0)
  }
}



//def backprop(self, x, y):
//"""Return a tuple ``(nabla_b, nabla_w)`` representing the
//gradient for the cost function C_x.  ``nabla_b`` and
//``nabla_w`` are layer-by-layer lists of numpy arrays, similar
//to ``self.biases`` and ``self.weights``."""
//nabla_b = [np.zeros(b.shape) for b in self.biases]
//nabla_w = [np.zeros(w.shape) for w in self.weights]
//# feedforward
//activation = x
//activations = [x] # list to store all the activations, layer by layer
//zs = [] # list to store all the z vectors, layer by layer
//for b, w in zip(self.biases, self.weights):
//z = np.dot(w, activation)+b
//zs.append(z)
//activation = sigmoid(z)
//activations.append(activation)
//# backward pass
//delta = self.cost_derivative(activations[-1], y) * \
//sigmoid_prime(zs[-1])
//nabla_b[-1] = delta
//nabla_w[-1] = np.dot(delta, activations[-2].transpose())
//# Note that the variable l in the loop below is used a little
//# differently to the notation in Chapter 2 of the book.  Here,
//# l = 1 means the last layer of neurons, l = 2 is the
//# second-last layer, and so on.  It's a renumbering of the
//# scheme in the book, used here to take advantage of the fact
//# that Python can use negative indices in lists.
//for l in xrange(2, self.num_layers):
//z = zs[-l]
//sp = sigmoid_prime(z)
//delta = np.dot(self.weights[-l+1].transpose(), delta) * sp
//nabla_b[-l] = delta
//nabla_w[-l] = np.dot(delta, activations[-l-1].transpose())
//return (nabla_b, nabla_w)
//
//def evaluate(self, test_data):
//"""Return the number of test inputs for which the neural
//network outputs the correct result. Note that the neural
//network's output is assumed to be the index of whichever
//neuron in the final layer has the highest activation."""
//test_results = [(np.argmax(self.feedforward(x)), y)
//for (x, y) in test_data]
//return sum(int(x == y) for (x, y) in test_results)
//
//def cost_derivative(self, output_activations, y):
//"""Return the vector of partial derivatives \partial C_x /
//\partial a for the output activations."""
//return (output_activations-y)