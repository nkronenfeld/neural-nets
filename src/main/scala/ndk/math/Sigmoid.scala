package ndk.math


object Sigmoid {
  def apply (z: Double): Double = {
    1.0 / (1 + math.exp(-z))
  }
  def derivative (z: Double): Double = {
    val s = Sigmoid(z)
    s * (1.0 - s)
  }
}