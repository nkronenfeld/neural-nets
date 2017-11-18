package ndk.math



import scala.util.Random




object Gaussian {
  def random (random: Random = new Random): Double = {
    math.sqrt(-2 * math.log(random.nextDouble())) * math.cos(2 * math.Pi * random.nextDouble())
  }
}