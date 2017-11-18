package ndk.math



import scala.util.Random




object Gaussian {
  private val r = new Random
  def random: Double = {
    math.sqrt(-2 * math.log(r.nextDouble())) * math.cos(2 * math.Pi * r.nextDouble())
  }
}