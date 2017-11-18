package ndk.nn

import ndk.math.Sigmoid
import ndk.math.linalg.{Matrix, Vector}
import org.scalatest.FunSpec

class NetworkTests extends FunSpec {
  describe("network initialization") {
    it("should have biases vectors of the correct size") {
      assert(Seq(3, 2) === new Network(4, 3, 2).biases.map(_.length))
      assert(Seq((4, 3), (3, 2)) === new Network(4, 3, 2).weights.map(m => (m.C, m.R)))
    }
  }
  describe("propagation") {
    val network = new Network(3,
      Array(Vector(-1.0, 1.0, -1.0), Vector(2.0, 3.0)),
      Array(
        Matrix.fromVectors(Vector(2.0, 1.0, 0.0), Vector(3.0, 2.0, 1.0), Vector(4.0, 3.0, 2.0), Vector(5.0, 4.0, 3.0)),
        Matrix.fromVectors(Vector(-1.0, 1.0), Vector(1.0, -1.0), Vector(1.0, 2.0))
      )
    )
    //         /                      /             | 1 |          \         \
    //         |                      | | 2 3 4 5 | |   |   | -1 | |         |
    //         | | -1  1 -1 |         | |         | | 1 |   |    | |   | 2 | |
    // sigmoid | |          | sigmoid | | 1 2 3 4 | |   | + |  1 | | + |   | | =
    //         | |  1 -1  1 |         | |         | | 1 |   |    | |   | 3 | |
    //         |                      | | 0 1 2 3 | |   |   | -1 | |         |
    //         \                      \             | 1 |          /         /
    //
    //         /                      | 13 |         \
    //         | | -1  1  1 |         |    |   | 2 | |
    // sigmoid | |          | sigmoid | 11 | + |   | |
    //         | |  1 -1  2 |         |    |   | 3 | |
    //         \                      |  5 |         /
    //
    assert(Vector(
      Sigmoid(-Sigmoid(13) + Sigmoid(11) + Sigmoid(5) + 2),
      Sigmoid(Sigmoid(13) - Sigmoid(11) + 2 * Sigmoid(5) + 3)
    ) === network.feedForward(Vector(1.0, 1.0, 1.0, 1.0)))
  }
}
