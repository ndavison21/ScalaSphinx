package sphinx.test.dissertation

import sphinx.clientAndServer.Client
import sphinx.clientAndServer.SphinxServer
import sphinx.params.Params

object MixHeaderTimeGroup {
  def main(args: Array[String]) {
    Framework.writeToFile("output/mixHeaderTimeGroup.csv", "r,avg. time, variance\n")
    for (r <- 1 to 10) {

      var mean = 0L
      var variance = 0L

      for (i <- 1 to 1000) {
        val time = runTest(r)
        val prev_mean = mean
        mean = mean + (time - mean) / i
        variance = variance + (time - prev_mean) * (time - mean)
      }

      val samplevar: Float = variance / 999

      println("r = " + r + " avg. time = " + mean + " variance = " + samplevar)
      Framework.appendToFile("output/mixHeaderTimeGroup.csv", r + "," + mean + "," + samplevar)
    }
  }

  def runTest(r: Int): Long = {
    Framework.cleanup
    val p = new Params(r, true)

    for (i <- 0 to r * 2) {
      new SphinxServer(p)
    }

    val client = new Client(p)

    val id = Array.fill[Byte](Params.k)(0.asInstanceOf[Byte])
    val nodeIDs = Params.randomSubset(Params.pki.keySet.toArray, r)

    val start = System.nanoTime()
    val ((a, b, c), delta) = Client.createMixHeader(Params.dSpecial, id, nodeIDs.map { x => Params.stringOfHexToByteArray(x) }, p)
    val end = System.nanoTime()

    return end - start
  }
}