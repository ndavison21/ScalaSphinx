package sphinx.test.dissertation

import sphinx.clientAndServer.Client
import sphinx.params.Params
import sphinx.clientAndServer.SphinxServer

object MixHeaderTimeEcc {
  def main(args: Array[String]) {
    Framework.writeToFile("output/mixHeaderTimeEcc.csv", "r,avg. time, variance\n")
    for (r <- 1 to 10) {
      
      var mean = 0D
      var variance = 0D
      
      for(i <- 1 to 10000) {
        val time = runTest(r)
        val prev_mean = mean
        mean = prev_mean + (time - prev_mean)/i
        variance = ((i-1)*variance + (time - prev_mean)*(time - mean))/i
      }
      
      val samplevar = variance

      println("r = " + r + " avg. time = " + mean + " variance = " + samplevar)
      Framework.appendToFile("output/mixHeaderTimeEcc.csv", r + "," + mean + "," + samplevar)
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